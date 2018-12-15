// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import java.util.Dictionary

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import scalaz._
import Scalaz._
import net.degoes.applications.exercises.{Crawl, URL, extractURLs, getURL}

object exercises extends App {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  //def bind[A, B](fa : F[A])(f: A => F[B]): F[B]
  def crawlIO[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
//      def reduce[A: Monoid](l: List[A]): A =
//        l.foldLeft(mzero[A])(_ |+| _)
    def loop(seeds: Set[URL], visited: Set[URL]): IO[Nothing, Crawl[E,A]] =
      IO.traverse(seeds)(url =>
        getURL(url).redeem(
          _    => IO.now(mzero[Crawl[E, A]]),
          html =>
            for {
              crawl1 <- processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
              urls <- IO.now(extractURLs(url, html).toSet.flatMap(router))
              //crawl2 <- crawlIO(urls.toSet.flatMap(router), router, processor)
              crawl2 <- loop(urls -- visited, visited ++ urls)

            } yield crawl1 |+| crawl2
        )).map(_.foldMap()) : IO[Nothing, Crawl[E, A]]

    loop(seeds, seeds)
      //canBuildFrom went away in scala 2.13. So ZIO doesn't use that.
  }
  // the web pages we crawl may have URLs of their own. How to handle that?

  // this implementation is tail-recursive, which means it uses constant-heap, but the earlier impl is not
  def crawlIOAliter[E: Monoid, A: Monoid](
                                     seeds     : Set[URL],
                                     router    : URL => Set[URL],
                                     processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Unit] =
      IO.traverse(seeds)(url =>
        getURL(url).redeem(
          _    => IO.unit,
          html =>
            processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
              val urls = extractURLs(url, html).toSet.flatMap(router)

              ref.modify {
                case (crawl0, visited) =>
                  (visited, (crawl0 |+| crawl1, visited ++ urls))
              }.flatMap(visited =>
                loop(urls -- visited, ref)
              )
            }
        )).void : IO[Nothing, Unit]

    for {
      ref   <- Ref(mzero[Crawl[E, A]] -> seeds)
      _     <- loop(seeds, ref)
      tuple <- ref.get
    } yield tuple._1
  }

  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  //`ref` provides Atomic guarantees. So it can be used for consistent atomic updates across multiple fibers
  // semaphore from java blocks while that from ZIO doesnt block
  // use par is for doing fixed number of things in parallel
  // use parAll for doing large number of things in parallel
  // if you're using parTraverse in a recursive function, you'll get into trouble
  // at some point you'll hit limitations of the thread pool if you keep increasing parallelism of parAll.
  def crawlIOPar[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Unit] =
      IO.parTraverse(seeds)(url =>
        getURL(url).redeem(
          _    => IO.unit,
          html =>
            processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
              val urls = extractURLs(url, html).toSet.flatMap(router)

              //modify op will be atomic.
              ref.modify {
                case (crawl0, visited) =>
                  (visited, (crawl0 |+| crawl1, visited ++ urls))
              }.flatMap(old =>
                loop(urls -- old, ref) // there's a very subtle race condition here. Which means, we will traverse the same URL more than once
              )
            }
        )).void : IO[Nothing, Unit]

    for {
      ref   <- Ref(mzero[Crawl[E, A]] -> seeds)
      _     <- loop(seeds, ref)
      tuple <- ref.get
    } yield tuple._1
  }

  // this is queue version with polling
  def crawlIOParQueueVersion[E: Monoid, A: Monoid](
                                        seeds     : Set[URL],
                                        router    : URL => Set[URL],
                                        processor : (URL, String) => IO[E, A]): IO[Nothing, (Fiber[Nothing, Unit], Ref[(Crawl[E, A], Set[URL])])] =  {

    def start(n: Int, queue: Queue[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Fiber[Nothing, Unit]] =
      IO.forkAll(List.fill(n)(queue.take.flatMap(url =>
        getURL(url).redeem(
          _    => IO.unit,
          html => processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
            val urls = extractURLs(url, html).toSet.flatMap(router)

            ref.modify {
              case (crawl0, visited) =>
                (visited, (crawl0 |+| crawl1, visited ++ urls))
            }.flatMap(old => IO.traverse(urls -- old)(queue.offer(_)).void)
          }
        )
      ).forever)).map(_.map(_ => ()))

    for {
      ref   <- Ref(mzero[Crawl[E, A]] -> seeds)
      queue <- Queue.bounded[URL](1000)
      _     <- IO.sequence(seeds.toList.map(queue.offer))
      fiber <- start(10, queue, ref)
    } yield (fiber, ref)
  }
  //
  // EXERCISE 3
  //
  // Implement a version of the `crawlIOPar` that can be tested without having
  // to interact with the real world.
  //
  def crawlIO2[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A],
    getURL    : URL => IO[Exception, String] = getURL(_)): IO[Nothing, Crawl[E, A]] = {

    def start(n: Int, queue: Queue[URL], ref: Ref[(Crawl[E, A], Set[URL])]): IO[Nothing, Fiber[Nothing, Unit]] =
      IO.forkAll(List.fill(n)(queue.take.flatMap(url =>
        getURL(url).redeem(
          _    => IO.unit,
          html => processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _)).flatMap { crawl1 =>
            val urls = extractURLs(url, html).toSet.flatMap(router)

            ref.modify {
              case (crawl0, visited) =>
                (visited, (crawl0 |+| crawl1, visited ++ urls))
            }.flatMap(old => IO.traverse(urls -- old)(queue.offer(_)).void)
          }
        )
      ).forever)).map(_.map(_ => ()))

    for {
      ref   <- Ref(mzero[Crawl[E, A]] -> seeds)
      queue <- Queue.bounded[URL](1000)
      _     <- IO.sequence(seeds.toList.map(queue.offer))
      fiber <- start(10, queue, ref)
    } yield (fiber, ref)
  }

  //
  // EXERCISE 4
  //
  // Create a type class to describe `printLine` and `readLine`.
  //
  trait Console[F[_]] {
    def printLine(line: String): F[Unit]
    def readLine: F[String]
  }
  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  //
  // EXERCISE 5
  //
  // Implement helper methods called `printLine` and `readLine` that work with
  // any `F[_]` that supports the `Console` effect.
  //
  def printLine[F[_]: Console](line: String): F[Unit] =
    Console[F].printLine(line)
  def readLine[F[_]: Console]: F[String] =
    Console[F].readLine

  //
  // EXERCISE 6
  //
  // Create an instance of the `Console` type class for `IO[E, ?]` for any `E`
  // by using `IO.sync` and the Scala functions `println` and
  // `scala.io.StdIn.readLine`.
  //
  implicit def ConsoleIO[E]: Console[IO[E, ?]] =
    new Console[IO[E, ?]] {
      def printLine(line: String): IO[E, Unit] =
        IO.sync(println(line))
      def readLine: IO[E, String] =
        IO.sync(scala.io.StdIn.readLine())
    }
  readLine[IO[Throwable, ?]]
  printLine[IO[Throwable, ?]]("hello!")
  printLine[IO[Exception, ?]]("hello!")

  //
  // EXERCISE 7
  //
  // Create an instance of the `Random` type class for `IO[E, ?]` for any `E`
  // by using `IO.sync` and `scala.util.Random.nextInt`.
  //
  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }
  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)
  implicit def RandomIO[E]: Random[IO[E, ?]] =
    new Random[IO[E, ?]] {
      def nextInt(max: Int): IO[E, Int] =
        IO.sync(scala.util.Random.nextInt(max))
    }
  nextInt[IO[Throwable, ?]](420)

  //
  // EXERCISE 8
  //
  // Create a hangman game that is polymorphic in the effect type `F[_]`,
  // requiring only the capability to perform `Console` and `Random` effects.
  //
  def myGame[F[_]: Console: Random: Monad]: F[Unit] =
    for {
      name <- getName[F]
      word <- chooseWord[F]
      state <- State(name, Set(), word).point[F]
      _ <- renderState[F](state)
      _ <- gameLoop[F](state)
    } yield()

  case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).size == 0
  }

  /* `with` filter is not type safe*/
  def gameLoopBuggy[F[_]: Console: Monad](state: State): F[State] =
    for {
      guess <- getChoice[F]
      state <- state.copy(guesses = state.guesses + guess).point[F]
      state <-if (state.playerLost)
                printLine[F]("Sorry, " + state.name + ", you lost!") *> state.point[F]
              else if (state.playerWon)
                printLine("Congratulations, " + state.name + "you won!!!") *> state.point[F]

              else {
                val guessedRight = state.word.contains(guess)
                printLine[F](
                  if (guessedRight) "Good job, keep going"
                  else "No luck, but you still have a chance")*> gameLoop[F](state)
               }
    } yield state

  // clean version
  def gameLoop[F[_]: Console: Monad](state0: State): F[State] =
    for {
      guess  <- getChoice[F]
      state  <- state0.copy(guesses = state0.guesses + guess).point[F]
      _      <- renderState[F](state)
      state  <- if (state.playerLost)
        printLine[F]("Sorry, " + state.name + ", you lost!") *> state.point[F]
      else if (state.playerWon)
        printLine[F]("Congratulations, " + state.name + ", you won!!!") *> state.point[F]
      else printLine[F](if (state0.guesses.contains(guess))
        s"Stop being silly, ${state.name}!"
      else {
        val guessedRight = state.word.contains(guess)

        if (guessedRight) s"Good job, ${state.name}, keep going!"
        else s"No luck, ${state.name}, but you still have a chance!"
      }) *> gameLoop[F](state)
    } yield state

  trait Effect[F[_, _]] {
    def monad[E]: Monad[F[E, ?]]
  }

  def renderState[F[_]: Console](state: State): F[Unit] = {
    //
    //  f     n  c  t  o
    //  -  -  -  -  -  -  -
    //
    //  Guesses: a, z, y, x
    //
    val word =
      state.word.toList.map(c =>
        if (state.guesses.contains(c)) s" $c " else "   ").mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    printLine[F](text)
  }

  def getChoice[F[_]: Console: Monad]: F[Char] =
    for {
      _ <- printLine[F]("Please guess a letter:")
      line <- readLine[F]
      letter <- line.trim.toLowerCase.toList match {
        case letter:: Nil => Monad[F].point(letter)
        case _ => printLine[F]("You did not enter a letter") *> getChoice[F]
      }
    } yield letter

  def getName[F[_]: Console: Apply]: F[String] =
    printLine[F]("Please enter your name:") *> readLine[F]

  trait Error[F[_]] {
    def fail(message: String): F[Unit]
  }

  def chooseWord[F[_]: Random: Functor]: F[String] =
    nextInt[F](Dictionary.length).map(index =>
      Dictionary.lift(index).getOrElse("buggy")
    )

  val Dictionary = List("aaron", "abelian", "ability", "about", "abstract", "abstract", "abstraction", "accurately", "adamek", "add", "adjacent", "adjoint", "adjunction", "adjunctions", "after", "after", "again", "ahrens", "albeit", "algebra", "algebra", "algebraic", "all", "all", "allegories", "almost", "already", "also", "american", "among", "amount", "ams", "an", "an", "analysis", "analytic", "and", "and", "andre", "any", "anyone", "apart", "apologetic", "appears", "applicability", "applications", "applications", "applied", "apply", "applying", "applying", "approach", "archetypical", "archetypical", "are", "areas", "argument", "arising", "aristotle", "arrowsmorphism", "article", "arxiv13026946", "arxiv13030584", "as", "as", "aspect", "assumed", "at", "attempts", "audience", "august", "awodey", "axiom", "axiomatic", "axiomatized", "axioms", "back", "barr", "barry", "basic", "basic", "be", "beginners", "beginning", "behind", "being", "benedikt", "benjamin", "best", "better", "between", "bicategories", "binary", "bodo", "book", "borceux", "both", "both", "bourbaki", "bowdoin", "brash", "brendan", "build", "built", "but", "but", "by", "called", "cambridge", "can", "cardinal", "carlos", "carnap", "case", "cases", "categorial", "categorical", "categorical", "categories", "categories", "categorification", "categorize", "category", "category", "cats", "catsters", "central", "certain", "changes", "charles", "cheng", "chicago", "chiefly", "chopin", "chris", "cite", "clash", "classes", "classical", "closed", "coend", "coin", "colimit", "colin", "collection", "collections", "comparing", "completion", "composed", "composition", "computational", "computer", "computing", "concept", "concepts", "concepts", "conceptual", "concrete", "confronted", "consideration", "considers", "consistently", "construction", "constructions", "content", "contents", "context", "context", "contexts", "continues", "continuous", "contrast", "contributed", "contributions", "cooper", "correctness", "costas", "count", "course", "cover", "covering", "current", "currently", "david", "decategorification", "deducing", "define", "defined", "defining", "definition", "definitions", "der", "derives", "described", "describing", "description", "descriptions", "detailed", "development", "dictum", "did", "different", "dimensions", "directed", "discovered", "discovery", "discuss", "discussed", "discussion", "discussion", "disparage", "disservice", "do", "does", "driving", "drossos", "duality", "dvi", "each", "easy", "ed", "edges", "edit", "edition", "eilenberg", "eilenbergmaclane", "elementary", "elementary", "elements", "elementwise", "elephant", "ellis", "else", "embedding", "embodiment", "embryonic", "emily", "end", "enthusiastic", "equations", "equivalence", "equivalences", "equivalences", "etc", "etcs", "eugenia", "even", "eventually", "everything", "evident", "example", "examples", "examples", "except", "excused", "exist", "exists", "exposure", "expressed", "expressiveness", "extension", "extra", "f", "fact", "fair", "families", "far", "feeds", "feeling", "finds", "finite", "first", "flourished", "focuses", "folklore", "follows", "fong", "for", "for", "force", "forced", "foremost", "form", "formalizes", "formulated", "forthcoming", "found", "foundation", "foundations", "foundations", "francis", "free", "freyd", "freydmitchell", "from", "functions", "functor", "functor", "functors", "fundamental", "further", "gabrielulmer", "general", "general", "generalized", "generalizes", "geometry", "geometry", "george", "geroch", "get", "gift", "give", "given", "going", "goldblatt", "grandis", "graph", "gray", "grothendieck", "ground", "group", "groupoid", "grp", "guide", "göttingen", "had", "handbook", "handful", "handle", "harper", "has", "have", "he", "here", "here", "herrlich", "higher", "higher", "higherdimensional", "highlevel", "hilberts", "his", "historical", "historically", "history", "history", "holistic", "holland", "home", "homomorphisms", "homotopy", "homotopy", "horizontal", "horst", "however", "i", "idea", "ideas", "ieke", "if", "if", "illustrated", "important", "in", "in", "inaccessible", "inadmissible", "include", "includes", "including", "indeed", "indexes", "infinite", "informal", "initial", "innocent", "instance", "instead", "instiki", "interacting", "internal", "intersection", "into", "introduce", "introduced", "introduces", "introducing", "introduction", "introduction", "introductory", "intuitions", "invitation", "is", "isbell", "isbn", "isomorphisms", "it", "it", "its", "itself", "ive", "j", "jaap", "jacob", "jiri", "johnstone", "joy", "jstor", "just", "kan", "kant", "kapulkin", "kashiwara", "kind", "kinds", "kleins", "kmorphisms", "ktransfors", "kℕ", "la", "lagatta", "lane", "language", "large", "last", "later", "later", "latest", "lauda", "lawvere", "lawveres", "lead", "leads", "least", "lectures", "led", "leinster", "lemma", "lemmas", "level", "library", "lifting", "likewise", "limit", "limits", "link", "linked", "links", "list", "literally", "logic", "logic", "logically", "logische", "long", "lurie", "mac", "maclane", "made", "major", "make", "manifest", "many", "many", "mappings", "maps", "marco", "masaki", "material", "mathct0305049", "mathematical", "mathematical", "mathematician", "mathematician", "mathematics", "mathematics", "mathematicsbrit", "may", "mclarty", "mclartythe", "means", "meet", "membership", "methods", "michael", "misleading", "mitchell", "models", "models", "moerdijk", "monad", "monadicity", "monographs", "monoid", "more", "morphisms", "most", "mostly", "motivation", "motivations", "much", "much", "music", "must", "myriads", "named", "natural", "natural", "naturally", "navigation", "ncategory", "necessary", "need", "never", "new", "nlab", "no", "no", "nocturnes", "nonconcrete", "nonsense", "nontechnical", "norman", "north", "northholland", "not", "notes", "notes", "nothing", "notion", "now", "npov", "number", "object", "objects", "obliged", "observation", "observing", "of", "on", "one", "online", "oosten", "operads", "opposed", "or", "order", "originally", "other", "other", "others", "out", "outside", "outside", "over", "packing", "page", "page", "pages", "paper", "paradigm", "pareigis", "parlance", "part", "particularly", "pdf", "pedagogical", "people", "perfect", "perhaps", "perpetrated", "perspective", "peter", "phenomenon", "phil", "philosopher", "philosophers", "philosophical", "philosophy", "physics", "physics", "pierce", "pierre", "played", "pleasure", "pointed", "poset", "possession", "power", "powered", "powerful", "pp", "preface", "prerequisite", "present", "preserving", "presheaf", "presheaves", "press", "prevail", "print", "probability", "problem", "proceedings", "process", "progression", "project", "proof", "property", "provide", "provides", "ps", "publicly", "published", "pure", "purloining", "purpose", "quite", "quiver", "rails", "rather", "reader", "realizations", "reason", "recalled", "record", "references", "reflect", "reflects", "rejected", "related", "related", "relation", "relation", "relations", "representable", "reprints", "reproduce", "resistance", "rests", "results", "reveals", "reverse", "revised", "revisions", "revisions", "rezk", "riehl", "robert", "role", "row", "ruby", "running", "same", "samuel", "saunders", "say", "scedrov", "schanuel", "schapira", "school", "sci", "science", "scientists", "search", "see", "see", "sense", "sep", "sequence", "serious", "set", "set", "sets", "sets", "sheaf", "sheaves", "shortly", "show", "shulman", "similar", "simon", "simple", "simplified", "simply", "simpson", "since", "single", "site", "situations", "sketches", "skip", "small", "so", "society", "some", "some", "sometimes", "sophisticated", "sophistication", "source", "space", "speak", "special", "specific", "specifically", "speculative", "spivak", "sprache", "stage", "standard", "statements", "steenrod", "stephen", "steps", "steve", "still", "stop", "strecker", "structural", "structuralism", "structure", "structures", "students", "study", "studying", "subjects", "such", "suggest", "summer", "supported", "supports", "symposium", "syntax", "tac", "taken", "talk", "tannaka", "tautological", "technique", "tend", "tends", "term", "terminology", "ternary", "tex", "textbook", "textbooks", "texts", "than", "that", "the", "the", "their", "their", "them", "themselves", "then", "theorem", "theorems", "theorems", "theoretic", "theoretical", "theories", "theorist", "theory", "theory", "there", "there", "these", "these", "they", "thinking", "this", "this", "thought", "through", "throughout", "thus", "time", "to", "tom", "tone", "too", "toolset", "top", "topics", "topoi", "topological", "topology", "topologyhomotopy", "topos", "topos", "toposes", "toposes", "transactions", "transformation", "transformations", "trinitarianism", "trinity", "triple", "triples", "trivial", "trivially", "true", "turns", "two", "two", "type", "typically", "uncountable", "under", "under", "understood", "unification", "unify", "unions", "univalent", "universal", "universal", "universes", "university", "use", "used", "useful", "using", "usual", "van", "variants", "various", "vast", "vect", "versatile", "video", "videos", "viewpoint", "views", "vol", "vol", "vs", "was", "way", "we", "wealth", "web", "wells", "were", "what", "when", "when", "where", "which", "while", "whole", "whose", "will", "willerton", "william", "willingness", "with", "witticism", "words", "working", "working", "would", "writes", "xfy", "xfygzxgfz", "xy", "yoneda", "york1964", "youtube")

  //
  // EXERCISE 9
  //
  // Instantiate the polymorphic game to the `IO[Nothing, ?]` type.
  //
  val myGameIO: IO[Nothing, Unit] = myGame[IO[Nothing, ?]]

  //
  // EXERCISE 10
  //
  // Create a test data structure that can contain a buffer of lines (to be
  // read from the console), a log of output (that has been written to the
  // console), and a list of "random" numbers.
  //
  case class TestData(input: List[String], output: List[String], random: List[Int]) {
    def renderOutput: String =
      output.reverse.mkString("\n")
  }

  //
  // EXERCISE 11
  //
  // Implement the following dynamically-created instance. Effects should be
  // implemented in terms of modifying the passed in `Ref` that contains
  // `TestData`.
  //
  type GameEffects[F[_]] = Console[F] with Random[F] with Monad[F]
  final case class TestIO[+E, +A](run: IO[E, A])
  def createTestInstance[E](ref: Ref[TestData]): GameEffects[TestIO[E, ?]] =
    new Console[TestIO[E, ?]] with Random[TestIO[E, ?]] with Monad[TestIO[E, ?]] {
      def point[A](a: => A): TestIO[E,A] = TestIO(IO.point(a))
      def bind[A, B](fa: TestIO[E,A])(f: A => TestIO[E,B]): TestIO[E,B] =
        TestIO(fa.run.flatMap(f.andThen(_.run)))

      def printLine(line: String): TestIO[E,Unit] =
        TestIO(ref.update(d => d.copy(output = line :: d.output)).void)

      def readLine: TestIO[E, String] =
        TestIO(ref.modify(d => (d.input.head, d.copy(input = d.input.drop(1)))))
      def nextInt(max: Int): TestIO[E,Int] =
        TestIO(ref.modify(d => (d.random.head, d.copy(random = d.random.drop(1)))))
    }

  //
  // EXERCISE 12
  //
  // Implement the following runner function, which will run the game using
  // the provided set of input test data.
  //
  def testGame(data: TestData): IO[Nothing, TestData] =
    for {
      ref   <- Ref(data)
      _     <- ({
                 implicit val F = createTestInstance[Nothing](ref)
                 myGame[TestIO[Nothing, ?]]
               }: TestIO[Nothing, Unit]).run
      data  <- ref.get
    } yield data

  //
  // EXERCISE 13
  //
  // Create some test data for a trial run of the game.
  //
  val GameTest1 = testGame(TestData(
    input  = List("John", "a", "a", "r", "o", "n"),
    output = Nil,
    random = List(0)
  )).map(_.renderOutput)

  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]]{
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util.Try(parsed.path match {
        case Path(parts) =>
          val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

          parsed.withPath(UrlPath(whole))
      }).toOption.map(new URL(_))

    def url: String = parsed.toString

    override def equals(a: Any): Boolean = a match {
      case that : URL => this.url == that.url
      case _ => false
    }

    override def hashCode: Int = url.hashCode
  }

  object URL {
    import io.lemonlabs.uri._

    def apply(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): IO[Exception, String] =
    for {
      promise <-  Promise.make[Exception, String]
      _       <-  (for {
                    exitResult <- IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(
                                    new Runnable () {
                                      def run: Unit =
                                        try {
                                          k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
                                        } catch {
                                          case e : Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
                                        }
                                    }
                                  )) : IO[Nothing, ExitResult[Exception, String]]
                    _          <- promise.done(exitResult)
                  } yield ()).fork
      html    <-  promise.get
    } yield html

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  object test {
    val Home          = URL("http://scalaz.org").get
    val Index         = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About         = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    val getURL: URL => IO[Exception, String] =
      (url: URL) => SiteIndex.get(url).fold[IO[Exception, String]](IO.fail(new Exception("Could not connect to: " + url)))(IO.now(_))

    val ScalazRouter: URL => Set[URL] =
      url => if (url.parsed.apexDomain == Some("scalaz.org")) Set(url) else Set()

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, html) => IO.now(List(url -> html))
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _     <-  putStrLn("Hello World!")
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
