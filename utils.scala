import os.Path
import scala.annotation.targetName

type Parser[T] = String => T

def parse[T](input: String)(using p: Parser[T]): List[T] =
    input.split("\n").filter(_.nonEmpty).toList.map(p)

def parseFile[T](path: Path)(using p: Parser[T]): List[T] =
    parse(os.read(path))

extension [T](input: String)
    def parsed(using p: Parser[T]): List[T] = parse(input)

extension [T](input: Path)
    def parsed(using p: Parser[T]): List[T] = parseFile(input)

given Parser[Int]    = _.toInt
given Parser[String] = identity

object parsers:
    def separatedBy(separator: String): Parser[List[String]] = input =>
        input.split(separator).toList

    def separatedPairBy[T](separator: String)(f: String => T): Parser[(T, T)] = input =>
        val Array(a, b) = input.split(separator)
        f(a) -> f(b)

    def separatedPair(separator: String): Parser[(String, String)] = separatedPairBy(separator)(identity)
end parsers

trait Show[T]:
    def show(input: T): String

extension [T](input: T)(using show: Show[T])
    def show: String = show.show(input)

def display[T1: Show, T2: Show](title: String, part1: T1, part2: T2): Unit =
    println("*" * (title.length + 4))
    println(s"* $title *")
    println("*" * (title.length + 4))
    println()
    println("Part 1:")
    println(part1.show)
    println()
    println("Part 2:")
    println(part2.show)

// Base case: Empty tuple
given Show[EmptyTuple] = _ => "()"

// Inductive case: Non-empty tuple
@targetName("showTuple")
given [H, T <: Tuple](using sh: Show[H], st: Show[T]): Show[H *: T] = input =>
    val headShow = sh.show(input.head)
    val tailShow = st.show(input.tail)
    s"($headShow, $tailShow)"

given [T: Show]: Show[Iterable[T]] = _.map(show).mkString("[", ", ", "]")

given Show[String] with
    def show(input: String): String = s""""$input""""

given Show[Int] = _.toString
given Show[Long] = _.toString
