package helpers

import os.Path
import scala.annotation.targetName

type Parser[T]     = String => T
type ListParser[T] = Parser[List[T]]

def parse[T](input: String)(using p: Parser[T]): T = p(input)

extension [T](input: String)
    def parsed(using p: Parser[T]): T = parse(input)

extension [T](input: Path)
    def parsed(using p: Parser[T]): T = parse(os.read(input))

given Parser[Int]                              = _.toInt
given Parser[String]                           = identity
given Parser[Long]                             = _.toLong
given [T](using p: Parser[T]): Parser[List[T]] = _.split('\n').filter(_.nonEmpty).toList.map(parse).toList

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
end display

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

given Show[Int]  = _.toString
given Show[Long] = _.toString
given Show[Char] = _.toString
given [T : Show]: Show[Board[T]] with
    def show(board: Board[T]): String =
        board.values
            .map: row =>
                row.map(_.show).mkString
            .mkString("\n")
