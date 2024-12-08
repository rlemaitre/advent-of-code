package helpers

import scala.compiletime.constValue

case class Board[T](width: Int, height: Int, values: List[List[T]])(using e: T):
    val empty: T = e
    def apply(pos: Position): T = values(pos.y)(pos.x)
    
    def get(pos: Position): Option[T] =
        if pos.inBounds(this) then Some(this(pos)) else None

    def update(pos: Position, value: T): Board[T] =
        copy(values = values.updated(pos.y, values(pos.y).updated(pos.x, value)))

    def forAllNeighbours(pos: Position)(f: T => Boolean): Boolean =
        pos.neighbours.forall(n => n.inBounds(this) && f(this(n)))

    def countNeighbours(pos: Position)(f: T => Boolean): Int =
        pos.neighbours.count(n => n.inBounds(this) && f(this(n)))

    def existsNeighbours(pos: Position)(f: T => Boolean): Boolean =
        pos.neighbours.exists(n => n.inBounds(this) && f(this(n)))

    def filter(f: (pos: Position, value: T) => Boolean): Board[T] =
        val newValues = values.zipWithIndex.map: (row, y) =>
            row.zipWithIndex.map: (value, x) =>
                if f(Position(x, y), value) then value else empty
        Board(width, height, newValues)

    def count(f: T => Boolean): Int = values.flatten.count(f)

    def display(using Show[T]): Unit =
        values.foreach(row => println(row.map(_.show).mkString))

    def foreach(f: (T, Position) => Unit): Unit =
        values.zipWithIndex.foreach: (row, y) =>
            row.zipWithIndex.foreach: (value, x) =>
                f(value, Position(x, y))

    def map[B](f: T => B)(using emptyB: B): Board[B] =
        Board[B](width, height, values.map(_.map(f)))

    def flatMap[B](f: T => Board[B])(using emptyB: B): Board[B] =
        val newValues = values.zipWithIndex.flatMap: (row, y) =>
            row.zipWithIndex.map: (value, x) =>
                f(value).values(y)(x)
        Board[B](width, height, newValues.grouped(width).toList)

    def findAll(f: T => Boolean): List[Position] =
        values.zipWithIndex.flatMap: (row, y) =>
            row.zipWithIndex.collect:
                case (value, x) if f(value) => Position(x, y)
end Board

object Board:
    def apply[T](str: String, p: Char => T)(using empty: T): Board[T] =
        val values = str.split('\n').toList.filter(_.nonEmpty).map(_.map(p).toList)
        Board[T](values.head.size, values.size, values)

    def simple(str: String): Board[Char] = apply(str, identity)(using '.')
end Board


type SimpleBoard = Board[Char]

opaque type Position = (Int, Int)
object Position:
    def apply(x: Int, y: Int): Position = (x, y)

extension (pos: Position)
    def x: Int                             = pos._1
    def y: Int                             = pos._2
    def neighbours: List[Position]         =
        List(
            (pos.x - 1, pos.y),
            (pos.x + 1, pos.y),
            (pos.x, pos.y - 1),
            (pos.x, pos.y + 1),
            (pos.x - 1, pos.y - 1),
            (pos.x + 1, pos.y - 1),
            (pos.x - 1, pos.y + 1),
            (pos.x + 1, pos.y + 1)
        )
    def inBounds(board: Board[?]): Boolean =
        pos.x >= 0 && pos.x < board.width && pos.y >= 0 && pos.y < board.height

    def move(direction: Direction): Position = direction match
        case Direction.North     => Position(pos.x, pos.y - 1)
        case Direction.South     => Position(pos.x, pos.y + 1)
        case Direction.East      => Position(pos.x + 1, pos.y)
        case Direction.West      => Position(pos.x - 1, pos.y)
        case Direction.NorthEast => Position(pos.x + 1, pos.y - 1)
        case Direction.NorthWest => Position(pos.x - 1, pos.y - 1)
        case Direction.SouthEast => Position(pos.x + 1, pos.y + 1)
        case Direction.SouthWest => Position(pos.x - 1, pos.y + 1)
end extension

enum Direction:
    case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest