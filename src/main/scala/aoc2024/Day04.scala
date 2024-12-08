package aoc2024

import helpers.*
import helpers.given
import scala.annotation.tailrec

object Day04 extends AoCDay:
    type Output = Long
    type Input  = Board[Char]

    given Parser[Input] = Board.simple

    def main(args: Array[String]): Unit =
        val input = (os.pwd / "files" / "day04.txt").parsed
        display("Day 04", part1(input), part2(input))
    end main

    def part1(input: Input): Output =
        input.findAll(_ == 'X')
            .map: pos =>
                Direction
                    .values
                    .count: direction =>
                        val nextPosition = pos.move(direction)
                        val nextIsLegit  = nextPosition.inBounds(input)
                        val hasWord      = if nextIsLegit then input.hasWord(nextPosition, 'X', direction) else false
//                        println(s"Checking $pos -> $nextPosition: $nextIsLegit, $hasWord")
                        nextIsLegit && hasWord
            .sum
    end part1

    extension (board: Board[Char])
        @tailrec
        def hasWord(pos: Position, from: Char, direction: Direction): Boolean =
            val currentValue = board(pos)
            val nextPosition = pos.move(direction)
            val nextIsLegit  = nextPosition.inBounds(board)
            println(s"Checking $pos -> $nextPosition (${if nextIsLegit then "OK" else "KO"}) : $currentValue, $from")
            from match
                case 'X' if currentValue == 'M' => nextIsLegit && hasWord(nextPosition, 'M', direction)
                case 'M' if currentValue == 'A' => nextIsLegit && hasWord(nextPosition, 'A', direction)
                case 'A' if currentValue == 'S' => true
                case _                          => false
            end match

    def part2(input: Input): Output =
        input.findAll(_ == 'A')
            .count: pos =>
                val nw = input.get(pos.move(Direction.NorthWest))
                val se = input.get(pos.move(Direction.SouthEast))
                val ne = input.get(pos.move(Direction.NorthEast))
                val sw = input.get(pos.move(Direction.SouthWest))
                val diag1 =  (nw.contains('M') && se.contains('S')) || (nw.contains('S') && se.contains('M'))
                val diag2 =  (ne.contains('M') && sw.contains('S')) || (ne.contains('S') && sw.contains('M'))
                diag1 && diag2
    end part2

end Day04
