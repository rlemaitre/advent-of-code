package aoc2024

import Day04.*
import Day04.given
import helpers.*
import helpers.given

class Day04Test extends munit.FunSuite:
    val testInput: String = """
        |MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX
        |""".stripMargin

    test("part 1"):
        val parsed: Board[Char] = testInput.parsed
        println(s"Parsed: Board(width=${parsed.width}, height=${parsed.height})")
        println(parsed.show)
        assertEquals(part1(parsed), 18L)
    test("part 2"):
        assertEquals(part2(testInput.parsed), 9L)
end Day04Test
