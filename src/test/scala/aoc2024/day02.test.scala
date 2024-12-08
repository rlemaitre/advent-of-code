package aoc2024

import helpers.*
import helpers.given

class Day02Test extends munit.FunSuite:
    val testInput: String = """
        |7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9
        |""".stripMargin
    test("part 1"):
        assertEquals(Day02.part1(testInput.parsed), 2)
    test("part 2"):
        assertEquals(Day02.part2(testInput.parsed), 4)
end Day02Test
