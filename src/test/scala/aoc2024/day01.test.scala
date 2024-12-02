package aoc2024

import Day01.{part1, part2}
import helpers.{*, given}

class Day01Test extends munit.FunSuite:
    val testInput: String =
        """
          |3   4
          |4   3
          |2   5
          |1   3
          |3   9
          |3   3
          |""".stripMargin
    import Day01.{*, given}

    test("part 1"):
        assertEquals(part1(testInput.parsed), 11L)
    test("part 2"):
        assertEquals(part2(testInput.parsed), 31L)
end Day01Test
