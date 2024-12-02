package aoc2024

import aoc2024.Day02.Report
import helpers.{*, given}

object Day01 extends AoCDay:
    type Input = (Long, Long)
    type Output = Long
    given parser: Parser[Input] = parsers.separatedPairBy("\\s+")(_.toLong)

    def main(): Unit =
        val input = (os.pwd / "files" / "day01.txt").parsed
        display("Day 1", part1(input), part2(input))
    end main

    def part1(input: List[Input]): Output =
        val (first, second) = input.unzip
        first
            .sorted
            .zip(second.sorted)
            .map: (a, b) =>
                Math.absExact(a - b)
            .sum
    end part1

    def part2(input: List[Input]): Output =
        val (first, second) = input.unzip
        first
            .map: a =>
                a * second.count(_ == a)
            .sum
    end part2

end Day01