package aoc2024

import helpers.*
import helpers.given

object Day02 extends AoCDay:
    enum Order:
        case Asc, Desc, Uninitialized

    case class Report(levels: List[Int]):
        def isSafe: Boolean =
            def loop(list: List[Int], order: Order): Boolean =
                list match
                    case first :: second :: tail =>
                        val diff     = second - first
                        if Math.abs(diff) < 1 || Math.abs(diff) > 3 then return false
                        val newOrder = if diff > 0 then Order.Asc else if diff < 0 then Order.Desc else order
                        if order == Order.Uninitialized then loop(second :: tail, newOrder)
                        else if order != newOrder then false
                        else loop(second :: tail, order)
                    case _                       => true
            loop(levels, Order.Uninitialized)
        end isSafe

        def subreports: List[Report] =
            levels.zipWithIndex.map: (level, index) =>
                Report(levels.patch(index, Nil, 1))
    end Report

    type Input  = Report
    type Output = Int
    given parser: Parser[Input] = input =>
        Report(input.split(" ").toList.map(_.toInt))

    @main
    def main(): Unit =
        val input = (os.pwd / "files" / "day02.txt").parsed
        display("Day 02", part1(input), part2(input))
    end main

    def part1(input: List[Input]): Output =
        input.count(_.isSafe)
    end part1

    def part2(input: List[Input]): Output =
        input.count: report =>
            report.isSafe || report.subreports.exists(_.isSafe)
    end part2
end Day02
