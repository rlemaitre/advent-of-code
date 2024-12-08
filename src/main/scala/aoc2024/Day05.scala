package aoc2024

import helpers.*
import helpers.given

object Day05 extends AoCDay:
    type Output = Int
    type Input  = (List[Rule], List[Print])

    extension (i: Input)
        def rules: List[Rule]   = i._1
        def prints: List[Print] = i._2
    end extension

    given parser: Parser[Input] = str =>
        str.split("\n\n") match
            case Array(ruleLines, printLines) =>
                val rules  =
                    ruleLines.linesIterator.filter(_.nonEmpty).map(parsers.separatedPairBy[Page]("\\|")(_.toInt)).toList
                val prints =
                    printLines.linesIterator.filter(_.nonEmpty).map(parsers.separatedBy[Page](",")(_.toInt)).toList
                (rules, prints)
            case _                            => throw RuntimeException("Invalid input")

    def main(args: Array[String]): Unit =
        val input = (os.pwd / "files" / "day05.txt").parsed
        display("Day 05", part1(input), part2(input))
    end main

    def part1(input: Input): Output =
        input
            .prints
            .filter(_.isValid(input.rules))
            .map: print =>
                println(s"$print is valid, middle is ${print.middle}")
                print.middle
            .sum
    end part1

    def part2(input: Input): Output =
        input.prints
            .filter(!_.isValid(input.rules))
            .map: p =>
                val ordered = p.inCorrectOrder(input.rules)
                ordered.middle
            .sum
    end part2

    opaque type Page  = Int
    opaque type Rule  = (Page, Page)
    opaque type Print = List[Page]

    extension (r: Rule)
        def first: Page = r._1
        def second: Page = r._2
        def invert: Rule = r.second -> r.first
    end extension
    extension (p: Print)
        def middle: Page = p(p.size / 2)

        def isValid(rules: List[Rule]): Boolean      =
            p match
                case head :: tail =>
                    val dependencyInTail = !rules.exists:
                        case (a, b) if b == head => tail.contains(a)
                        case _                   => false
                    dependencyInTail && tail.isValid(rules)
                case Nil          => true
        def inCorrectOrder(rules: List[Rule]): Print =
            val appliedRules = rules.map(_.invert).collect:
                case (a, b) if p.contains(a) && p.contains(b) => (a, b)
            val applicableRules          = MultiMap[Int, Int](appliedRules)
            val toSort = (p.toSet -- applicableRules.keySet).foldLeft(applicableRules):
                case (acc, page) =>
                    acc.addAll(page, Set.empty)
            val sorted = toSort.topologicalSort(identity)
            sorted match
                case Left(value)  => throw RuntimeException(value)
                case Right(value) =>
                    value.map(_._1)
        end inCorrectOrder
    end extension
end Day05
