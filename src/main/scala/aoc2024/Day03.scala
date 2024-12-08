package aoc2024

import aoc2024.Day03.Instruction.Do
import aoc2024.Day03.Instruction.Dont
import aoc2024.Day03.Instruction.Mul
import helpers.*
import helpers.given
import scala.annotation.tailrec
import scala.language.future
import scala.util.matching.Regex

object Day03 extends AoCDay:
    enum Instruction:
        case Mul(a: Long, b: Long)
        case Do
        case Dont

    type Input  = List[List[Instruction]]
    type Output = Long
    private val pattern: Regex = """dont'\(\).*do\(\)""".r

    def clean(input: String): String                     =
        ???
    // incorrect 47585037
    @tailrec
    private def removeAllDisabled(input: String): String =
        pattern.findFirstIn(input) match
            case Some(_) =>
                val str = pattern.replaceFirstIn(input, "")
                println(s"Removing disabled: $input")
                println(s"Result: $str")
                removeAllDisabled(str)
            case None    =>
                println(s"Final result: $input")
                input

    private def removeFinalDisabled(input: String): String =
        println(s"Removing final disabled: $input")
        val index  = input.lastIndexOf("don't()")
        val string = if index <= 0 then input else input.substring(0, index)
        println(s"Result: $string")
        string
    end removeFinalDisabled

    given parser: Parser[List[Instruction]] = input =>
        val r       = """mul\((\d+),(\d+)\)""".r
//        r.findAllMatchIn(input).toSeq.distinct
        val cleaned = removeFinalDisabled(removeAllDisabled(input))
        println(s"cleaned: $cleaned")
        require(cleaned != input)
        r.findAllMatchIn(cleaned).toSeq.distinct
            .map: m =>
                val a = m.group(1).toLong
                val b = m.group(2).toLong
                Instruction.Mul(a, b)
            .toList

//    @main
    def main(args: Array[String]): Unit =
        val input: Input = (os.pwd / "files" / "day03.txt").parsed
        display("Day 03", part1(input), part2(input))
    end main

    // 157621318
    def part1(input: Input): Output =
        input
            .map: is =>
                is
                    .collect:
                        case m: Mul => m.a * m.b
                    .sum
            .sum
    end part1

    def part2(input: Input): Output =
        input
            .map: is =>
                is
                    .collect:
                        case m: Mul => m.a * m.b
                    .sum
            .sum
//        @tailrec
//        def loop(is: List[Instruction], enabled: Boolean, acc: Long): Long =
//            is match
//                case Nil          => acc
//                case head :: tail =>
//                    println(s"Evaluating: $head")
//                    head match
//                        case Dont      =>
//                            println("disabling")
//                            loop(tail, false, acc)
//                        case Do        =>
//                            println("enabling")
//                            loop(tail, true, acc)
//                        case Mul(a, b) =>
//                            val inc = if enabled then a * b else 0
//                            println(s"inc mul($a, $b): $inc")
//                            loop(tail, enabled, acc + inc)
//                        case _         => ???
//                    end match
//        input.map: is =>
//            val inc = loop(is, enabled = true, 0L)
//            println(s"result: $inc")
//            inc
//        .sum
    end part2
end Day03
