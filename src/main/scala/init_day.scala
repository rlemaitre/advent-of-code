package aoc

import java.time.LocalDate
import scala.io.StdIn
import sttp.client4.Response
import sttp.client4.quick.*
import helpers.*
import helpers.given


object initDay:
    @main
    def init(): Unit =
        val day: Day = readDay
        val year     = 2024

        val tokenFile  = os.pwd / ".token"
        if !os.exists(tokenFile) then
            sys.error("Please create a file named .token in the root of the project with your session token")
        val token      = os.read(tokenFile).trim
        val inputPath  = os.pwd / "files" / s"day${day.show}.txt"
        if !os.exists(inputPath) then
            val response = quickRequest
                .get(uri"https://adventofcode.com/$year/day/$day/input")
                .header("Cookie", s"session=$token")
                .send()
            os.write(inputPath, response.body)
            println(s"Input file for day ${day.show} created")
        else
            println(s"Input file for day ${day.show} already exists")
        end if
        val sourcePath = os.pwd / s"day${day.show}.scala"
        if !os.exists(sourcePath) then
            os.write(sourcePath, sourceCode(day))
            println(s"Source file for day ${day.show} created")
        else
            println(s"Source file for day ${day.show} already exists")
        end if
        val testPath   = os.pwd / s"day${day.show}.test.scala"
        if !os.exists(testPath) then
            os.write(testPath, testCode(day))
            println(s"Test file for day ${day.show} created")
        else
            println(s"Test file for day ${day.show} already exists")
        end if
    end init

    private def readDay: Day =
        println("Enter the day number (0 for today):")
        val stringOrDay = StdIn.readInt() match
            case 0 => Day(LocalDate.now().getDayOfMonth)
            case d => Day(d)
        val day: Day = stringOrDay match
            case Left(error) => sys.error(error)
            case Right(d)  => d
        day
    end readDay

    opaque type Day = Int
    object Day:
        def apply(value: Int): Either[String, Day] =
            if value >= 1 && value <= 25 then Right(value)
            else Left("Day must be between 1 and 25")
    given Show[Day] = day => f"$day%02d"

    opaque type Year = Int
    object Year:
        def apply(value: Int): Either[String, Year] =
            if value == 2024 then Right(value)
            else Left("Year must be 2024")

    private def sourceCode(day: Day): String = s"""
          |object day${day.show} extends AoCDay:
          |    type Output = Long
          |    type Input = (Long, Long)
          |
          |    given parser: Parser[Input] = ??? // Define the parser here
          |
          |    @main
          |    def main(): Unit =
          |        val input = (os.pwd / "files" / "day${day.show}.txt").parsed
          |        display("Day ${day.show}", part1(input), part2(input))
          |    end main
          |
          |    def part1(input: List[Input]): Output =
          |       ??? // Define the first part here
          |    end part1
          |
          |    def part2(input: List[Input]): Output =
          |       ??? // Define the second part here
          |    end part2
          |end day${day.show}
          |""".stripMargin

    private def testCode(day: Day): String = s"""
          |class Day${day.show}Test extends munit.FunSuite:
          |    val testInput: String =
          |        \"\"\"
          |          |\"\"\".stripMargin
          |    import day${day.show}.*
          |    import day${day.show}.given
          |
          |    test("part 1"):
          |        assertEquals(part1(testInput.parsed), ???)
          |    test("part 2"):
          |        assertEquals(part2(testInput.parsed), ???)
          |end Day${day.show}Test
          |""".stripMargin
end initDay
