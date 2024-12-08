import helpers.*
import java.time.LocalDate
import scala.io.StdIn
import sttp.client4.quick.*

object InitDay:
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
        val sourcePath = os.pwd / "src" / "main" / "scala" / s"aoc$year" / s"Day${day.show}.scala"
        if !os.exists(sourcePath) then
            os.write(sourcePath, sourceCode(day, year))
            println(s"Source file for day ${day.show} created")
        else
            println(s"Source file for day ${day.show} already exists")
        end if
        val testPath   = os.pwd / "src" / "test" / "scala" / s"aoc$year" / s"Day${day.show}Test.scala"
        if !os.exists(testPath) then
            os.write(testPath, testCode(day, year))
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
        val day: Day    = stringOrDay match
            case Left(error) => sys.error(error)
            case Right(d)    => d
        day
    end readDay

    opaque type Day = Int
    private object Day:
        def apply(value: Int): Either[String, Day] =
            if value >= 1 && value <= 25 then Right(value)
            else Left("Day must be between 1 and 25")
    given Show[Day] = day => f"$day%02d"

    opaque type Year = Int
    object Year:
        def apply(value: Int): Either[String, Year] =
            if value == 2024 then Right(value)
            else Left("Year must be 2024")

    private def sourceCode(day: Day, year: Year): String = s"""
        |package aoc$year
        |
        |import helpers.*
        |import helpers.given
        |
        |object Day${day.show} extends AoCDay:
        |    type Output = Long
        |    type Input = ???
        |
        |    given parser: Parser[Input] = ??? // Define the parser here
        |
        |    def main(args: Array[String]): Unit =
        |        val input = (os.pwd / "files" / "day${day.show}.txt").parsed
        |        display("Day ${day.show}", part1(input), part2(input))
        |    end main
        |
        |    def part1(input: Input): Output =
        |       ??? // Define the first part here
        |    end part1
        |
        |    def part2(input: Input): Output =
        |       ??? // Define the second part here
        |    end part2
        |end Day${day.show}
        |""".stripMargin

    private def testCode(day: Day, year: Year): String = s"""
        |package aoc$year
        |
        |import helpers.*
        |import helpers.given
        |class Day${day.show}Test extends munit.FunSuite:
        |    val testInput: String =
        |        \"\"\"
        |          |\"\"\".stripMargin
        |    import Day${day.show}.*
        |    import Day${day.show}.given
        |
        |    test("part 1"):
        |        assertEquals(part1(testInput.parsed), ???)
        |    test("part 2"):
        |        assertEquals(part2(testInput.parsed), ???)
        |end Day${day.show}Test
        |""".stripMargin
end InitDay
