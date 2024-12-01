object day01:
    given parser: Parser[(Long, Long)] = parsers.separatedPairBy("\\s+")(_.toLong)

    @main
    def main(): Unit =
        val input = (os.pwd / "files" / "day01.txt").parsed
        display("Day 1", part1(input), part2(input))
    end main

    def part1(input: List[(Long, Long)]): Long =
        val (first, second) = input.unzip
        first
            .sorted
            .zip(second.sorted)
            .map: (a, b) =>
                Math.absExact(a - b)
            .sum
    end part1

    def part2(input: List[(Long, Long)]): Long =
        val (first, second) = input.unzip
        first
            .map: a =>
                a * second.count(_ == a)
            .sum
    end part2

end day01
