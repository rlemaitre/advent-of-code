package helpers

trait AoCDay:
    type Input
    type Output

    def part1(input: Input): Output
    def part2(input: Input): Output
    
    def main(args: Array[String]): Unit

