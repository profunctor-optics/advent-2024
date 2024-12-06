package advent2024

class Day01Test extends munit.FunSuite:
  test("compute the total distance"):
    val actual = Day01.part1("day01.test.txt")
    assertEquals(actual, 11L)

  test("compute the similarity score"):
    val actual = Day01.part2("day01.test.txt")
    assertEquals(actual, 31L)
