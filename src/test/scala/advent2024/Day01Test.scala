package advent2024

class Day01Test extends munit.FunSuite:
  test("compute the total distance"):
    val actual = Day01.part1(Day01.testFile)
    assertEquals(actual, 11L)

  test("compute the similarity score"):
    val actual = Day01.part2(Day01.testFile)
    assertEquals(actual, 31L)
