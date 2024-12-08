package advent2024

class Day07Test extends munit.FunSuite:
  test("calculates the calibrated sum"):
    val actual = Day07.withTestFile(Day07.part1)
    assertEquals(actual, 3749L)

  test("calculates the calibrated sum with concat"):
    val actual = Day07.withTestFile(Day07.part2)
    assertEquals(actual, 11387L)
