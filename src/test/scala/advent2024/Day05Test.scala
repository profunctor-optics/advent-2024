package advent2024

class Day05Test extends munit.FunSuite:
  test("sum all sorted queues"):
    val actual = Day05.withTestFile(Day05.part1)
    assertEquals(actual, 143L)

  test("fix all unsorted queues"):
    val actual = Day05.withTestFile(Day05.part2)
    assertEquals(actual, 123L)
