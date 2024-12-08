package advent2024

class Day02Test extends munit.FunSuite:
  test("one duplicate is safe"):
    assert(Day02.safeReport1(IArray(1, 2, 2, 3)))

  test("one inversion at the beginning is safe"):
    assert(Day02.safeReport1(IArray(5, 1, 2, 3)))

  test("one inversion at position 1 is safe"):
    assert(Day02.safeReport1(IArray(1, 5, 2, 3)))

  test("one inversion at the end is safe"):
    assert(Day02.safeReport1(IArray(1, 2, 3, 0)))

  test("one inversion in the middle safe"):
    assert(Day02.safeReport1(IArray(1, 2, 3, 0, 4, 5, 6)))

  test("count the safe reports"):
    val actual = Day02.part1(Day02.testFile)
    assertEquals(actual, 2)

  test("count the safe reports with mitigation"):
    val actual = Day02.part2(Day02.testFile)
    assertEquals(actual, 4)
