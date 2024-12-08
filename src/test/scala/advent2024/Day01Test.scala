package advent2024

class Day01Test extends munit.FunSuite:
  test("compute the total distance"):
    val actual = Day01.withTestFile(Day01.totalDistance)
    assertEquals(actual, 11L)

  test("compute the similarity score"):
    val actual = Day01.withTestFile(Day01.similarityScore)
    assertEquals(actual, 31L)
