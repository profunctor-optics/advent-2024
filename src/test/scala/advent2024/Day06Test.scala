package advent2024

class Day06Test extends munit.FunSuite:
  test("count all visited points and possible obstructions"):
    val (visited, obstructed) = Day06.part12(Day06.testFile)
    assertEquals(visited, 41)
    assertEquals(obstructed, 6)
