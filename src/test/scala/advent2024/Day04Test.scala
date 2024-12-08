package advent2024

class Day04Test extends munit.FunSuite:
  val xmas = """
    |..X...
    |.SAMX.
    |.A..A.
    |XMAS.S
    |.X....
    |""".stripMargin

  val mas = """
    |M.S
    |.A.
    |M.S
    |""".stripMargin

  test("count all XMAS in the sample"):
    val actual = Day04.withSample(xmas)(Day04.part1)
    assertEquals(actual, 4)

  test("count all XMAS in the file"):
    val actual = Day04.withTestFile(Day04.part1)
    assertEquals(actual, 18)

  test("cross all MAS in the sample"):
    val actual = Day04.withSample(mas)(Day04.part2)
    assertEquals(actual, 1)

  test("cross all MAS in the file"):
    val actual = Day04.withTestFile(Day04.part2)
    assertEquals(actual, 9)
