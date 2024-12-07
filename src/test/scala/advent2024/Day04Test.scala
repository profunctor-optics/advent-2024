package advent2024

class Day04Test extends munit.FunSuite:
  val xmas = """
    |..X...
    |.SAMX.
    |.A..A.
    |XMAS.S
    |.X....
    |""".stripMargin.trim

  val mas = """
    |M.S
    |.A.
    |M.S
    |""".stripMargin.trim

  test("count all XMAS in the sample"):
    val search = Day04.TextSearch(xmas.linesIterator.map(IArray.from), "XMAS")
    assertEquals(search.countAll, 4)

  test("count all XMAS in the file"):
    val actual = Day04.part1("day04.test.txt")
    assertEquals(actual, 18)

  test("cross all MAS in the sample"):
    val search = Day04.TextSearch(mas.linesIterator.map(IArray.from), "MAS")
    assertEquals(search.crossAll, 1)

  test("cross all MAS in the file"):
    val actual = Day04.part2("day04.test.txt")
    assertEquals(actual, 9)
