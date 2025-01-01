package advent2024

class Day12Test extends munit.FunSuite:
  val sample1 = """
    |AAAA
    |BBCD
    |BBCC
    |EEEC
    |""".stripMargin

  test("calculate fence price of sample #1"):
    val actual = Day12.withSample(sample1)(Day12.part1)
    assertEquals(actual, 140L)

  test("calculate fence price of sample #2"):
    val sample = """
      |OOOOO
      |OXOXO
      |OOOOO
      |OXOXO
      |OOOOO
      |""".stripMargin

    val actual = Day12.withSample(sample)(Day12.part1)
    assertEquals(actual, 772L)

  test("calculate fence price of the test file"):
    val actual = Day12.withTestFile(Day12.part1)
    assertEquals(actual, 1930L)

  test("calculate discounted price of sample #1"):
    val actual = Day12.withSample(sample1)(Day12.part2)
    assertEquals(actual, 80L)

  test("calculate discounted price of sample #2"):
    val sample = """
      |EEEEE
      |EXXXX
      |EEEEE
      |EXXXX
      |EEEEE
      |""".stripMargin

    val actual = Day12.withSample(sample)(Day12.part2)
    assertEquals(actual, 236L)

  test("calculate discounted price of sample #3"):
    val sample = """
      |AAAAAA
      |AAABBA
      |AAABBA
      |ABBAAA
      |ABBAAA
      |AAAAAA
      |""".stripMargin

    val actual = Day12.withSample(sample)(Day12.part2)
    assertEquals(actual, 368L)

  test("calculate discounted price of the test file"):
    val actual = Day12.withTestFile(Day12.part2)
    assertEquals(actual, 1206L)
