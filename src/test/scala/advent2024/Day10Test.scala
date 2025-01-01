package advent2024

class Day10Test extends munit.FunSuite:
  test("calculate trail score of sample #1"):
    val sample = """
      |0123
      |1234
      |8765
      |9876
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part1)
    assertEquals(actual, 1)

  test("calculate trail score of sample #2"):
    val sample = """
      |...0...
      |...1...
      |...2...
      |6543456
      |7.....7
      |8.....8
      |9.....9
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part1)
    assertEquals(actual, 2)

  test("calculate trail score of sample #3"):
    val sample = """
      |..90..9
      |...1.98
      |...2..7
      |6543456
      |765.987
      |876....
      |987....
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part1)
    assertEquals(actual, 4)

  test("calculate trail score of sample #4"):
    val sample = """
      |10..9..
      |2...8..
      |3...7..
      |4567654
      |...8..3
      |...9..2
      |.....01
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part1)
    assertEquals(actual, 3)

  test("calculate trail score of the test file"):
    val actual = Day10.withTestFile(Day10.part1)
    assertEquals(actual, 36)

  test("calculate trail rating of sample #1"):
    val sample = """
      |.....0.
      |..4321.
      |..5..2.
      |..6543.
      |..7..4.
      |..8765.
      |..9....
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part2)
    assertEquals(actual, 3)

  test("calculate trail rating of sample #2"):
    val sample = """
      |..90..9
      |...1.98
      |...2..7
      |6543456
      |765.987
      |876....
      |987....
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part2)
    assertEquals(actual, 13)

  test("calculate trail rating of sample #3"):
    val sample = """
      |012345
      |123456
      |234567
      |345678
      |4.6789
      |56789.
      |""".stripMargin

    val actual = Day10.withSample(sample)(Day10.part2)
    assertEquals(actual, 227)

  test("calculate trail rating of the test file"):
    val actual = Day10.withTestFile(Day10.part2)
    assertEquals(actual, 81)
