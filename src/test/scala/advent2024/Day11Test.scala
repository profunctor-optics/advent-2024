package advent2024

class Day11Test extends munit.FunSuite:
  val sample = "125 17"

  test("calculate number of stones in the sample"):
    val actual = Day11.withSample(sample)(Day11.part1)
    assertEquals(actual, 55312L)
