package advent2024

class Day09Test extends munit.FunSuite:
  val smallSample = "12345"
  val bigSample = "2333133121414131402"

  test("calculate the blocks checksum of the small sample"):
    val actual = Day09.withSample(smallSample)(Day09.part1)
    assertEquals(actual, 60L)

  test("calculate the blocks checksum of the big sample"):
    val actual = Day09.withSample(bigSample)(Day09.part1)
    assertEquals(actual, 1928L)

  test("calculate the files checksum of the small sample"):
    val actual = Day09.withSample(smallSample)(Day09.part2)
    assertEquals(actual, 60L)
