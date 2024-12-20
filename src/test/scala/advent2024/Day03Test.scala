package advent2024

class Day03Test extends munit.FunSuite:
  test("compute the sum of all multiplications"):
    val memory = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    val actual = Day03.withSample(memory)(Day03.compute)
    assertEquals(actual, 161L)

  test("compute the sum of all multiplications with conditionals"):
    val memory = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    val actual = Day03.withSample(memory)(Day03.computeCond)
    assertEquals(actual, 48L)

  test("don't without braces has no effect"):
    val memory = "xmul(2,4)&mul[3,7]!^don't_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    val actual = Day03.withSample(memory)(Day03.computeCond)
    assertEquals(actual, 161L)

  test("not compute invalid instructions"):
    val invalid = """
      |mul(4*
      |mul(6,9!
      |?(12,34)
      |mul ( 2 , 4 )
      |""".stripMargin

    assertEquals(Day03.withSample(invalid)(Day03.compute), 0L)
    assertEquals(Day03.withSample(invalid)(Day03.computeCond), 0L)
