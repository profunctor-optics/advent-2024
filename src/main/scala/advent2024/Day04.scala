package advent2024

object Day04 extends Day[Array[Char]]:
  class TextSearch(text: Array[Array[Char]], word: String):
    assert(word.nonEmpty, "empty word")
    assert(text.nonEmpty, "empty text")

    private val wl = word.length
    private val wh = wl / 2
    private val xb = text.indices
    private val yb = text.head.indices

    def this(lines: Iterator[Array[Char]], word: String) =
      this(lines.toArray, word)

    private def foundAt(i: Int, j: Int)(di: Int, dj: Int): Boolean =
      if !xb.contains(i + di * (wl - 1)) then return false
      if !yb.contains(j + dj * (wl - 1)) then return false
      var ci = i
      var cj = j
      val it = word.iterator
      while it.hasNext do
        if text(ci)(cj) == it.next() then
          ci += di
          cj += dj
        else return false
      true

    private def crossAt(i: Int, j: Int) =
      (foundAt(i - wh, j - wh)(1, 1) || foundAt(i + wh, j + wh)(-1, -1))
        && (foundAt(i - wh, j + wh)(1, -1) || foundAt(i + wh, j - wh)(-1, 1))

    private def countAt(i: Int, j: Int) =
      val words = for
        di <- (-1 to 1).iterator
        dj <- (-1 to 1).iterator
        if foundAt(i, j)(di, dj)
      yield 1
      words.sum

    def countAll: Int =
      val words = for
        i <- xb.iterator
        j <- yb.iterator
      yield countAt(i, j)
      words.sum

    def crossAll: Int =
      if wl % 2 == 0 then return 0
      val words = for
        i <- (wh until xb.end - wh).iterator
        j <- (wh until yb.end - wh).iterator
        if crossAt(i, j)
      yield 1
      words.sum

  def parse(line: String): Option[Array[Char]] =
    Some(line.toCharArray)

  def part1(file: String): Int =
    withResource(file)(TextSearch(_, "XMAS").countAll)

  def part2(file: String): Int =
    withResource(file)(TextSearch(_, "MAS").crossAll)

  def main(args: Array[String]): Unit =
    println(part1("day04.txt"))
    println(part2("day04.txt"))
