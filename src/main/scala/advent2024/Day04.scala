package advent2024

object Day04 extends Day[Array[Char]]:
  class TextSearch(text: Array[Array[Char]], word: String):
    assert(word.nonEmpty, "empty word")
    assert(text.nonEmpty, "empty text")

    private val len = word.length
    private val mid = len / 2
    private val row = text.indices
    private val col = text.head.indices

    def this(lines: Iterator[Input], word: String) =
      this(lines.toArray, word)

    private def foundAt(i: Int, j: Int)(di: Int, dj: Int): Boolean =
      if !row.contains(i + di * (len - 1)) then return false
      if !col.contains(j + dj * (len - 1)) then return false
      var ci = i
      var cj = j
      val it = word.iterator
      while it.hasNext do
        if text(ci)(cj) == it.next() then
          ci += di
          cj += dj
        else return false
      true

    private def crossAt(i: Int, j: Int) = text(i)(j) == word(mid)
      && (foundAt(i - mid, j - mid)(+1, +1) || foundAt(i + mid, j + mid)(-1, -1))
      && (foundAt(i - mid, j + mid)(+1, -1) || foundAt(i + mid, j - mid)(-1, +1))

    private def countAt(i: Int, j: Int): Int =
      if text(i)(j) != word.head then return 0
      val words = for
        di <- (-1 to 1).iterator
        dj <- (-1 to 1).iterator
        if foundAt(i, j)(di, dj)
      yield 1
      words.sum

    def countAll: Int =
      val words = for
        i <- row.iterator
        j <- col.iterator
      yield countAt(i, j)
      words.sum

    def crossAll: Int =
      if len % 2 == 0 then return 0
      val words = for
        i <- (mid until row.end - mid).iterator
        j <- (mid until col.end - mid).iterator
        if crossAt(i, j)
      yield 1
      words.sum

  def parse(line: String): Option[Input] =
    Some(line.toCharArray)

  def part1(file: String): Int =
    withResource(file)(TextSearch(_, "XMAS").countAll)

  def part2(file: String): Int =
    withResource(file)(TextSearch(_, "MAS").crossAll)

  def main(args: Array[String]): Unit =
    println(part1("day04.txt"))
    println(part2("day04.txt"))
