package advent2024

import scala.annotation.tailrec

case object Day04 extends Day:
  type Input = IArray[Char]

  private class TextSearch(text: IArray[IArray[Char]], word: String):
    private val len = word.length
    private val mid = len / 2
    private val row = text.indices
    private val col = text.head.indices

    def this(lines: Iterator[Input], word: String) =
      this(IArray.from(lines), word)

    private def foundAt(i: Int, j: Int)(di: Int, dj: Int): Boolean =
      if !row.contains(i + di * (len - 1)) then return false
      if !col.contains(j + dj * (len - 1)) then return false
      val it = word.iterator
      @tailrec def loop(i: Int, j: Int): Boolean = it.nextOption() match
        case Some(c) if c == text(i)(j) => loop(i + di, j + dj)
        case Some(_) => false
        case None => true
      loop(i, j)

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

  def parse(line: String): Parsed[Input] =
    Right(IArray.unsafeFromArray(line.toCharArray))

  def part1(input: Iterator[Input]): Int =
    TextSearch(input, "XMAS").countAll

  def part2(input: Iterator[Input]): Int =
    TextSearch(input, "MAS").crossAll

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part1))
