package advent2024

import scala.annotation.tailrec

case object Day09 extends Day:
  type Input = IndexedSeq[Int]

  final case class File(id: Int, blocks: Int):
    def isFree: Boolean = id % 2 == 1
    def prev(disk: Input): File = File.at(id - 2, disk)
    def next(disk: Input): File = File.at(id + 1, disk)
    def take(that: File): File = copy(blocks = that.blocks)
    def remaining(that: File): File = copy(blocks = this.blocks - that.blocks)
    def advance(pos: Int): Int = pos + blocks
    def checksum(pos: Int): Long = (pos.toLong * 2 + blocks - 1) * blocks / 2 * id / 2

  object File:
    def at(id: Int, disk: Input): File = File(id, disk(id))

  private def blocksChecksum(disk: Input): Long =
    @tailrec def loop(f: File, g: File, pos: Int, sum: Long): Long =
      if f.id > g.id then sum
      else if f.isFree then
        if f.blocks >= g.blocks then loop(f.remaining(g), g.prev(disk), g.advance(pos), sum + g.checksum(pos))
        else loop(f.next(disk), g.remaining(f), f.advance(pos), sum + g.take(f).checksum(pos))
      else if f.id == g.id then loop(f, g.prev(disk), g.advance(pos), sum + g.checksum(pos))
      else loop(f.next(disk), g, f.advance(pos), sum + f.checksum(pos))

    val size = disk.size
    if size <= 2 then 0
    else loop(File.at(0, disk), File.at(size - 2 + size % 2, disk), 0, 0)

  private def filesChecksum(disk: Input): Long =
    @tailrec def loop(f: File, gId: Int, pos: Int, sum: Long): Long =
      if f.id > gId then sum
      else if f.isFree then
        gId.until(f.id).by(-2).find(disk(_) <= f.blocks).map(File.at(_, disk)) match
          case Some(g) => loop(f.remaining(g), gId - 2, g.advance(pos), sum + g.checksum(pos))
          case None => loop(f.next(disk), gId - 2, f.advance(pos), sum)
      else loop(f.next(disk), gId, f.advance(pos), sum + f.checksum(pos))

    val size = disk.size
    if size <= 2 then 0
    else loop(File.at(0, disk), size - 2 + size % 2, 0, 0)

  def parse(line: String): Parsed[Input] =
    Right(line.map(_.asDigit))

  def part1(input: Iterator[Input]): Long =
    input.map(blocksChecksum).sum

  def part2(input: Iterator[Input]): Long =
    input.map(filesChecksum).sum

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
