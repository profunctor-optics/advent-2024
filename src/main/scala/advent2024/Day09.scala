package advent2024

import scala.annotation.tailrec

case object Day09 extends Day:
  type Input = Array[File]

  final case class File(pos: Int, id: Int, blocks: Int):
    def isFree: Boolean = id % 2 == 1
    def isEmpty: Boolean = blocks <= 0
    def isBefore(that: File): Boolean = this.id < that.id
    def isBigger(that: File): Boolean = this.blocks >= that.blocks
    def prev(disk: Input, n: Int = 1): File = disk(id - n)
    def next(disk: Input, n: Int = 1): File = disk(id + n)
    def move(that: File): File = this.copy(pos = that.pos, blocks = this.blocks min that.blocks)
    def take(that: File): File = that.copy(blocks = that.blocks - this.blocks)
    def drop(that: File): File = this.copy(pos = pos + that.blocks, blocks = this.blocks - that.blocks)
    def checksum: Long = (pos.toLong * 2 + blocks - 1) * blocks / 2 * id / 2

  private def blocksChecksum(partial: Boolean)(disk: Input): Long =
    @tailrec def loop(f: File, g: File, sum: Long): Long =
      if g.isBefore(f) then sum
      else if f.isEmpty then loop(f.next(disk), g, sum)
      else if g.isEmpty || g.isFree then loop(f, g.prev(disk), sum)
      else if !f.isBefore(g) then sum + g.checksum
      else if !f.isFree then loop(f.next(disk), g, sum + f.checksum)
      else if partial then
        val checksum = g.move(f).checksum
        if f.isBigger(g) then loop(f.drop(g), g.prev(disk, 2), sum + checksum)
        else loop(f.next(disk), f.take(g), sum + checksum)
      else
        val free = f.id.until(g.id).by(2).iterator.map(disk)
        val checksum = free.find(_.isBigger(g)) match
          case Some(f) =>
            disk(f.id) = f.drop(g)
            g.move(f).checksum
          case None =>
            g.checksum
        loop(disk(f.id), g.prev(disk, 2), sum + checksum)

    if disk.sizeIs <= 2 then 0
    else loop(disk.head, disk.last, 0)

  def parse(line: String): Parsed[Input] =
    val files = line.iterator.scanLeft(File(pos = 0, id = -1, blocks = 0)):
      case (f, b) => File(pos = f.pos + f.blocks, id = f.id + 1, blocks = b.asDigit)
    Right(files.drop(1).toArray)

  def part1(input: Iterator[Input]): Long =
    input.map(blocksChecksum(partial = true)).sum

  def part2(input: Iterator[Input]): Long =
    input.map(blocksChecksum(partial = false)).sum

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
