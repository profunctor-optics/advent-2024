package advent2024

import scala.collection.mutable

case object Day06 extends Day:
  type Input = Array[Point]

  enum Point(val char: Char):
    case Blank extends Point('.')
    case Blocked extends Point('#')
    case Left extends Point('<')
    case Up extends Point('^')
    case Right extends Point('>')
    case Down extends Point('v')

    def isBlank: Boolean = this == Blank
    def isBlocked: Boolean = this == Blocked
    def isVisited: Boolean = !isBlank && !isBlocked
    override def toString: String = char.toString

  private object Point:
    val byChar = values.iterator.map(p => p.char -> p).toMap

  private class Guard(lab: Array[Array[Point]], var i: Int, var j: Int):
    private def point = lab(i)(j)
    private var heading = point
    private val start = (i, j)
    private val x = lab.length
    private val y = lab.head.length

    var visited = 1
    var obstructed = 0

    private def moveTo(pos: (Int, Int)): this.type =
      i = pos._1
      j = pos._2
      this

    private def headTo(heading: Point): this.type =
      this.heading = heading
      this

    def reset(): this.type =
      moveTo(start).headTo(point)

    private def nextPosition = heading match
      case Point.Left => (i, j - 1)
      case Point.Right => (i, j + 1)
      case Point.Up => (i - 1, j)
      case Point.Down => (i + 1, j)
      case Point.Blank | Point.Blocked => (i, j)

    private def nextHeading = heading match
      case Point.Left => Point.Up
      case Point.Up => Point.Right
      case Point.Right => Point.Down
      case Point.Down => Point.Left
      case Point.Blank => Point.Blank
      case Point.Blocked => Point.Blocked

    private def isLeaving = heading match
      case Point.Left => j <= 0
      case Point.Up => i <= 0
      case Point.Right => j >= y - 1
      case Point.Down => i >= x - 1
      case Point.Blank | Point.Blocked => false

    private def isBlocked =
      val (i, j) = nextPosition
      lab(i)(j).isBlocked

    private def track(): Unit =
      if point.isBlank then
        lab(i)(j) = heading
        visited += 1

    private def move(simulate: Boolean) =
      var turned = 0
      while isBlocked && turned < 4 do
        headTo(nextHeading)
        turned += 1
      if !simulate then track()
      if turned < 4 then moveTo(nextPosition)
      turned > 0

    def patrol(simulate: Boolean): this.type =
      if point.isBlocked then return this
      val turns = mutable.Set.empty[(Int, Int, Point)]
      var stuck = false
      while !stuck && !isLeaving
      do if move(simulate) then stuck = !turns.add((i, j, heading))
      if !simulate then track()
      else if stuck then obstructed += 1
      this

  def solve(input: Iterator[Input]): (Int, Int) =
    val lab = input.toArray
    if lab.isEmpty then return (0, 0)

    val initial = for
      i <- lab.indices.iterator
      j <- lab(i).indices
      if lab(i)(j).isVisited
    yield Guard(lab, i, j)

    val guard = initial.nextOption() match
      case Some(guard) => guard.patrol(simulate = false)
      case None => return (0, 0)

    for
      i <- lab.indices.iterator
      j <- lab(i).indices
    do
      val point = lab(i)(j)
      if point.isVisited then
        lab(i)(j) = Point.Blocked
        guard.reset().patrol(simulate = true)
        lab(i)(j) = point

    (guard.visited, guard.obstructed)

  def parse(line: String): Parsed[Input] =
    Right(line.toCharArray.map(Point.byChar))

  def part12(file: String): (Int, Int) =
    withResource(file)(solve)

  def run(file: String): Unit =
    val (visited, obstructed) = part12(file)
    println(visited)
    println(obstructed)
