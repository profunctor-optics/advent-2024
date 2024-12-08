package advent2024

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.{Failure, Success, Using}

trait Day extends Product:
  type Input
  type Parsed[+I] = Either[String, I]

  protected def parse(line: String): Parsed[Input]
  protected def run(file: String): Unit

  private def timed[T](task: => T): (T, Duration) =
    val start = System.nanoTime()
    val result = task
    val end = System.nanoTime()
    (result, (end - start).nanos)

  final def testFile: String =
    s"${productPrefix.toLowerCase}.test.txt"

  final def main(args: Array[String]): Unit =
    val ((), elapsed) = timed(run(s"${productPrefix.toLowerCase}.txt"))
    println(s"$productPrefix took ${elapsed.toUnit(TimeUnit.SECONDS)}s")

  final def withResource[R](file: String)(solve: Iterator[Input] => R): R =
    def doParse(line: String, i: Int) = parse(line) match
      case Right(input) => input
      case Left(reason) => throw new IllegalArgumentException(s"Unexpected input on line #$i ($reason): $line")
    def doSolve(source: Source) =
      solve(source.getLines().zipWithIndex.filterNot(_._1.isBlank).map(doParse))
    Using(Source.fromResource(file))(doSolve) match
      case Success(result) => result
      case Failure(error) => throw error
