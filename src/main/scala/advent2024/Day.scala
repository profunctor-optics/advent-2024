package advent2024

import java.time.Duration
import scala.io.Source
import scala.util.{Failure, Success, Using}

trait Day:
  type Input

  protected def parse(line: String): Option[Input]

  final def timed[T](task: => T): (T, Duration) =
    val start = System.nanoTime()
    val result = task
    val end = System.nanoTime()
    (result, Duration.ofNanos(end - start))

  final def withResource[R](file: String)(solve: Iterator[Input] => R): R =
    def doParse(line: String, i: Int) =
      parse(line).getOrElse(throw new IllegalArgumentException(s"Unexpected input on line #$i: $line"))
    def doSolve(source: Source) =
      solve(source.getLines().zipWithIndex.filterNot(_._1.isBlank).map(doParse))
    Using(Source.fromResource(file))(doSolve) match
      case Success(result) => result
      case Failure(error) => throw error
