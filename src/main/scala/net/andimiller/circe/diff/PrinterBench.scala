package net.andimiller.circe.diff

import org.openjdk.jmh.annotations.{Benchmark, State}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.RunnerException
import org.openjdk.jmh.runner.options.Options
import org.openjdk.jmh.runner.options.OptionsBuilder
import cats.effect._
import io.circe.Json

class PrinterBench {

  @Benchmark
  def myParser(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = DiffablePrinter
      .go(DiffablePrinter.print(json))
      .compile
      .toList
      .unsafeRunSync()
  }

  @Benchmark
  def myListParser(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = DiffablePrinterLists
      .go(DiffablePrinterLists.print(json))
  }

  @Benchmark
  def myListAndMutableParser(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = DiffablePrinterListsAndBuilder
      .go(DiffablePrinterListsAndBuilder.print(json))
  }

  @Benchmark
  def myIterableAndMutableParser(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = DiffablePrinterIteratorAndBuilder
      .go(DiffablePrinterIteratorAndBuilder.print(json))
  }

  @Benchmark
  def myListAndMutableWithSizingPrinter(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = DiffablePrinterListsAndBuilderWithSizing
      .go(DiffablePrinterListsAndBuilderWithSizing.print(json))
  }
  /*
  @Benchmark
  def circe(): Unit = {
    val json =
      Json.arr(
        Json.arr(Json.Null),
        Json.arr((1 to 10).map(Json.fromInt): _*),
        Json.obj(
          "a" -> Json.fromString("aaa"),
          "b" -> Json.obj(
            "hello" -> Json.fromString("world")
          )
        )
      )
    val string = json.spaces2
  }
  */

}
