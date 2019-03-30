package net.andimiller.circe.diff

import io.circe.{DiffPrinter, Json}

object Main extends App {

  println(DiffPrinter.pretty(
    Json.obj(
      "a" -> Json.arr((1 to 10).map(Json.fromInt):_*),
      "b" -> Json.fromInt(123),
      "c" -> Json.obj(('a' to 'z').map(letter => letter.toString -> Json.fromString(letter.toString)) :+ ("blah" -> Json.obj("1" -> Json.fromString("1"))):_*)
    )
  ))

}
