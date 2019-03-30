package net.andimiller.circe.diff

import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json.Folder

object KeySorter {

  def apply(j: Json): Json =
    j.foldWith(new Folder[Json] {
      override def onNull: Json = Json.Null
      override def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
      override def onNumber(value: JsonNumber): Json = Json.fromJsonNumber(value)
      override def onString(value: String): Json = Json.fromString(value)
      override def onArray(value: Vector[Json]): Json = Json.arr(value:_*)
      override def onObject(value: JsonObject): Json = Json.obj(value.toIterable.toVector.sortBy(_._1):_*)
    })
}
