package net.andimiller.circe.diff

import atto._
import Atto._
import cats._
import cats.implicits._
import io.circe.Json

object DiffableParser extends App {

  lazy val json: Parser[Json] = jstring | jnull | jnumber | jarray | jobject
  val jstring = stringLiteral.map(Json.fromString)
  val jnull = string("null").map(_ => Json.Null)
  val jnumber = bigDecimal.map(Json.fromBigDecimal)
  val jarray = for {
    _ <- char('[') <* skipWhitespace
    items <- (skipWhitespace *> json <* skipWhitespace <* opt(char(',')) <* skipWhitespace).many
    _ <- char(']') <* skipWhitespace
  } yield Json.arr(items: _*)

  val jobjectkv: Parser[(String, Json)] = for {
    key <- stringLiteral <* skipWhitespace
    _ <- char(':') <* skipWhitespace
    value <- json <* skipWhitespace
    _ <- opt((char(',') <* skipWhitespace))
  } yield key -> value

  val jobject = for {
    _ <- char('{') <* skipWhitespace
    kvs <- jobjectkv.many
    _ <- char('}') <* skipWhitespace
  } yield Json.obj(kvs: _*)

  val input = "[1,2,3,4,]"

  println(json.parse(input).done)

}
