package net.andimiller.circe.diff

import io.circe.{Json, JsonNumber, JsonObject}

object Extractors {
  object IsNull {
    def unapply(arg: Json): Option[Unit] = Option(()).filter(_ => arg.isNull)
  }
  object IsArray {
    def unapply(arg: Json): Option[Vector[Json]] = arg.asArray
  }
  object IsBoolean {
    def unapply(arg: Json): Option[Boolean] = arg.asBoolean
  }
  object IsNumber {
    def unapply(arg: Json): Option[JsonNumber] = arg.asNumber
  }
  object IsObject {
    def unapply(arg: Json): Option[JsonObject] = arg.asObject
  }
  object IsString {
    def unapply(arg: Json): Option[String] = arg.asString
  }
}
