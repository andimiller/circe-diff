package net.andimiller.circe.diff

import cats._
import cats.implicits._
import cats.effect._
import io.circe.{ACursor, Json, JsonNumber, JsonObject}
import fs2._

object Core extends App {
  import Extractors._
  implicit class IndentableString(s: String) {
    def indent(i: Int): String = s.lines.map(l => (" "*i) + l).mkString("\n")
    def prepend(p: String): String = s.lines.map(l => p + l).mkString("\n")
    def prependSuccessive(p: String): String = {
      val lines = s.lines.toList
      (lines.head ++ lines.tail.map(l => p + l)).mkString("\n")
    }
  }


  sealed trait Diff
  final case class LeftHas(j: Json) extends Diff
  final case class RightHas(r: Json) extends Diff
  final case class Different(l: Json, r: Json) extends Diff

  def diff(a: Json, b: Json) = {
    go(a.hcursor, b.hcursor)
  }

  type traverse = (ACursor, ACursor) => List[Diff]

  println(diff(
    Json.obj("a" -> Json.fromInt(1)),
    Json.obj("a" -> Json.fromInt(42)),
  ))

  def go(l: ACursor, r: ACursor, indent: Int = 0): String = {
    (l.focus, r.focus) match {
      case (None, Some(rv)) =>
        rv.spaces2.indent(indent).prepend(">>")
      case (Some(lv), None) =>
        lv.spaces2.indent(indent).prepend("<<")
      case (Some(IsNull(_)), Some(IsNull(_))) =>
        "null"
      case (Some(IsObject(lv)), Some(IsObject(rv))) =>
        val sb = new StringBuilder
        sb.append("\n{")
        val (left, right) = (lv, rv).bimap(_.keys.toSet, _.keys.toSet)
        val leftOnly = left diff right
        val rightOnly = right diff left
        lv.keys.foreach {
          case k if leftOnly contains k =>
            sb.append("\n<<")
            sb.append(k)
            sb.append(":")
            sb.append(lv(k).get.spaces2.prependSuccessive("<<"))
          case k if rightOnly contains k =>
            sb.append("\n>>")
            sb.append(k)
            sb.append(":")
            sb.append(lv(k).get.spaces2.prependSuccessive(">>"))
          case k =>
            sb.append("\n")
            sb.append(k)
            sb.append(":")
            val (leftValue, rightValue) = (lv, rv).bimap(_(k).get, _(k).get)
            sb.append(go(leftValue.hcursor, rightValue.hcursor))
        }
        sb.append("\n}")
        sb.mkString
    case (Some(IsNumber(lv)), Some(IsNumber(rv))) =>
        if (lv.eqv(rv)) {
          lv.toString
        } else {
          s"""<<$lv
            |>>$rv
            |""".stripMargin
        }
      /*
    case (Some(IsArray(lv)), Some(IsArray(rv))) =>
    case (Some(lv), Some(rv)) if lv.isBoolean && rv.isBoolean =>
    case (Some(lv), Some(rv)) if lv.isNull && rv.isNull =>
    case (Some(lv), Some(rv)) if lv.isNull && rv.isNull =>
    */
      case (Some(lv), Some(rv)) =>
        rv.spaces2.indent(indent).prepend(">>") +
          "\n" +
        lv.spaces2.indent(indent).prepend("<<")
    }
  }

}
