package net.andimiller.circe.diff

import cats.effect.{IO, Sync}
import cats.implicits._
import fs2._
import io.circe.Json.Folder
import io.circe.{Json, JsonNumber, JsonObject}

object DiffablePrinterLists {

  sealed trait PrinterAction
  case object Indent extends PrinterAction
  case object Unindent extends PrinterAction
  case class Print(s: String) extends PrinterAction
  case object Newline extends PrinterAction
  case object Trim extends PrinterAction

  case class Interpreter(indent: Int = 0, buffer: String = "")

  def go(events: List[PrinterAction]) = {
    events.foldLeft(new Interpreter) {
      case (interpreter, action) =>
        action match {
          case Indent   => interpreter.copy(indent = interpreter.indent + 1)
          case Unindent => interpreter.copy(indent = interpreter.indent - 1)
          case Print(s) => interpreter.copy(buffer = interpreter.buffer + s)
          case Newline =>
            interpreter.copy(
              buffer = interpreter.buffer + "\n" + ("  " * interpreter.indent))
          case Trim => interpreter.copy(buffer = interpreter.buffer.trim)
        }
    }
  }

  val folder: Folder[List[PrinterAction]] =
    new Folder[List[PrinterAction]] {
      override def onNull: List[PrinterAction] = List(Print("null"))
      override def onBoolean(value: Boolean): List[PrinterAction] =
        List(Print(value match {
          case true  => "true"
          case false => "false"
        }))
      override def onNumber(value: JsonNumber): List[PrinterAction] =
        List(Print(value.toString))
      override def onString(value: String): List[PrinterAction] =
        List(Print("\"" + value + "\""))
      override def onArray(value: Vector[Json]): List[PrinterAction] =
        List(
          List(
            Print("["),
            Indent,
            Newline
          ),
          value
            .flatMap(j =>
                j.foldWith(folder) ++ (Print(",") :: Newline :: Nil)
            ),
          List(
            Trim,
            Unindent,
            Newline,
            Print("]")
          ),
        ).flatten
      override def onObject(value: JsonObject): List[PrinterAction] =
        List(
          List(Print("{"), Indent, Newline),
          (value.toList.sortBy(_._1)).flatMap {
            case (k, v) =>
              List(
                  List(
                    Print(s""""$k":"""),
                    Indent,
                    Newline
                  ),
                v.foldWith(folder),
                  List(
                    Print(","),
                    Unindent,
                    Newline
                  )
              ).flatten
          },
          List(
            Trim,
            Unindent,
            Newline,
            Print("}")
          ),
        ).flatten
    }

  def print(j: Json) = j.foldWith(folder)

}
