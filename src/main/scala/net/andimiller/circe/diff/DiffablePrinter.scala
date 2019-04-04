package net.andimiller.circe.diff

import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json.Folder
import fs2._
import cats._
import cats.effect.{IO, Sync}
import cats.implicits._

object DiffablePrinter {

  sealed trait PrinterAction
  case object Indent extends PrinterAction
  case object Unindent extends PrinterAction
  case class Print(s: String) extends PrinterAction
  case object Newline extends PrinterAction
  case object Trim extends PrinterAction

  case class Interpreter(indent: Int = 0, buffer: String = "")

  def go[F[_]](events: Stream[IO, PrinterAction]) = {
    events.fold(new Interpreter) {
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

  def folder[F[_]: Sync]: Folder[Stream[F, PrinterAction]] =
    new Folder[Stream[F, PrinterAction]] {
      override def onNull: Stream[F, PrinterAction] = Stream.emit(Print("null"))
      override def onBoolean(value: Boolean): Stream[F, PrinterAction] =
        Stream.emit(Print(value match {
          case true  => "true"
          case false => "false"
        }))
      override def onNumber(value: JsonNumber): Stream[F, PrinterAction] =
        Stream.emit(Print(value.toString))
      override def onString(value: String): Stream[F, PrinterAction] =
        Stream.emit(Print("\"" + value + "\""))
      override def onArray(value: Vector[Json]): Stream[F, PrinterAction] =
        List(
          Stream.emits(List(
            Print("["),
            Indent,
            Newline
          )),
          Stream
            .emits(value)
            .evalMap(j =>
              Sync[F].delay {
                j.foldWith(folder[F]) ++ Stream.emit(Print(",")) ++ Stream.emit(
                  Newline)
              }
            ).flatMap(identity),
          Stream.emits(List(
            Trim,
            Unindent,
            Newline,
            Print("]")
          )),
        ).combineAll
      override def onObject(value: JsonObject): Stream[F, PrinterAction] =
        List(
          Stream.emits(List(Print("{"), Indent, Newline)),
          Stream.emits(value.toList.sortBy(_._1)).flatMap {
            case (k, v) =>
              List(
                Stream.emits(
                  List(
                    Print(s""""$k":"""),
                    Indent,
                    Newline
                  )),
                v.foldWith(folder[F]),
                Stream.emits(
                  List(
                    Print(","),
                    Unindent,
                    Newline
                  ))
              ).combineAll
          },
          Stream.emits(List(
            Trim,
            Unindent,
            Newline,
            Print("}")
          )),
        ).combineAll
    }

  def print(j: Json) = j.foldWith(folder[IO])

}
