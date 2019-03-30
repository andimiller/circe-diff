package net.andimiller.circe.diff

import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json.Folder
import fs2._
import cats._, cats.implicits._, cats.effect._

object DiffablePrinter extends IOApp {

  sealed trait PrinterAction
  case object Indent extends PrinterAction
  case object Unindent extends PrinterAction
  case class Print(s: String) extends PrinterAction
  case class Newline(s: String = "") extends PrinterAction

  case class Interpreter(indent: Int = 0, buffer: String = "")

  def go[F[_]: Sync](events: Stream[F, PrinterAction]) = {
    events.fold(new Interpreter) { case (interpreter, action) =>
      action match {
        case Indent => interpreter.copy(indent = interpreter.indent + 1)
        case Unindent =>  interpreter.copy(indent = interpreter.indent - 1)
        case Print(s) => interpreter.copy(buffer = interpreter.buffer + s)
        case Newline(s) => interpreter.copy(buffer = interpreter.buffer + s + "\n" + ("  " * interpreter.indent))
      }
    }
  }

  def folder[F[_]: Sync]: Folder[Stream[F, PrinterAction]] = new Folder[Stream[F, PrinterAction]] {
    override def onNull: Stream[F, PrinterAction] = Stream.emit(Print("null"))
    override def onBoolean(value: Boolean): Stream[F, PrinterAction] = Stream.emit(Print(value match {
      case true => "true"
      case false => "false"
    }))
    override def onNumber(value: JsonNumber): Stream[F, PrinterAction] = Stream.emit(Print(value.toString))
    override def onString(value: String): Stream[F, PrinterAction] = Stream.emit(Print("\"" + value + "\""))
    override def onArray(value: Vector[Json]): Stream[F, PrinterAction] = {
      Stream.emit(Print("[")) ++
      Stream.emit(Indent) ++
      Stream.emit(Newline()) ++
      Stream.emits(value).flatMap(
        _.foldWith(folder[F]) ++ Stream.emit(Newline(","))
      ) ++
      Stream.emit(Unindent) ++
      Stream.emit(Newline()) ++
      Stream.emit(Print("]"))
    }
    override def onObject(value: JsonObject): Stream[F, PrinterAction] = {
      Stream.emit(Print("{")) ++
      Stream.emit(Indent) ++
      Stream.emits(value.toList.sortBy(_._1)).flatMap { case (k, v) =>
        Stream.emit(Print(s""""$k" : """)) ++
        Stream.emit(Indent) ++
        v.foldWith(folder[F]) ++
        Stream.emit(Unindent)
      }
    }
  }

  def print(j: Json) = j.foldWith(folder[IO])

  override def run(args: List[String]): IO[ExitCode] = {
    go(print(
        Json.arr((1 to 10).map(Json.fromInt):_*)
    )).compile.toList.map(r => println(r)).as(ExitCode.Success)
  }
}
