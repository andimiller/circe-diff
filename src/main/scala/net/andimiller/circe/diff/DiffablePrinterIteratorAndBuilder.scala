package net.andimiller.circe.diff

import cats.implicits._
import io.circe.Json.Folder
import io.circe.{Json, JsonNumber, JsonObject}

object DiffablePrinterIteratorAndBuilder {

  sealed trait PrinterAction
  case object Indent extends PrinterAction
  case object Unindent extends PrinterAction
  case class Print(s: String) extends PrinterAction
  case object Newline extends PrinterAction
  case object Trim extends PrinterAction

  implicit class TrimmableStringBuilder(s: StringBuilder) {
    def trim: StringBuilder = {
      ((s.length - 1) to 0 by -1).toIterator.takeWhile { i =>
        s(i).isWhitespace
      }.foldLeft(Option.empty[Int])((_, i) => Some(i)).foreach { i =>
        s.delete(i, s.length)
      }
      s
    }
  }

  case class Interpreter(var indent: Int = 0, var buffer: StringBuilder = new StringBuilder)

  def go(events: Iterator[PrinterAction]) = {
    events.foldLeft(new Interpreter) {
      case (interpreter, action) =>
        action match {
          case Indent   => interpreter.indent += 1
          case Unindent => interpreter.indent -= 1
          case Print(s) => interpreter.buffer.append(s)
          case Newline =>
            interpreter.buffer.append("\n")
            for (_ <- 0 to interpreter.indent) {
              interpreter.buffer.append("  ")
            }
          case Trim => interpreter.buffer.trim
        }
        interpreter
    }
  }

  val folder: Folder[Iterator[PrinterAction]] =
    new Folder[Iterator[PrinterAction]] {
      override def onNull: Iterator[PrinterAction] = Iterator(Print("null"))
      override def onBoolean(value: Boolean): Iterator[PrinterAction] =
        Iterator(Print(value match {
          case true  => "true"
          case false => "false"
        }))
      override def onNumber(value: JsonNumber): Iterator[PrinterAction] =
        Iterator(Print(value.toString))
      override def onString(value: String): Iterator[PrinterAction] =
        Iterator(Print("\"" + value + "\""))
      override def onArray(value: Vector[Json]): Iterator[PrinterAction] =
        Iterator(
          Iterator(
            Print("["),
            Indent,
            Newline
          ),
          value
            .flatMap(j =>
                j.foldWith(folder) ++ (Print(",") :: Newline :: Nil)
            ),
          Iterator(
            Trim,
            Unindent,
            Newline,
            Print("]")
          ),
        ).flatten
      override def onObject(value: JsonObject): Iterator[PrinterAction] =
        Iterator(
          Iterator(Print("{"), Indent, Newline),
          (value.toList.sortBy(_._1)).flatMap {
            case (k, v) =>
              Iterator(
                  Iterator(
                    Print(s""""$k":"""),
                    Indent,
                    Newline
                  ),
                v.foldWith(folder),
                  Iterator(
                    Print(","),
                    Unindent,
                    Newline
                  )
              ).flatten
          }.toIterator,
          Iterator(
            Trim,
            Unindent,
            Newline,
            Print("}")
          ),
        ).flatten
    }

  def print(j: Json) = j.foldWith(folder)

}
