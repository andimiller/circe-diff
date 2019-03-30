package io.circe

import io.circe.Printer.PrintingFolder
import java.lang.StringBuilder


object DiffPrinter {
  val indent = "  "
  val lbraceLeft = ""
  val lbraceRight = "\n"
  val rbraceLeft = "\n"
  val rbraceRight = ""
  val lbracketLeft = ""
  val lbracketRight = "\n"
  val rbracketLeft = "\n"
  val rbracketRight = ""
  val lrbracketsEmpty = "\n"
  val arrayCommaLeft = ""
  val arrayCommaRight = "\n"
  val objectCommaLeft = ""
  val objectCommaRight = "\n"
  val colonLeft = ""
  val colonRight = "\n  "

  val openBraceText = "{"
  val closeBraceText = "}"
  val openArrayText = "["
  val closeArrayText = "]"
  val commaText = ","
  val colonText = ":"

  case class PF(sb: java.lang.StringBuilder = new java.lang.StringBuilder) extends PrintingFolder(sb, pieces, false, false) {
    override def onBoolean(value: Boolean): Unit = sb.append(value)
    override def onNumber(value: JsonNumber): Unit = value.appendToStringBuilder(sb)
  }

  def pretty(j: Json): String = {
    val pf = new PF()
    j.foldWith(pf)
    pf.sb.toString
  }


  val pieces: Printer.PiecesAtDepth =
      new Printer.MemoizedPieces(indent) {
        final def compute(i: Int): Printer.Pieces = {
          val builder = new StringBuilder()

          addIndentation(builder, lbraceLeft, i)
          builder.append(openBraceText)
          addIndentation(builder, lbraceRight, i + 1)

          val lBraces = builder.toString

          builder.setLength(0)

          addIndentation(builder, rbraceLeft, i)
          builder.append(closeBraceText)
          addIndentation(builder, rbraceRight, i + 1)

          val rBraces = builder.toString

          builder.setLength(0)

          addIndentation(builder, lbracketLeft, i)
          builder.append(openArrayText)
          addIndentation(builder, lbracketRight, i + 2)

          val lBrackets = builder.toString

          builder.setLength(0)

          addIndentation(builder, rbracketLeft, i + 1)
          builder.append(closeArrayText)
          addIndentation(builder, rbracketRight, i + 1)

          val rBrackets = builder.toString

          builder.setLength(0)

          builder.append(openArrayText)
          addIndentation(builder, lrbracketsEmpty, i)
          builder.append(closeArrayText)

          val lrEmptyBrackets = builder.toString

          builder.setLength(0)

          addIndentation(builder, arrayCommaLeft, i + 1)
          builder.append(commaText)
          addIndentation(builder, arrayCommaRight, i + 2)

          val arrayCommas = builder.toString

          builder.setLength(0)

          addIndentation(builder, objectCommaLeft, i)
          builder.append(commaText)
          addIndentation(builder, objectCommaRight, i + 1)

          val objectCommas = builder.toString

          builder.setLength(0)

          addIndentation(builder, colonLeft, i)
          builder.append(colonText)
          addIndentation(builder, colonRight, i + 1)

          val colons = builder.toString

          Printer.Pieces(lBraces, rBraces, lBrackets, rBrackets, lrEmptyBrackets, arrayCommas, objectCommas, colons)
        }
      }


}
