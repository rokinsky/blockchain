package com.github.rokinsky.blockchain

import cats.Show
import cats.syntax.show.*

import scala.util.Properties.lineSeparator as EOL

object PPrint:
  def pprH[A: Show](values: Seq[A]): String =
    intercalateS(" ", values)

  def pprV[A: Show](values: Seq[A]): String =
    intercalateS(EOL, values)

  def intercalateS[A: Show](sep: String, values: Seq[A]): String = values match
    case Nil => ""
    case x :: Nil => x.show
    case x :: xs => s"${x.show}$sep${intercalateS(sep, xs)}"

  given[A: Show]: Show[(String, A)] with
    def show(pair: (String, A)): String = pair match
      case (key, value) => s"$key: ${value.show}"
