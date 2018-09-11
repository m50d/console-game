package example

import cats.data.State
import org.fusesource.jansi.Ansi

object Draw {
  def eraseScreen: State[Ansi, Unit] =
    State.modify { _.eraseScreen() }

  def saveCursorPosition: State[Ansi, Unit] =
    State.modify { _.saveCursorPosition() }

  def restoreCursorPosition: State[Ansi, Unit] =
    State.modify { _.restoreCursorPosition() }

  def drawBlock(x: Int, y: Int): State[Ansi, Unit] = State.modify { b: Ansi =>
    b.bold
      .cursor(y, x)
      .a("***")
      .cursor(y + 1, x)
      .a(" * ")
      .reset
  }

  def drawBox(x0: Int, y0: Int, w: Int, h: Int): State[Ansi, Unit] = State.modify { b: Ansi =>
    require(w > 1 && h > 1)
    val topStr = "┌".concat("─" * (w - 2)).concat("┐")
    val wallStr = "│".concat(" " * (w - 2)).concat("│")
    val bottomStr = "└".concat("─" * (w - 2)).concat("┘")
    val top = b.cursor(y0, x0).a(topStr)
    val walls = (0 to h - 2).toList.foldLeft(top) { (bb: Ansi, i: Int) =>
       bb.cursor(y0 + i + 1, x0).a(wallStr)
     }
    walls.cursor(y0 + h - 1, x0).a(bottomStr)
  }

  def drawText(x: Int, y: Int, text: String): State[Ansi, Unit] = State.modify { b: Ansi =>
    b.cursor(y, x).a(text)
  }
}
