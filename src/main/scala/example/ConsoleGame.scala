package example

import cats.data.State
import cats.effect.IO
import fs2.Stream
import org.fusesource.jansi.{Ansi, AnsiConsole}
import jline.console.{ConsoleReader, KeyMap, Operation}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object ConsoleGame extends App {
  implicit val timer = IO.timer

  val reader = new ConsoleReader()

  case class GameState(pos: (Int, Int))

  AnsiConsole.out.println(Draw.eraseScreen.run(Ansi.ansi()).value._1)

  val km = KeyMap.keyMaps().get("vi-insert")
  val inputHandling = Stream.repeatEval(IO {
    val c = reader.readBinding(km)
    if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
    else Left(c match { case op: Operation => op })
  }).map {
    case Right("q") | Left(Operation.VI_EOF_MAYBE) =>
      None
    case Left(op) => Some(op)
  }.unNoneTerminate

  val ticks = Stream.unfoldEval[IO, Int, Int](0) { tick => IO.sleep(100 milliseconds).map(_ => Some((tick + 1, tick + 1))) }

  val inputAndTicks = inputHandling map Left.apply mergeHaltL (ticks map Right.apply)

  inputAndTicks.evalMapAccumulate(GameState(pos = (6, 7))) {
    case (gameState, Left(op)) =>
      val nextState = handleKeypress(op, gameState)
      IO.pure((nextState, nextState))
    case (gameState, Right(tick)) =>
      IO {
        if (tick % 10 == 0) {
          info("something ".concat(tick.toString))
        }
        drawGame(gameState)
        (gameState, gameState)
      }
  }.compile.drain.unsafeRunSync()

  def info(msg: String): Unit = {
    AnsiConsole.out.println(Ansi.ansi()
      .cursor(5, 0)
      .scrollUp(1)
      .eraseLine()
      .a(msg))
  }

  def handleKeypress(k: Operation, g: GameState): GameState =
    k match {
      // Left arrow
      case Operation.BACKWARD_CHAR =>
        val pos0 = g.pos
        g.copy(pos = (pos0._1 - 1, pos0._2))
      // Right arrow
      case Operation.FORWARD_CHAR =>
        val pos0 = g.pos
        g.copy(pos = (pos0._1 + 1, pos0._2))
      // Down arrow
      case Operation.NEXT_HISTORY =>
        val pos0 = g.pos
        g.copy(pos = (pos0._1, pos0._2 + 1))
      // Up arrow
      case Operation.PREVIOUS_HISTORY =>
        g
      case _ =>
        // println(k)
        g
    }

  def drawGame(g: GameState): Unit = {
    val drawing: State[Ansi, Unit] =
      for {
        _ <- Draw.drawBox(2, 6, 20, 6)
        _ <- Draw.drawBlock(g.pos._1, g.pos._2)
        _ <- Draw.drawText(2, 12, "press 'q' to quit")
      } yield ()
    val result = drawing.run(Ansi.ansi()).value._1
    AnsiConsole.out.println(result)
  }
}
