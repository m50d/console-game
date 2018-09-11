package example

import cats.effect.IO
import fs2.Stream
import org.fusesource.jansi.{Ansi, AnsiConsole}
import jline.console.{ConsoleReader, KeyMap, Operation}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class GameState(pos: (Int, Int))

object ConsoleGame extends App {
  implicit val timer = IO.timer

  val eraseScreen = Draw.eraseScreen.run(Ansi.ansi()).value._1

  val inputHandling = {
    val reader = new ConsoleReader()
    val km = KeyMap.keyMaps().get("vi-insert")
    Stream.repeatEval(IO {
      val c = reader.readBinding(km)
      if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
      else Left(c match { case op: Operation => op })
    })
  }.map(handleKeypress)
    .unNoneTerminate

  val ticks = Stream.unfoldEval[IO, Int, Int](0) { tick => IO.sleep(100 milliseconds).map(_ => Some((tick + 1, tick + 1))) }

  val inputAndTicks = inputHandling map Left.apply mergeHaltL (ticks map Right.apply)

  val gameAnsis = inputAndTicks.mapAccumulate(GameState(pos = (6, 7))) {
    case (gameState, Left(step)) =>
      (step(gameState), Seq.empty)
    case (gameState, Right(tick)) =>
      (gameState, (if (tick % 10 == 0)
        Seq(info("something ".concat(tick.toString)))
      else Seq.empty) :+ drawGame(gameState))
  }.flatMap {
    case (_, ansis) => Stream.emits(ansis)
  }

  Stream.emit(eraseScreen).append(gameAnsis)
    .observe1(ansi => IO {
    AnsiConsole.out.println(ansi)
  }).compile.drain.unsafeRunSync()

  def info(msg: String): Ansi =
    Ansi.ansi()
      .cursor(5, 0)
      .scrollUp(1)
      .eraseLine()
      .a(msg)

  def handleKeypress(k: Either[Operation, String]): Option[GameState => GameState] =
    k match {
      case Right("q") | Left(Operation.VI_EOF_MAYBE) =>
        None
      // Left arrow
      case Left(Operation.BACKWARD_CHAR) =>
        Some { g =>
          val pos0 = g.pos
          g.copy(pos = (pos0._1 - 1, pos0._2))
        }
      // Right arrow
      case Left(Operation.FORWARD_CHAR) =>
        Some { g =>
          val pos0 = g.pos
          g.copy(pos = (pos0._1 + 1, pos0._2))
        }
      // Down arrow
      case Left(Operation.NEXT_HISTORY) =>
        Some { g =>
          val pos0 = g.pos
          g.copy(pos = (pos0._1, pos0._2 + 1))
        }
      // Up arrow
      case Left(Operation.PREVIOUS_HISTORY) =>
        Some(identity)
      case _ =>
        // println(k)
        Some(identity)
    }

  def drawGame(g: GameState): Ansi =
    (for {
      _ <- Draw.drawBox(2, 6, 20, 6)
      _ <- Draw.drawBlock(g.pos._1, g.pos._2)
      _ <- Draw.drawText(2, 12, "press 'q' to quit")
    } yield ()).run(Ansi.ansi()).value._1
}
