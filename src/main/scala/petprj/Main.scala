package petprj

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

//main entry point to application
object Main extends IOApp {
  override def run(args: List[String]) = {
    val ifConfig = AppConfig.apply()
    ifConfig.fold(_ => ExitCode.Error.pure[IO], WebServer.serve[IO](_).compile.drain.as(ExitCode.Success))
  }
}

