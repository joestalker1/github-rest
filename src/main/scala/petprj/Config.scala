package petprj

import com.typesafe.config.ConfigFactory
import cats.implicits._
import scala.util.Try

//timeout in seconds
case class AppConfig(gitToken: String, host: String, port: Int,
                     idleTimeoutInSec: Int, requestTimeoutInSec: Int, responseTimeoutInSec: Int) extends Product with Serializable

/**
  * Application config
  */
object AppConfig {
  def apply(): Either[AppError, AppConfig] = {
    val allConfigs = ConfigFactory.load()
    (Try(allConfigs.getString("git.accessToken")).toEither.left.map(_ => ConfigError("git.accessToken is not defined")),
      Try(allConfigs.getString("app.host")).toEither.left.map(_ => ConfigError("app.host is not defined")),
      Try(allConfigs.getInt("app.port")).toEither.left.map(_ => ConfigError("app.port is not defined")),
      Try(allConfigs.getInt("app.idleTimeoutInSec")).toEither.left.map(_ => ConfigError("app.idleTimeoutInSec is not defined")),
      Try(allConfigs.getInt("app.requestTimeoutInSec")).toEither.left.map(_ => ConfigError("app.requestTimeoutInSec is not defined")),
      Try(allConfigs.getInt("app.responseTimeoutInSec")).toEither.left.map(_ => ConfigError("app.responseTimeoutInSec is not defined"))).mapN(AppConfig.apply)
  }

}
