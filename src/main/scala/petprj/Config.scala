package petprj

import com.typesafe.config.ConfigFactory
import cats.implicits._
import scala.util.Try

//timeout in seconds
case class AppConfig(gitToken: String, host: String, port: Int, idleTimeout: Int, requestTimeout: Int, responseTimeout: Int) extends Product with Serializable

/**
  * Application config
  */
object AppConfig {
  def apply(): Either[AppError, AppConfig] = {
    val allConfigs = ConfigFactory.load()
    (Try(allConfigs.getString("git.accessToken")).toEither.left.map(_ => ConfigError("git.accessToken is not defined")),
      Try(allConfigs.getString("app.host")).toEither.left.map(_ => ConfigError("app.host is not defined")),
      Try(allConfigs.getInt("app.port")).toEither.left.map(_ => ConfigError("app.port is not defined")),
      Try(allConfigs.getInt("app.idleTimeout")).toEither.left.map(_ => ConfigError("app.idleTimeout is not defined")),
      Try(allConfigs.getInt("app.requestTimeout")).toEither.left.map(_ => ConfigError("app.requestTimeout is not defined")),
      Try(allConfigs.getInt("app.responseTimeout")).toEither.left.map(_ => ConfigError("app.responseTimeout is not defined"))).mapN(AppConfig.apply)
  }

}
