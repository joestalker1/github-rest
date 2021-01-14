package petprj

import cats.Applicative
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

//Application errors
sealed trait AppError extends Product with Serializable {
  val msg: String
}

case class ConfigError(msg: String) extends AppError

case class InvalidJson(msg: String) extends AppError

case class NotFound(msg: String) extends AppError

case class RateLimit(msg: String) extends AppError

object AppError {
  implicit val appErrorDecoder: Decoder[AppError] = deriveDecoder[AppError]

  implicit def appErrorEntityDecoder[F[_] : Sync]: EntityDecoder[F, AppError] = jsonOf

  implicit val appErrorEncoder: Encoder[AppError] = deriveEncoder[AppError]

  implicit def appErrorEntityEncoder[F[_] : Applicative]: EntityEncoder[F, AppError] = jsonEncoderOf

}
