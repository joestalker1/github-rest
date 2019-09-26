package petprj

import cats.Applicative
import cats.effect.Sync
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

/**
  * Domain
  *
  * @param token
  */
case class GitToken(token: String) extends Product with Serializable

sealed trait DomainObj extends Product with Serializable

case class OrganizationContributor(name: String, contributions: Int) extends DomainObj

case class RepoContributor(login: String, id: Int, contributions: Int, `type`: String) extends DomainObj

case class OrganizationRepo(id: Int, name: String, contributorsUrl: String) extends DomainObj

// json encoder/decoder
trait DomainObjJson {
  implicit def domainObjListDecoder[T <: DomainObj : Decoder]: Decoder[List[T]] = (c: HCursor) => {
    // hand-written decoder to fix the invalid request body when get Git response
    // TODO to replace it by deriveDecder[..]
    val ifObjList = c.values.fold(Nil: List[Result[T]]) { jsons =>
      jsons.map(json => json.as[T]).toList
    }
    Right(ifObjList.collect {
      case Right(obj) => obj
    })
  }

}

object OrganizationContributor extends DomainObjJson {
  implicit val orgContributorDecoder: Decoder[OrganizationContributor] = (c: HCursor) => {
    for {
      name <- c.downField("login").as[String]
      contributions <- c.downField("contributions").as[Int]
    } yield OrganizationContributor(name, contributions)
  }

  implicit def orgContributorEntityDecoder[F[_] : Sync]: EntityDecoder[F, OrganizationContributor] = jsonOf

  implicit val orgContributorEncoder: Encoder[OrganizationContributor] = deriveEncoder[OrganizationContributor]

  implicit def orgContributorEntityEncoder[F[_] : Applicative]: EntityEncoder[F, OrganizationContributor] = jsonEncoderOf

  implicit val orgContributorListDecoder: Decoder[List[OrganizationContributor]] = domainObjListDecoder[OrganizationContributor]

  implicit def orgContributorListEntityDecoder[F[_] : Sync]: EntityDecoder[F, List[OrganizationContributor]] = jsonOf

  implicit val orgContributorListEncoder: Encoder[List[OrganizationContributor]] = deriveEncoder[List[OrganizationContributor]]

  implicit def orgContributorListEntityEncoder[F[_] : Applicative]: EntityEncoder[F, List[OrganizationContributor]] = jsonEncoderOf

}

object RepoContributor extends DomainObjJson {
  implicit val repoContributorDecoder: Decoder[RepoContributor] = (c: HCursor) => {
    for {
      login <- c.downField("login").as[String]
      id <- c.downField("id").as[Int]
      contributions <- c.downField("contributions").as[Int]
      tp <- c.downField("type").as[String]
    } yield RepoContributor(login, id, contributions, tp)
  }


  implicit def repoContributorEntityDecoder[F[_] : Sync]: EntityDecoder[F, RepoContributor] = jsonOf

  implicit val repoContributorEncoder: Encoder[RepoContributor] = deriveEncoder[RepoContributor]

  implicit def repoContributorEntityEncoder[F[_] : Applicative]: EntityEncoder[F, RepoContributor] = jsonEncoderOf

  implicit val repoContributorListDecoder: Decoder[List[RepoContributor]] = domainObjListDecoder[RepoContributor]

  implicit def repoContributorListEntityDecoder[F[_] : Sync]: EntityDecoder[F, List[RepoContributor]] = jsonOf

  implicit val repoContributorListEncoder: Encoder[List[RepoContributor]] = deriveEncoder[List[RepoContributor]]

  implicit def repoContributorListEntityEncoder[F[_] : Applicative]: EntityEncoder[F, List[RepoContributor]] = jsonEncoderOf

}

object OrganizationRepo extends DomainObjJson {
  implicit val orgReposDecoder: Decoder[OrganizationRepo] = (c: HCursor) => {
    for {
      id <- c.downField("id").as[Int]
      name <- c.downField("name").as[String]
      contributorUrl <- c.downField("contributors_url").as[String]
    } yield OrganizationRepo(id, name, contributorUrl)
  }

  implicit def orgReposEntityDecoder[F[_] : Sync]: EntityDecoder[F, OrganizationRepo] = jsonOf

  implicit val orgReposEncoder: Encoder[OrganizationRepo] = deriveEncoder[OrganizationRepo]

  implicit def orgReposEntityEncoder[F[_] : Applicative]: EntityEncoder[F, OrganizationRepo] = jsonEncoderOf

  implicit val orgReposListDecoder: Decoder[List[OrganizationRepo]] = domainObjListDecoder[OrganizationRepo]

  implicit def orgReposListEntityDecoder[F[_] : Sync]: EntityDecoder[F, List[OrganizationRepo]] = jsonOf

  implicit val orgReposListEncoder: Encoder[List[OrganizationRepo]] = deriveEncoder[List[OrganizationRepo]]

  implicit def orgReposListEntityEncoder[F[_] : Applicative]: EntityEncoder[F, List[OrganizationRepo]] = jsonEncoderOf

}
