package petprj

import cats.effect.{ConcurrentEffect, Sync}
import cats.implicits._
import io.circe.Decoder
import org.http4s._
import org.http4s.{Method, Request, Uri}
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl

import GitServices._

trait GitService[F[_]] {


  /**
    * Look up organization contributors who contribute to organization repos.
    * First request repos list that belong to the organization, then we request for each repos contributors list.
    *
    * @param organizationName
    * @return contributors ordered by contributions in ascending order
    */
  def findOrganizationContributors(organizationName: String): F[GitResp[OrganizationContributor]]
}


object GitServices {
  type GitResp[R] = Either[AppError, List[R]]

  /**
    * Create [[GitService]] implementation
    *
    * @param appConfig
    * @param client
    * @tparam F
    * @return
    */
  def service[F[_] : ConcurrentEffect](appConfig: AppConfig, client: Client[F]): GitService[F] = new GitService[F] {
    val dsl = new Http4sClientDsl[F] {}

    import dsl._

    /**
      * Look up organization contributors.First request repo list.
      *
      * @param organizationName
      * @return contributors ordered by contributions in ascending order
      */
    override def findOrganizationContributors(organizationName: String): F[GitResp[OrganizationContributor]] = {
      //find organization repos
      findOrganizationRepos(organizationName).flatMap {
        case Right(orgRepos) => if (orgRepos.isEmpty) (Right(Nil): GitResp[OrganizationContributor]).pure[F]
        else {
          // having list organization repos let's get repo contributors
          val repoContributors = findRepoContributors(organizationName, orgRepos)

          repoContributors.map { case listContribsOr =>
            for {
              contribs <- listContribsOr
            } yield {
              val pairOfLoginFrequency = contribs.foldLeft(List.empty[(String, Int)]) { (acc, repoContributor) =>
                 (repoContributor.login -> 1) :: acc
              }
              val loginToFreq = pairOfLoginFrequency.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2).sum)
              val orgContributors = loginToFreq.map(kv => OrganizationContributor(kv._1, kv._2))
              orgContributors.toList.sortBy(_.contributions)
            }
          }

        }
        case Left(appError) => (Left(appError): GitResp[OrganizationContributor]).pure[F]
      }
    }


    //helpers
    //run loading data from Git
    private def load[T <: DomainObj : Decoder](loader: Int => F[GitResp[T]])(page: Int, res: List[T]): F[GitResp[T]] = {
      loader(page).flatMap {
        case Right(newObjs) => if (newObjs.nonEmpty) {
          load(loader)(page + 1, newObjs ++ res)
        } else (Right(res): Either[AppError, List[T]]).pure[F]
        case err: Left[AppError, List[T]] => (err: GitResp[T]).pure[F]
      }
    }

    /**
      * Find organization repos. Run requesting repos sequenced to get all page by page.
      *
      * @param organizationName
      * @return
      */
    private def findOrganizationRepos(organizationName: String): F[GitResp[OrganizationRepo]] = {
      //if Git returns empty list it stop requesting a new page.
      load[OrganizationRepo](findOrganizationRepos(organizationName, _))(1, Nil)
    }

    /**
      * Find repo contributors.Run requesting data concurrently for every organization repos.
      * Use fiber abstraction to run every task in configured execution contexts.
      *
      * @param organizationName
      * @param repos
      * @return
      */
    private def findRepoContributors(organizationName: String, repos: List[OrganizationRepo]): F[GitResp[RepoContributor]] = {
      val fibers = for {
        repo <- repos
        fiber = ConcurrentEffect[F].start(load[RepoContributor](findRepoContributors(organizationName, repo.name, _))(1, Nil))
      } yield fiber
      fibers.sequence.flatMap {
        listFibers =>
          //use monoid for Fiber
          listFibers.combineAll.join //wait for result
      }
    }

    /**
      * Make http request to Git to get organization repos
      *
      * @param organizationName
      * @param page
      * @return
      */
    private def findOrganizationRepos(organizationName: String, page: Int): F[GitResp[OrganizationRepo]] = {
      val url = Uri.uri("https://api.github.com/orgs") / organizationName / "repos"
      val request = getRequest(page, url)

      client.fetch(request) {
        case Status.Successful(r) => r.as[List[OrganizationRepo]].map(b => Right(b))
        case Status.NotFound(r) => r.as[String].map(b => Left(NotFound(b)))
        case Status.Forbidden(r) => r.as[String].map(b => Left(RateLimit(b)))
      }
    }

    /**
      * Create HTTP request by injecting tokens in headers
      *
      * @param page
      * @param url
      * @return
      */
    private def getRequest(page: Int, url: Uri): Request[F] = {
      Request[F](
        Method.GET,
        url.withQueryParam("page", page),
        headers = Headers.of(Header("Authorization", s"token ${appConfig.gitToken}"),
          Header("User-Agent", "gist"),
          Header("Content-Type", "application/json"))
      )
    }

    /**
      * Make HTTP request to Git to get repo contributors
      *
      * @param organizationName
      * @param repoName
      * @param page
      * @return
      */
    private def findRepoContributors(organizationName: String, repoName: String, page: Int): F[GitResp[RepoContributor]] = {
      val url = Uri.uri("https://api.github.com/repos") / organizationName / repoName / "contributors"
      val request = getRequest(page, url)
      client.fetch(request) {
        case Status.Successful(r) => r.as[List[RepoContributor]].map(b => Right(b))
        case Status.NotFound(r) => r.as[String].map(b => Left(NotFound(b)))
        case Status.Forbidden(r) => r.as[String].map(b => Left(RateLimit(b)))
      }

    }
  }
}
