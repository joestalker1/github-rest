package petprj

import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import cats.effect._
import cats.implicits._
import fs2._
import AppError._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

/**
  * Web service serves the endpoint: /org/<organizationName>/contributors
  */
object WebServer {
  /**
    * Run web service that listen to client requests on configured host and port and make dependency injection to Blaze HTTP server.
    * Set up huge request and response timeout to avoid HTTP client stopping request processing.
    * First create HTTP client, then GIT service and Blaze HTTP server by injecting this dependcies.
    *
    * @param appConfig
    * @param Effect
    * @param T
    * @tparam F
    * @return
    */
  def serve[F[_]](appConfig: AppConfig)(implicit Effect: ConcurrentEffect[F], T: Timer[F]): Stream[F, ExitCode] =
    for {
      client <- Stream.resource(BlazeClientBuilder[F](global).withIdleTimeout(appConfig.idleTimeout seconds)
        .withRequestTimeout(appConfig.requestTimeout seconds).withResponseHeaderTimeout(appConfig.responseTimeout seconds).resource)
      gitService = GitServices.service[F](appConfig, client)
      webService = routes[F](gitService)
      exitCode <- BlazeServerBuilder[F].bindHttp(appConfig.port, appConfig.host).withIdleTimeout(appConfig.idleTimeout seconds)
        .withResponseHeaderTimeout(appConfig.responseTimeout seconds)
        .withHttpApp(webService.orNotFound)
        .serve
    } yield exitCode

  /**
    * Endpoint definitions. We match GET request URL to configured endpoints here.
    *
    * @param gitService
    * @tparam F
    * @return
    */
  def routes[F[_] : Sync](gitService: GitService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "org" / organizationName / "contributors" =>
        gitService.findOrganizationContributors(organizationName).flatMap {
          case Right(contributors) => Ok(contributors.pure[F])
          case Left(appError) => NotFound(appError.pure[F])
        }
    }
  }
}