
import java.io.File

import com.twitter.io.Reader
import com.twitter.util.Future
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.Json

import scala.sys.process._

object Main extends App {

  object Res {

    def log[T](t: T): T = { println(t); t }

    type Id = String

    def ids(): Seq[Id] =
      Seq("docker", "ps", "--all", "--format", "{{.ID}}").!!.split("\n").toSeq

    case class Container(
      id: Id,
      name: String,
      service: Option[String],
      running: Boolean,
      links: Seq[Id],
      volumesFrom: Seq[Id],
      inspect: Json
    )

    case class DockerContainerLight(
      Id: Id,
      Name: String
    )
    case class DockerContainer(
      Id: Id,
      Name: String,
      State: DockerContainerState,
      NetworkSettings: DockerNetworkSettings,
      HostConfig: DockerHostConfig,
      Config: DockerConfig
    )
    case class DockerContainerState(
      Running: Boolean
    )
    case class DockerNetworkSettings(
      Networks: Map[String, DockerNetwork]
    )
    case class DockerNetwork(
      Links: Option[Seq[String]]
    )
    case class DockerHostConfig(
      VolumesFrom: Seq[String]
    )
    case class DockerConfig(
      Labels: Map[String, String]
    )

    def inspect(): Seq[Container] = {
      val cmd = Seq("docker", "inspect") ++ ids()
      val inspect = parse(cmd.!!).valueOr(throw _)
      val cs = inspect.as[Seq[DockerContainerLight]].leftMap(log).getOrElse(Seq())
      val nameToId = cs.groupBy(_.Name.replaceAll("^/","")).mapValues(_.head.Id)

      inspect.asArray.getOrElse(List()).map { json =>
        val c = json.as[DockerContainer].valueOr(throw _)
        val lnkNames = for {
          net <- c.NetworkSettings.Networks.values.toSeq
          lnks <- net.Links.toSeq
          lnk <- lnks
        } yield lnk.takeWhile(_ != ':')

        Container(
          id = c.Id,
          name = c.Name.replaceAll("^/", ""),
          service = c.Config.Labels.get("com.docker.compose.service"),
          running = c.State.Running,
          links = lnkNames.map(nameToId).distinct,
          volumesFrom = c.HostConfig.VolumesFrom.map(_.takeWhile(_ != ':')),
          inspect = json
        )
      }
    }
  }

  import io.finch._
  import io.finch.circe._
  import com.twitter.finagle.Http
  import com.twitter.finagle.http.Response
  import com.twitter.util.Await

  object files {
    def file(path: String, contentType: String = "text/html"): Future[Response] =
      Reader.readAll(Reader.fromFile(new File(path))).map { content =>
        val rep = Response()
        rep.content = content
        rep.contentType = contentType
        rep
      }

    val page: Endpoint[Response] = get(/) { file("page.html") }
  }

  object ids {
    case class Ids(ids: Seq[String])

    val ep: Endpoint[Ids] = get("api" :: "ids") {
      Ok(Ids(Res.ids()))
    }
  }

  object nodes {
    case class Nodes(nodes: Seq[Node])
    case class Node(data: NodeData, classes: String)
    case class NodeData(id: String, label:  String, info: Res.Container)

    val ep: Endpoint[Nodes] = get("api" :: "nodes") {
      def cls(c: Res.Container) = if (c.running) "running" else ""
      val nodeList = Res.inspect()
        .map(c => Node(NodeData(c.id, c.service.getOrElse(c.name), c), cls(c)))
      Ok(Nodes(nodeList))
    }
  }

  object edges {
    case class Edges(edges: Seq[Edge])
    case class Edge(data: EdgeData, classes: String)
    case class EdgeData(source: String, target: String)

    val ep: Endpoint[Edges] = get("api" :: "edges") {
      val cs = Res.inspect()
      val links = for {
        src <- cs; dst <- src.links
      } yield Edge(EdgeData(src.id, dst), "")
      val vols = for {
        src <- cs; dst <- src.volumesFrom
      } yield Edge(EdgeData(src.id, dst), "volumes_from")
      Ok(Edges(links ++ vols))
    }
  }

  Await.ready(
    Http.server.serve(":9999", (files.page :+: ids.ep :+: nodes.ep :+: edges.ep).toServiceAs[Application.Json])
  )

}
