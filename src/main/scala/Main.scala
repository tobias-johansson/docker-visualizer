import java.io.File

import com.twitter.io.Reader
import com.twitter.util.Future
import com.twitter.finagle.Http
import com.twitter.finagle.http.Response
import com.twitter.util.Await

import io.finch._
import io.finch.circe._

import io.circe.generic.auto._

object Main extends App {

  object files {
    def file(path: String, contentType: String = "text/html"): Future[Response] = {
//      Reader.readAll(Reader.fromFile(new File(path))).map { content =>
      val stream = getClass.getResourceAsStream(path)
      Reader.readAll(Reader.fromStream(stream)).map { content =>
        val rep = Response()
        rep.content = content
        rep.contentType = contentType
        rep
      }
    }

    val page: Endpoint[Response] = get(/) { file("page.html") }
  }

  object ids {
    case class Ids(ids: Seq[String])

    val ep: Endpoint[Ids] = get("api" :: "ids") {
      Ok(Ids(Docker.ids()))
    }
  }

  object nodes {
    case class Nodes(nodes: Seq[Node])
    case class Node(data: NodeData, classes: String)
    case class NodeData(id: String, label:  String, info: Docker.Container)

    val ep: Endpoint[Nodes] = get("api" :: "nodes") {
      def cls(c: Docker.Container) = if (c.running) "running" else ""
      val nodeList = Docker.inspect(Docker.ids()).map { c =>
        val label = s"${c.service.getOrElse(c.name)}\n\n${c.version}"
        Node(NodeData(c.id, label, c), cls(c))
      }
      Ok(Nodes(nodeList))
    }
  }

  object edges {
    case class Edges(edges: Seq[Edge])
    case class Edge(data: EdgeData, classes: String)
    case class EdgeData(source: String, target: String)

    val ep: Endpoint[Edges] = get("api" :: "edges") {
      val cs = Docker.inspect(Docker.ids())
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
    Http.server.serve(":9999", (
        files.page :+:
        ids.ep :+:
        nodes.ep :+:
        edges.ep
      ).toServiceAs[Application.Json]
    )
  )

}
