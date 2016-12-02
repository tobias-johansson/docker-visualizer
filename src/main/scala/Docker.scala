import scala.sys.process._

import io.circe.parser._
import io.circe.generic.auto._
import io.circe.Json

object Docker {

  def log[T](t: T): T = { println(t); t }

  type Id = String

  def ids(): Seq[Id] =
    Seq("docker", "ps", "--all", "--format", "{{.ID}}").!!.split("\n").toSeq

  case class Container(
    id: Id,
    name: String,
    service: Option[String],
    image: String,
    version: String,
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
    Image: String,
    Labels: Map[String, String]
  )

  def inspect(ids: Seq[Id]): Seq[Container] = {
    val cmd = Seq("docker", "inspect") ++ ids
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
        id          = c.Id,
        name        = c.Name.replaceAll("^/", ""),
        service     = c.Config.Labels.get("com.docker.compose.service"),
        image       = c.Config.Image,
        version     = c.Config.Image.dropWhile(_ != ':').dropWhile(_ == ':'),
        running     = c.State.Running,
        links       = lnkNames.map(nameToId).distinct,
        volumesFrom = c.HostConfig.VolumesFrom.map(_.takeWhile(_ != ':')),
        inspect     = json
      )
    }
  }
}
