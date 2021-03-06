package room

import adts.BSTMap
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import akka.actor.Props
import entities.{ Character, Player }

class RoomManager extends Actor {
  import RoomManager._
  import entities.Character.PrintMessage
  import entities.Mage._
  //Actor Management
  def receive = {
    case EnterRoom(loc, p) => p ! Character.TakeExit(Some(rooms(loc.toUpperCase)),0)
    case LinkingRooms(key, exits) => roomExits += (key -> exits)
    case ShortPath(curr, dest) =>
      val path = shortestPath(curr, dest, roomExits, List())
      path.foreach(a => if (a.nonEmpty) sender ! PrintMessage(a))
    case CheckExists(rm, ar, cmd) =>
      if (!rooms.contains(rm.toUpperCase)) {
        ar ! PrintMessage(rm + " is not a room!")
      } else cmd match {
        case "tele" => ar ! StartTeleport(rm.filter(x => !x.isWhitespace))
        case "trans" => ar ! CheckPlayers(rm.filter(x => !x.isWhitespace))
        case _ =>
      }
  }
  private def comp(s1: String, s2: String): Int = {
    if (s1 == s2) 0
    else if (s1 > s2) 1
    else -1
  }
  val rooms = new BSTMap[String, ActorRef](comp)
  (xml.XML.loadFile("map.xml") \ "room").map { n =>
    val key = (n \ "@keyword").text
    rooms += key.toUpperCase -> context.actorOf(Props(Room(n)), key)
  }

  private var roomExits: Map[String, List[String]] = Map()
  context.children.foreach(_ ! Room.LinkRooms(rooms))
  private val dirs = "north south east west up down".split(" ")

  def shortestPath(curr: String, dest: String, exitsMap: Map[String, List[String]], visited: List[String]): List[String] = {
    val newVisited = curr :: visited
    if (curr == dest) newVisited.reverse
    else {
      val path = for ((i, d) <- exitsMap(curr) zip dirs; if (i.trim.nonEmpty && !newVisited.contains(i))) yield {
        shortestPath(i, dest, exitsMap, d :: newVisited)
      }
      path.filter(_.nonEmpty) match {
        case Nil => Nil
        case lst => lst.minBy(_.length)
      }
    }
  }
}

object RoomManager {
  //Puts char in a room
  case class EnterRoom(loc: String, p: ActorRef)
  case class CheckExists(rm: String, ar: ActorRef, cmd: String)
  case class LinkingRooms(key: String, exits: List[String])
  case class ShortPath(curr: String, dest: String)

}
