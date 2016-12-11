package room

import adts.{ BSTMap, MutableDLList }
import akka.actor.Actor
import akka.actor.actorRef2Scala
import akka.actor.ActorRef
import scala.Console._
import entities.{ Player, NPC }
import entities.{ Character, Item }

class Room(
    keyword: String,
    val name: String,
    val description: List[String],
    private var _items: MutableDLList[Item],
    private val exits: List[String]) extends Actor {

  import Room._
  import Character._

  def receive = {
    case PrintDescription =>
      sender ! Player.PrintMessage(printDescription)
    //Exit Management
    case GetExit(dir) =>
      sender ! TakeExit(getExit(dir))
    case LinkRooms(rooms) =>
      actorExits = exits.map(e => if (e.isEmpty) None else Some(rooms(e.toUpperCase)))
      sender ! RoomManager.LinkingRooms(name, exits)
    //Item Management
    case GetItem(name, itemName) =>
      sender ! Player.AddToInventory(getItem(itemName))
    case DropItem(name, item) =>
      dropItem(item)
    //Player Management
    case EnterRoom(pl, name, stlth) =>
      if (!stlth) {
        chars.foreach(p => p._1 ! Player.PrintMessage(name + " entered the room."))
      }
      addPlayer(pl, stlth)
    case LeaveRoom(pl, name, stlth) =>
      if (!stlth) {
        chars.foreach(p => p._1 ! Player.PrintMessage(name + " left the room."))
      }
      removePlayer(pl)
    case LeaveGame(pl, name) =>
      chars.foreach(p => p._1 ! Player.PrintMessage(makeFstCap(name) + " left the game."))
      removePlayer(pl)
    case Unstealth(pl) =>
      _chars.find(_._1 == pl) match {
        case Some(p) =>
          p._2 == false
        case None =>
          println("None called on Room.Unstealth")
      }
    case HasDied(pl, name) =>
      chars.foreach(p => p._1 ! Player.PrintMessage(name + " has died!"))
      removePlayer(pl)
    //Messages  
    case SayMessage(msg, name) =>
      chars.foreach(p => p._1 ! Player.PrintMessage(s"${RESET}${MAGENTA}$name: $msg${RESET}"))
    case CheckInRoom(cmd, pl, ar) =>
      val ch = chars.filter(c => c._1.path.name == pl).map(x => x._1)
      if (ch.length == 0) {
        ar ! Player.PrintMessage("Invalid Target")
      } else cmd match {
        case "kill" =>
          ar ! KillCmnd(ch(0))
        case "view" =>
          ar ! View(ch(0))
        case "heal" =>
          ar ! HealCmnd(ch(0))
        case "stun" =>
          ar ! StunCmnd(ch(0))
        case "poison" =>
          ar ! PoisonCmnd(ch(0))
        case _ =>
          ar ! Player.PrintMessage("Unknown command")
          println("Unknown command sent.")
      }

  }

  //Print Description
  def printDescription(): String = {
    { RESET } + { CYAN } + name + "\r\n" + description.map(a => a + "\r\n").mkString +
      s"${RESET}${GREEN}You see: \r\n" +
      { GREEN } + (if (items.length == 0) "nothing" else (for (i <- 0 until items.length) yield (items(i).name)).mkString("\r\n")) + "\r\n========" +
      { RESET } + { YELLOW } + "\r\nPlayers in room: \r\n" + chars.filter(_._2 == false).map(n => makeFstCap(n._1.path.name)).mkString("\r\n") + "\r\n========== " + { RESET }
  }

  def makeFstCap(name: String): String = {
    name.substring(0, 1).toUpperCase() + name.substring(1).toLowerCase()
  }

  //Room Exit Management
  def getExit(dir: Int): Option[ActorRef] = {
    actorExits(dir)
  }

  private var actorExits: List[Option[ActorRef]] = List.fill(6)(None)

  //Room Player Management
  private var _chars: List[(ActorRef, Boolean)] = List()

  def chars = _chars

  def addPlayer(char: ActorRef, stlth: Boolean): Unit = _chars = (char, stlth) :: chars

  def removePlayer(char: ActorRef): Unit = _chars = _chars.filter(_._1 != char)

  //Room Item Management
  def items = _items

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name == itemName) match {
      case Some(item) => {
        _items = _items.filter(it => !it.eq(item))
        Some(item)
      }
      case None =>
        None
    }
  }

  def dropItem(item: Item): Unit = {
    _items += item
  }
}

object Room {
  //Description
  case object PrintDescription
  //Inventory Management
  case class GetItem(name: String, itemName: String)
  case class DropItem(name: String, item: Item)
  //Exit Management
  case class GetExit(dir: Int)
  case class LinkRooms(rooms: BSTMap[String, ActorRef])
  //Room Character Management
  case class EnterRoom(p: ActorRef, name: String, stealthed: Boolean)
  case class LeaveRoom(p: ActorRef, name: String, stealthed: Boolean)
  case class Unstealth(p: ActorRef)
  case class HasDied(p: ActorRef, name: String)
  case class LeaveGame(p: ActorRef, name: String)
  //Messaging
  case class SayMessage(msg: String, name: String)
  //Combat Management
  case class CheckInRoom(cmd: String, pl: String, ar: ActorRef)

  def apply(n: xml.Node): Room = {
    val keyword = (n \ "@keyword").text
    val name = (n \ "@name").text
    val description = (n \ "description").text.trim.split("""\.""").toList
    (n \ "npcs").map { npc => NPC(npc) }
    val item = new MutableDLList[Item]()
    (n \ "item").map { inode => Item(inode) }.toList.foreach(_ +=: item)
    val exits = (n \ "exits").text.split(",").padTo(6, "").toList
    new Room(keyword, name, description, item, exits)
  }
}