package entities

import adts.MutableDLList
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import Character.{ PrintMessage, Invite, ProcessInput }
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import mud.Main
import room._
import scala.Console._

class PlayerManager extends Actor {
  import PlayerManager._
  //Actor Management
  def receive = {
    case CheckInput =>
      context.children.foreach(_ ! ProcessInput)
    case NewPlayer(clas, name, pass, lvl, health, loc, inv, in, out, sock) =>
      if (context.child(name.toUpperCase()).nonEmpty) {
        out.println("Name taken.")
        Main.makePlayer(in, out, sock)
      } else {
        clas match {
          case "WARRIOR" =>
            val p = context.actorOf(Props(new Warrior(name, pass, lvl, health, inv, in, out, sock)), name.toUpperCase())
            Main.roomManager ! RoomManager.EnterRoom(loc, p)
          case "MAGE" =>
            val p = context.actorOf(Props(new Mage(name, pass, lvl, health, inv, in, out, sock)), name.toUpperCase())
            Main.roomManager ! RoomManager.EnterRoom(loc, p)
          case "ROGUE" =>
            val p = context.actorOf(Props(new Rogue(name, pass, lvl, health, inv, in, out, sock)), name.toUpperCase())
            Main.roomManager ! RoomManager.EnterRoom(loc, p)
          case "PRIEST" =>
            val p = context.actorOf(Props(new Priest(name, pass, lvl, health, inv, in, out, sock)), name.toUpperCase())
            Main.roomManager ! RoomManager.EnterRoom(loc, p)
        }

      }
    case PrintShoutMessage(msg, name) =>
      context.children.foreach(_ ! PrintMessage(s"${RESET}${RED}$name shouts: $msg${RESET}"))
    case PrintTellMessage(to, from, msg) =>
      context.child(to.toUpperCase()) match {
        case Some(pl) => pl ! PrintMessage(s"${RESET}${BOLD}${BLUE}$from: $msg${RESET}")
        case None => sender ! PrintMessage(s"Player $to does not exist.")
      }
    case CheckPlayerExist(pl, pt, pla) =>
      context.child(pl.toUpperCase) match {
        case Some(p) =>
          if (p != pla) p ! Invite(pla)
          else pla ! PrintMessage("You cannot invite yourself to a party.")
        case None => pla ! PrintMessage(s"Player $pl does not exist.")
      }
    case FindChild(name, pass, in, out, sock) =>
      findChild(name, pass, in, out, sock)
  }

  def findChild(child: String, pass: String, in: BufferedReader, out: PrintStream, sock: Socket) = {
    context.child(child.toUpperCase) match {
      case Some(child) =>
        child ! Character.CheckPass(pass, in, out, sock)
      case None =>
        out.println("Invalid Username. Try again.")
        Main.startScreen(in, out, sock)

    }
  }
}

object PlayerManager {
  //Player Management
  case object CheckInput
  case class NewPlayer(clas: String, name: String, pass: String, level: Int, health: Double, location: String, inventory: MutableDLList[Item], input: BufferedReader, output: PrintStream, sock: Socket)
  case class FindChild(name: String, pass: String, in: BufferedReader, out: PrintStream, sock: Socket)

  //Messaging Management
  case class PrintShoutMessage(msg: String, name: String)
  case class PrintTellMessage(to: String, from: String, msg: String)
  case class CheckPlayerExist(pl: String, party: scala.collection.mutable.Map[ActorRef, ActorRef], pla: ActorRef)
}