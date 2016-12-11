package entities

import adts.MutableDLList
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import classes.Class
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
      context.children.foreach(_ ! Player.ProcessInput)
    case NewPlayer(name, clas, lvl, health, loc, inv, in, out, sock) =>
      if (context.child(name.toUpperCase()).nonEmpty) {
        out.println("Name taken.")
        Main.makePlayer(in, out, sock)
      } else {
        val p = context.actorOf(Props(new Player(name, clas, lvl, health, inv, in, out, sock)), name.toUpperCase())
        Main.roomManager ! RoomManager.EnterRoom(loc, p)
      }
    case PrintShoutMessage(msg, name) =>
      context.children.foreach(_ ! Player.PrintMessage(s"${RESET}${RED}$name shouts: $msg${RESET}"))
    case PrintTellMessage(to, from, msg) =>
      context.child(to) match {
        case Some(pl) => pl ! Player.PrintMessage(s"${RESET}${BLUE}$from: $msg${RESET}")
        case None => sender ! Player.PrintMessage(s"Player $to does not exist.")
      }
    case CheckPlayerExist(pl, pt) =>
      context.child(pl.toUpperCase) match {
        case Some(p) =>
          if (p != sender) p ! Player.SendInvite(sender, pt)
          else sender ! Player.PrintMessage("You cannot invite yourself to a party.")
        case None => sender ! Player.PrintMessage(s"Player $pl does not exist.")
      }
  }
}

object PlayerManager {
  //Player Management
  case object CheckInput
  case class NewPlayer(name: String, clas: Class, level: Int, health: Double, location: String, inventory: MutableDLList[Item], input: BufferedReader, output: PrintStream, sock: Socket)

  //Messaging Management
  case class PrintShoutMessage(msg: String, name: String)
  case class PrintTellMessage(to: String, from: String, msg: String)
  case class CheckPlayerExist(pl: String, party: scala.collection.mutable.Map[ActorRef, ActorRef])
}