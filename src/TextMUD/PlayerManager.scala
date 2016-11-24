package TextMUD

import akka.actor.Actor
import java.io.PrintStream
import akka.actor.ActorRef
import java.io.InputStream
import akka.actor.Props
import java.io.BufferedReader
import java.net.Socket
import scala.Console._

class PlayerManager extends Actor {
  import PlayerManager._
  //Actor Management
  def receive = {
    case CheckInput =>
      context.children.foreach(_ ! Player.ProcessInput)
    case NewPlayer(name, clas, lvl, health, loc, inv, in, out, sock) =>
      if (context.child(name).nonEmpty) {
        out.println("Name taken.")
        sock.close
      } else {
        val p = context.actorOf(Props(new Player(name, clas, lvl, health, inv, in, out, sock)), name)
        Main.roomManager ! RoomManager.EnterRoom(loc, p)
      }
    case PrintShoutMessage(msg, name) =>
      context.children.foreach(_ ! Player.PrintMessage(s"${RESET}${RED}$name shouts: $msg${RESET}"))
    case PrintTellMessage(to, from, msg) =>
      var found = false
      for (c <- context.children; if c.path.name.toLowerCase() == to.toLowerCase()) {
        c ! Player.PrintMessage(s"${RESET}${BLUE}$from: $msg${RESET}")
        sender ! Player.PrintMessage(s"${RESET}${BLUE}You: $msg${RESET}")
        found = true
      }
      if (!found) sender ! Player.PrintMessage(s"Player $to does not exist.")
  }
}

object PlayerManager {
  //Player Management
  case object CheckInput
  case class NewPlayer(name: String, clas: Class, level: Int, health: Double, location: String, inventory: MutableDLList[Item], input: BufferedReader, output: PrintStream, sock: Socket)

  //Messaging Management
  case class PrintShoutMessage(msg: String, name: String)
  case class PrintTellMessage(to: String, from: String, msg: String)
}