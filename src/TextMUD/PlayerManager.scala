package TextMUD

import akka.actor.Actor
import java.io.PrintStream
import akka.actor.ActorRef
import java.io.InputStream
import akka.actor.Props
import java.io.BufferedReader
import java.net.Socket

class PlayerManager extends Actor {
  import PlayerManager._
  //Actor Management
  def receive = {
    case CheckInput =>
      context.children.foreach(_ ! Player.ProcessInput)
    case NewPlayer(name, loc, inv, in, out, sock) =>
      if (context.child(name).nonEmpty) {
        out.println("Name taken.")
        sock.close
      } else {
        val p = context.actorOf(Props(new Player(name, inv, in, out, sock)), name)
        Main.roomManager ! RoomManager.EnterRoom(loc, p)
      }
    case PrintShoutMessage(msg, name) =>
      context.children.foreach(_ ! Player.PrintMessage(s"$name shouts: $msg"))
    case PrintTellMessage(to, from, msg) =>
      var found = false
      for (c <- context.children; if c.path.name.toLowerCase() == to.toLowerCase()) {
        c ! Player.PrintMessage(s"$from: $msg")
        sender ! Player.PrintMessage(s"You: $msg")
        found = true
      }
      if (!found) sender ! Player.PrintMessage(s"Player $to does not exist.")
  }
}

object PlayerManager {
  //Tells each player to check for input
  case object CheckInput
  //Creates new player.
  case class NewPlayer(name: String, location: String, inventory: MutableDLList[Item], input: BufferedReader, output: PrintStream, sock: Socket)
  //Messaging Management
  case class PrintShoutMessage(msg: String, name: String)
  case class PrintTellMessage(to: String, from: String, msg: String)
}