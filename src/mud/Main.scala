package mud
import io.StdIn._
import java.io.PrintStream
import java.io.InputStream
import akka.actor.ActorSystem
import akka.actor.Props
import scala.concurrent.duration._
import scala.concurrent.Future
import java.net.ServerSocket
import java.io.BufferedReader
import java.io.InputStreamReader

object Main extends App {
  //Initializes system, Player Manager, and Room Manager
  val system = ActorSystem("Main")
  val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
  val npcManager = system.actorOf(Props[NPCManager], "NPCManager")
  val activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")

  //Checks and schedules checks for connections
  implicit val ec = system.dispatcher
  def checkConnections(): Unit = {
    val ss = new ServerSocket(4445)
    while (true) {
      val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("What is your name?\r\nNo spaces. Letters only.")
        var name = (in.readLine().trim).filter(x => x.isLetter || x.isWhitespace).replaceAll(" ", "_")

        out.println("Choose a Class.\r\nWarrior\r\nMage\r\nRogue\r\nPriest")
        var clas = in.readLine().trim.toUpperCase()
        var cls: Class = null
        while (clas.trim.toUpperCase != "WARRIOR"
          && clas.trim.toUpperCase != "MAGE"
          && clas.trim.toUpperCase != "ROGUE"
          && clas.trim.toUpperCase != "PRIEST") {
          out.println(clas.toLowerCase() + " is not a class. Try again.")
          clas = in.readLine().trim.toUpperCase()
        }
        clas match {
          case "WARRIOR" => cls = new Warrior
          case "MAGE" => cls = new Mage
          case "ROGUE" => cls = new Rogue
          case "PRIEST" => cls = new Priest
          case _ =>
            out.println("Try again.")
            clas = in.readLine.trim.toUpperCase()
        }
        playerManager ! PlayerManager.NewPlayer(name, cls, Player.startLvl, Player.playerHealth, "FirstRoom", new MutableDLList[Item](), in, out, sock)
      }
    }
  }
  system.scheduler.schedule(0.seconds, 0.1.seconds, activityManager, ActivityManager.CheckQueue)
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckInput)
  checkConnections()

}