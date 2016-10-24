package TextMUD

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
  val system = ActorSystem("Main")
  val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")

  implicit val ec = system.dispatcher
  def checkConnections(): Unit = {
    val ss = new ServerSocket(4445)
    while (true) {
      val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("What is your name? \nNo spaces. Letters only.")
        val name = in.readLine()
        playerManager ! PlayerManager.NewPlayer(name, "FirstRoom", List(), in, out, sock)
      }
    }
  }
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckInput)
  checkConnections()
}