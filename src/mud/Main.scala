package mud

import adts._
import akka.actor.ActorSystem
import akka.actor.Props
import entities._
import io.StdIn._
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PrintStream
import java.net.Socket
import java.net.ServerSocket
import room._
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.Console._

object Main extends App {
  //Initializes system, Player Manager, Room Manager, NPC Manager, and Activity Manager
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
      startScreen(in, out, sock)
    }
  }

  def startScreen(in: BufferedReader, out: PrintStream, sock: Socket) = {
    Future {
      out.println("Welcome to\r")
      out.println({ RESET } + { RED } +
        /**     */
        """ _    _            _     _          __   _____         _                  __ _   """ + "\r")
      out.println("""| |  | |          | |   | |        / _| |_   _|       | |                / _| |  """ + "\r")
      out.println("""| |  | | ___  _ __| | __| |   ___ | |_    | | _____  _| |_ ___ _ __ __ _| |_| |_ """ + "\r")
      out.println("""| |/\| |/ _ \| '__| |/ _` |  / _ \|  _|   | |/ _ \ \/ / __/ __| '__/ _` |  _| __|""" + "\r")
      out.println("""\  /\  / (_) | |  | | (_| | | (_) | |     | |  __/>  <| || (__| | | (_| | | | |_ """ + "\r")
      out.println(""" \/  \/ \___/|_|  |_|\__,_|  \___/|_|     \_/\___/_/\_\\__\___|_|  \__,_|_|  \__|""" + "\r\n" + { RESET })

      makePlayer(in, out, sock)

      //      out.println("It this is your first time type " + { RESET } + { BOLD } + { GREEN } + " new" + { RESET } + ".\r")
      //      out.println("If you have an account, type your username.\r")
      //      out.print("-> ")
      //
      //      var text = (in.readLine().trim).filter(x => x.isLetter).toUpperCase
      //      text match {
      //        case "NEW" => makePlayer(in, out, sock)
      //        case _ => signIn(text, in, out, sock)
      //      }
    }
  }

  def signIn(name: String, in: BufferedReader, out: PrintStream, sock: Socket) = {
    Future {
      out.print("Enter your password: ")
      var pass = in.readLine().trim
      playerManager ! PlayerManager.FindChild(name, pass, in, out, sock)
    }
  }

  def makePlayer(in: BufferedReader, out: PrintStream, sock: Socket) = {
    def nameCheck(name: String): Boolean = {
      name.length < 3 || name.contains(" ")
    }
    def passCheck(pass: String): Boolean = {
      pass.length >= 6 && pass.filter(_.isUpper).length >= 1 && pass.filter(_.isDigit).length >= 1
    }
    Future {
      out.println("What is your name?\r\n3 or more characters. Letters only.")
      out.print("-> ")
      var name = (in.readLine().trim).filter(x => x.isLetter)
      while (nameCheck(name)) {
        out.println("Invalid name. Try again.")
        out.print("-> ")
        name = (in.readLine().trim).filter(x => x.isLetter)
      }
      //      out.println("Password:\r\nPasswords must be at least six characters, have one uppercase letter, and one number..\r")
      //      out.print("-> ")
      //      var pass = in.readLine().trim
      //      while (!passCheck(pass)) {
      //        out.println("Invalid password. Try again.")
      //        out.print("-> ")
      //        pass = (in.readLine().trim)
      //      }

      out.print("Choose a Class:\r\n\n")
      out.print({ RESET } + { RED } +
        "Warrior->        Warriors are heavy hitters on the battle field.\n\r")
      out.print("                 They have high damage reduction and are at their\n\r")
      out.print("                 best in the midst of battle.\n\r\n")
      out.print({ RESET } + { CYAN } +
        "Mage->           Mages are highly intellectual. They bombard their\r\n")
      out.print("                 foes with a multitude of attacks.\n\r\n")
      out.print({ RESET } + { GREEN } +
        "Rogue->          Rogues are creatures of the dark. They sneak around\r\n")
      out.print("                 poisoning their targets and disapear into the dark.\r\n\n")
      out.print({ RESET } + { YELLOW } +
        "Priest->         Priests are pure in heart. They are able to heal their \r\n")
      out.print("                 allies and help increase their atack power.\n\r\n" + { RESET })
      out.print("-> ")

      var clas = in.readLine().trim.toUpperCase()
      while (clas.trim.toUpperCase != "WARRIOR"
        && clas.trim.toUpperCase != "MAGE"
        && clas.trim.toUpperCase != "ROGUE"
        && clas.trim.toUpperCase != "PRIEST") {
        out.println(clas.toLowerCase() + " is not a class. Try again.")
        out.print("-> ")
        clas = in.readLine().trim.toUpperCase()
      }
      clas match {
        case "WARRIOR" =>
          playerManager ! PlayerManager.NewPlayer(clas, name, "", Player.startLvl, Warrior.startHealth,
            "FirstRoom", new MutableDLList[Item],
            in, out, sock)
        case "MAGE" =>
          playerManager ! PlayerManager.NewPlayer(clas, name, "", Player.startLvl, Mage.startHealth,
            "FirstRoom", new MutableDLList[Item],
            in, out, sock)
        case "ROGUE" =>
          playerManager ! PlayerManager.NewPlayer(clas, name, "", Player.startLvl, Rogue.startHealth,
            "FirstRoom", new MutableDLList[Item],
            in, out, sock)
        case "PRIEST" =>
          playerManager ! PlayerManager.NewPlayer(clas, name, "", Player.startLvl, Priest.startHealth,
            "FirstRoom", new MutableDLList[Item],
            in, out, sock)
      }
    }
  }

  system.scheduler.schedule(0.seconds, 0.1.seconds, activityManager, ActivityManager.CheckQueue)
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckInput)
  system.scheduler.schedule(0.seconds, 2.0.seconds, playerManager, PlayerManager.CheckStam)
  checkConnections()
}