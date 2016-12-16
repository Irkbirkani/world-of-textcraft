package entities

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import room.Room
import Character._
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Actor
import adts.MutableDLList
import java.net.Socket
import mud._

class Rogue(val name: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    val input: BufferedReader,
    val output: PrintStream,
    val sock: Socket) extends Player(name, _level, _health, _inventory, input, output, sock) with Actor {

  import Rogue._

  def receive = {
    case ProcessInput => processInput(this, self)
    case PrintMessage(msg) => output.println(msg)
    case AddToInventory(item) => addToInv(item, this)
    case TakeExit(dir) => takeExit(dir, this, self)
    case KillCmnd(c) => killCmnd(c, this, self)
    case AttackNow => attack(this)
    case SendDamage(loc, dmg) => sendDmg(loc, dmg, this, sender, self)
    case DamageTaken(dmg, alive, hp) => dmgTaken(dmg, alive, hp, this, self)
    case ResetChar => resetChar(this, self)
    case ResetVictim => setVictim(None)
    case View(name) => name ! Stats
    case Stats => sender ! PrintMessage("Level: " + level + "\r\nClass: " + className)
    case SendExp(xp) => party.filter(p => p._2 == location).foreach(p => p._1 ! AddExp(xp))
    case AddExp(xp) => addExp(xp)
    case SendInvite(pl, pt) =>
      //TODO refactor to use a "flag" or "mode" system to remove blocking call.
      invite(pl, pt, this)
    case AcceptInvite(pl, loc) =>
      acceptInvite(pl, loc, this)
    case AddMember(pl, loc) =>
      addMember(pl, loc, this)
    case ChangeLoc(pl, newL) =>
      newLocation(pl, newL, this)
    case RemoveMember(pl) =>
      removeMember(pl, this)
    case ReceiveHeal(hl) =>
      receiveHeal(hl, this, sender)
    case Stun(c) =>
      charStun(c, this, self)
    case Unstun(c) =>
      unstun(c, this, self)
    case Sneak =>
      if (sneakCD) {
        output.println("Sneak on cooldown")
      } else {
        setSneak
        sneakCD = true
        output.println("You are sneaking!")
        Main.activityManager ! ActivityManager.Enqueue(600, Unsneak, self)
        Main.activityManager ! ActivityManager.Enqueue(600, SneakCD, self)
      }
    case Unsneak =>
      if (sneaking) output.println("You are no longer sneaking!")
      setSneak
      location ! Room.Unstealth(self)
  }

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("poison")) poison(in.drop(7), pl, pla)
    else if (in.startsWith("sneak")) pla ! Player.Sneak
    else pla ! PrintMessage("What?")
  }

  def poison(vc: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 1000) pla ! PrintMessage("Level too low to use Poison!")
    else pl.location ! Room.CheckInRoom("poison", vc, pla)
  }

  val abilityPower = 2
  val abilitySpeed = 15
  val abilities = Map("Sneak: become invisible for 1 minute" -> 1) //, "Poison" -> 3)

  val className = "Rogue"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 20

  val hlthInc = 15
}

object Rogue {

  case object Sneak
  case object Unsneak

  case object SneakCD
}