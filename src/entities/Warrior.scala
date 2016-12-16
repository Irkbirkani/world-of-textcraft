package entities

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import entities.Character._
import entities.Player._
import room.Room
import java.io.PrintStream
import adts.MutableDLList
import java.net.Socket
import java.io.BufferedReader
import akka.actor.Actor
import mud._

class Warrior(
    val name: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    val input: BufferedReader,
    val output: PrintStream,
    val sock: Socket) extends Player(name, _level, _health, _inventory, input, output, sock) with Actor {

  import Warrior._

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
    case StunCmnd(c) =>
      setVictim(Some(c))
      if (victim.get == self) {
        output.println("You cannot stun yourself.")
        setVictim(None)
      } else if (stunCD) {
        output.println("Stun is on cooldown.")
      } else {
        output.println("You stunned " + makeFstCap(c.path.name))
        Main.activityManager ! ActivityManager.Enqueue(speed, SendStun(victim.get), self)
        stunCD = true
        Main.activityManager ! ActivityManager.Enqueue(80, StunCD, self)
        kill(victim.get.path.name, self)
      }
    case SendStun(c) =>
      c ! Stun(self)
    case Stun(c) =>
      charStun(c, this, self)
    case Unstun(c) =>
      unstun(c, this, self)
    case StunCD =>
      stunCD = false
  }

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("stun")) {
      stun(pl, in.drop(5), pla)
    } else pla ! PrintMessage("What?")
  }

  var stunCD = false

  def stun(pl: Player, nm: String, pla: ActorRef) = {
    if (pl.level < 3) pla ! PrintMessage("Level too low to use stun!")
    else pl.location ! Room.CheckInRoom("stun", nm.toUpperCase(), pla)
  }

  val abilityPower = 3
  val abilitySpeed = 20
  val abilities = Map("Stun: stun your target for 3 seconds." -> 3)

  val className = "Warrior"

  val stamina = 125

  val classPower = 100

  val dmgReduc = 25

  val hlthInc = 25
}

object Warrior {

  case object StunCD
}
