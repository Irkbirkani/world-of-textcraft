package entities

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import mud._
import room.RoomManager
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Actor
import adts.MutableDLList
import java.net.Socket
import Character._

class Mage(
    name: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)
  import Mage._

  def receive = {
    case ProcessInput => processInput(this, self, newMem)
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
    case Invite(pl) =>
      invite(pl, this)
    case InviteAccepted(accept, pla) =>
      inviteAccpt(accept, pla, this, self)
    case AddToParty(pt, snder) =>
      addToParty(pt, this, self, snder)
    case UpdateParty(pl, loc) =>
      updateParty(pl, loc, this)
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
    case StartTeleport(rm) =>
      if (teleCD) {
        output.println("Teleport on cooldown!")
      } else {
        output.println("Teleporting to " + rm + "!")
        teleCD = true
        Main.activityManager ! ActivityManager.Enqueue(100, Teleport(rm), self)
        Main.activityManager ! ActivityManager.Enqueue(600, TeleCD, self)
      }
    case Teleport(rm) =>
      Main.roomManager ! RoomManager.EnterRoom(rm, self)
    case TeleCD =>
      teleCD = false
  }

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("teleport")) teleport(in.drop(9), pl, pla)
    else pla ! PrintMessage("What?")

  }

  def teleport(dest: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 10) pla ! PrintMessage("Level not heigh ehough to teleport!")
    else Main.roomManager ! RoomManager.CheckExists(dest, pla)
  }

  var teleCD = false

  val abilityPower = 3
  val abilitySpeed = 15
  val abilities = Map("Teleport: teleport to a specific room" -> 10)

  val className = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15
  val startHealth = 110

}

object Mage {

  val startHealth = 110

  case class StartTeleport(rm: String)
  case class Teleport(rm: String)
  case object TeleCD
}