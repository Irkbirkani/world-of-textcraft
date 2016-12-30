package entities

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import mud._
import room._
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Actor
import adts.MutableDLList
import java.net.Socket
import Character._

class Mage(
    name: String,
    password: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, password, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)

  import Mage._

  def receive = {
    case SetMode(mode) => changeMode(mode)
    case ProcessInput => processInput(this, self)
    case CheckPass(pass, in, out, sock) => checkPass(pass, this, self, in, out, sock)
    case EnterGame(loc) => enterGame(loc, this, self)
    case PrintMessage(msg) => output.println(msg)
    case AddToInventory(item) => addToInv(item, this)
    case TakeExit(dir) => takeExit(dir, this, self)
    case KillCmnd(c) => killCmnd(c, this, self)
    case AttackNow(send) => attack(this, send)
    case SendDamage(loc, dmg, send) => sendDmg(loc, dmg, this, self, send)
    case DamageTaken(dmg, alive, hp) => dmgTaken(dmg, alive, hp, this, self)
    case ResetChar => resetChar(this, self)
    case ResetVictim => setVictim(None)
    case View(name) => name ! Stats
    case Stats => sender ! PrintMessage("Level: " + level + "\r\nClass: " + className)
    case SendExp(xp) => party.filter(p => p._2 == location).foreach(p => p._1 ! AddExp(xp))
    case AddExp(xp) => addExp(xp)
    case Invite(pl) => invite(pl, this)
    case InviteAccepted(accept, pla) => inviteAccpt(accept, pla, this, self)
    case AddToParty(pt, snder) => addToParty(pt, this, self, snder)
    case UpdateParty(pl, loc) => updateParty(pl, loc, this)
    case AddMember(pl, loc) => addMember(pl, loc, this)
    case ChangeLoc(pl, newL) => newLocation(pl, newL, this)
    case RemoveMember(pl) => removeMember(pl, this)
    case ReceiveHeal(hl) => receiveHeal(hl, this, sender)
    case Stun(c) => charStun(c, this, self)
    case Unstun(c) => unstun(c, this, self)
    case DOTCmnd(victim, dotType) =>
      dotCmnd(victim, this, self, 20, dotType)
      burning = Some(victim)
    case DOTNow(victim, dotType) => dotNow(victim, this, self, (level * abilityPower), dotType)
    case SendDOT(dmg, dotType, send) => sendDOT(dmg, this, self, dotType, send)
    case DOTTaken(dmg, alive, health, dotType, vic) => dotTaken(dmg, alive, health, dotType, this, vic)
    case CheckDOT =>
      burning match {
        case Some(vic) =>
          if (burnCD) Main.activityManager ! ActivityManager.Enqueue(20, DOTNow(vic, "poison"), self)
        case None =>
      }
    case ResetDOT(dotType) => burning = None
    case StartTransport(dest, self) => Main.roomManager ! RoomManager.CheckExists(dest, self, "trans")
    case CheckPlayers(dest) =>
      transDest = dest
      transporting match {
        case "party" => party.filter(m=> m._1 != self && m._2 == location).foreach { m =>
          m._1 ! PrintMessage(makeFstCap(name) + " wants to transport you to " + dest + ". y/_")
          m._1 ! SetMode(2)
          m._1 ! SetTransDest(dest)
        }
        case nme => 
           val trns = party.filter(m => m._1.path.name == nme.toUpperCase() && m._2 == location).toList
           if (trns.length == 0) output.println(makeFstCap(nme) + " is not in your party or in a different room.")
           else if (nme == name) output.println("You cannot transport your self. Use Teleport.")
           else {
             trns(0)._1 ! PrintMessage(makeFstCap(name) + " wants to transport you to " + dest + ". y/_")
             trns(0)._1 ! SetMode(2)
             trns(0)._1 ! SetTransDest(dest)
           }
      }
    case SetTransDest(dest) => transDest = dest
    case StartTeleport(rm) =>
      if (teleCD) {
        output.println("Teleport on cooldown!")
      } else {
        output.println("Teleporting to " + rm + "!")
        teleCD = true
        Main.activityManager ! ActivityManager.Enqueue(100, Teleport(rm), self)
        Main.activityManager ! ActivityManager.Enqueue(1800, TeleCD, self)
      }
    case Teleport(rm) => Main.roomManager ! RoomManager.EnterRoom(rm, self)
    case TeleCD =>
      output.println("Teleport off cooldown.")
      teleCD = false
    case BurnCD =>
      output.println("Burn off cooldown.")
      burnCD = false
    case TransCD => 
      output.println("Transport off cooldown.")
      transCD = false
  }

  def classCommands(in: String) = {
    if (in.startsWith("teleport")) teleport(in.drop(9))
    else if (in.startsWith("burn")) burn(in.drop(5))
    else if (in.startsWith("transport")) transport(in)
    else output.println("What?")

  }

  var teleCD = false

  def teleport(dest: String) = {
    if (level < 10) output.println("Level not high enough to teleport!")
    else Main.roomManager ! RoomManager.CheckExists(dest, self, "tele")
  }

  var burnCD = false
  var burning: Option[ActorRef] = None
  def burn(vic: String) = {
    if (level < 3) output.println("Level not high enough to use burn!")
    else {
      location ! Room.CheckInRoom("burn", vic, self)
      burnCD = true
      Main.activityManager ! ActivityManager.Enqueue(150, BurnCD, self)
    }
  }

  var transCD = false
  private var transporting = ""
  var transDest = ""
  def transport(in: String) = {
    if (level < 1) output.println("Level not high enough to use transport")
    else {
      val Array(_, ppl, dest) = in.split(" +", 3)
      transporting = ppl
      Main.activityManager ! ActivityManager.Enqueue(100, StartTransport(dest,self), self)
      Main.activityManager ! ActivityManager.Enqueue(1800, TransCD, self)
    }
  }

  val abilityPower = 3
  val abilitySpeed = 15
  val abilities = Map("Burn: burn someone for " + (level * abilityPower) + " every 2 seconds for 10 seconds. Cooldown: 15 Seconds." -> 3,
    "Teleport: teleport to a specific room. Cooldown: 3 Minutes." -> 10)

  val className = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15
  val startHealth = 110

}

object Mage {
  val startHealth = 110

  case object BurnCD

  case class StartTeleport(rm: String)
  case class Teleport(rm: String)
  case object TeleCD

  case class StartTransport(dest:String, self:ActorRef)
  case class CheckPlayers(dest: String)
  case object TransCD
}