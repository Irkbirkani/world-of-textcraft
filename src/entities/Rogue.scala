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
import mud.ActivityManager.Enqueue

class Rogue(
    name: String,
    password: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, password, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)

  import Rogue._

  def receive = {
    case ProcessInput => processInput(this, self, newMem)
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
      poisoning = Some(victim)
    case DOTNow(victim, dotType) => dotNow(victim, this, self, (level * abilityPower), dotType)
    case SendDOT(dmg, dotType, send) => sendDOT(dmg, this, self, dotType, send)
    case DOTTaken(dmg, alive, health, dotType, vic) => dotTaken(dmg, alive, health, dotType, this, vic)
    case CheckDOT =>
      poisoning match {
        case Some(vic) =>
          if (poisonCD) Main.activityManager ! Enqueue(20, DOTNow(vic, "poison"), self)
        case None =>
      }
    case ResetDOT(dotType) => poisoning = None
    case PoisonCD =>
      output.println("Poison off cooldown.")
      poisonCD = false
    case Sneak =>
      if (sneakCD) {
        output.println("Sneak on cooldown.")
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
    case SneakCD =>
      sneakCD = false
      output.println("Sneak off cooldown.")

  }

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("poison")) poison(in.drop(7), pl, pla)
    else if (in.startsWith("sneak")) pla ! Sneak
    else pla ! PrintMessage("What?")
  }

  def poison(vc: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 1) pla ! PrintMessage("Level too low to use Poison!")
    else {
      pl.location ! Room.CheckInRoom("poison", vc, pla)
      poisonCD = true
      Main.activityManager ! Enqueue(100, PoisonCD, self)
    }
  }

  private var sneakCD = false

  private var poisonCD = false
  private var poisoning: Option[ActorRef] = None

  val abilityPower = 2
  val abilitySpeed = 15
  val abilities = Map("Sneak: become invisible for 1 minute" -> 1, "Poison: poison someone for " + (level * abilityPower) + " every 2 seconds for 10 seconds" -> 3)

  val className = "Rogue"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 20
  val startHealth = 115

}

object Rogue {

  val startHealth = 115

  case object Sneak
  case object Unsneak
  case object SneakCD

  case object PoisonCD
}