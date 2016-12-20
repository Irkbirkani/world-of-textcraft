package entities

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import room.Room
import Character._
import akka.actor.Actor
import mud._
import java.io.PrintStream
import adts.MutableDLList
import java.net.Socket
import java.io.BufferedReader

class Priest(
    name: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)

  import Priest._

  def receive = {
    case ProcessInput => processInput(this, self, newMem)
    case PrintMessage(msg) => output.println(msg)
    case AddToInventory(item) => addToInv(item, this)
    case TakeExit(dir) => takeExit(dir, this, self)
    case KillCmnd(c) => killCmnd(c, this, self)
    case AttackNow(send) => attack(this, send)
    case SendDamage(loc, dmg, send) => sendDmg(loc, dmg, this, self, sender)
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
    case Stun(c) =>
      charStun(c, this, self)
    case Unstun(c) =>
      unstun(c, this, self)
    case HealCmnd(pl) =>
      if (healCD) {
        output.println("Heal on Cooldown")
      } else {
        output.println("Healing " + pl.path.name)
        healCD = true
        Main.activityManager ! ActivityManager.Enqueue(abilitySpeed, SendHeal(pl), self)
        Main.activityManager ! ActivityManager.Enqueue(50, HealCD, self)
      }
    case SendHeal(c) =>
      val healAmnt = level * abilityPower
      c ! ReceiveHeal(healAmnt)
    case ReceiveHeal(hl) =>
      receiveHeal(hl, this, sender)
    case HealCD =>
      healCD = false
  }

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("heal")) heal(pl, in.drop(5), pla)
    else pla ! PrintMessage("What?")

  }

  var healCD = false
  def heal(pl: Player, nm: String, pla: ActorRef) = {
    if (pl.level < 3) {
      pla ! PrintMessage("Level too low to use heal!")
    } else pl.location ! Room.CheckInRoom("heal", nm, pla)
  }

  val abilitySpeed = 20
  val abilityPower = 3
  val abilities = Map("Heal: heal a target" -> 3)

  val className = "Priest"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 10
  val startHealth = 110

}

object Priest {

  val startHealth = 110

  case object HealCD

}