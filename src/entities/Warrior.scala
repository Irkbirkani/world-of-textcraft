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
    name: String,
    password: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, password, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)

  import Warrior._

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
    case DOTCmnd(victim, dotType) =>
      dotCmnd(victim, this, self, 20, dotType)
      cutting = Some(victim)
    case DOTNow(victim, dotType) => dotNow(victim, this, self, (level * abilityPower), dotType)
    case SendDOT(dmg, dotType, send) => sendDOT(dmg, this, self, dotType, send)
    case DOTTaken(dmg, alive, health, dotType, vic) => dotTaken(dmg, alive, health, dotType, this, vic)
    case CheckDOT =>
      cutting match {
        case Some(target) =>
          if (cutCD) Main.activityManager ! ActivityManager.Enqueue(20, DOTNow(target, "cut"), self)
        case None =>
      }
    case ResetDOT(dotType) => cutting = None
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
    case SendStun(c) => c ! Stun(self)
    case Stun(c) => charStun(c, this, self)
    case Unstun(c) => unstun(c, this, self)
    case StunCD => stunCD = false
  }

  def classCommands(in: String) = {
    if (in.startsWith("stun")) {
      stun(in.drop(5))
    } else output.println("What?")
  }

  var stunCD = false
  def stun(nm: String) = {
    if (level < 3) output.println("Level too low to use stun!")
    else location ! Room.CheckInRoom("stun", nm, self)
  }

  var cutting: Option[ActorRef] = None
  var cutCD = false
  def cut(nm: String) = {
    if (level < 5) output.println("Level too low to use Cut!")
    else location ! Room.CheckInRoom("cut", nm, self)
  }

  val abilityPower = 3
  val abilitySpeed = 20
  val abilities = Map("Stun: stun your target for 3 seconds." -> 3)

  val className = "Warrior"

  val stamina = 125

  val classPower = 100

  val dmgReduc = 25
  val startHealth = 125

}

object Warrior {
  val startHealth = 125

  case object StunCD
}
