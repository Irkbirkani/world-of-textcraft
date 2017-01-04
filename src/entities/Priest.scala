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
    password: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    input: BufferedReader,
    output: PrintStream,
    sock: Socket) extends Player(name, password, _level, _health, _inventory, input, output, sock) with Actor {

  addMem(self, location)

  import Priest._

  def receive = {
    case SetMode(mode) => changeMode(mode)
    case CheckStamina => checkStamina(this)
    case ProcessInput => processInput(this, self)
    case CheckPass(pass, in, out, sock) => checkPass(pass, this, self, in, out, sock)
    case EnterGame(loc) => enterGame(loc, this, self)
    case PrintMessage(msg) => output.println(msg)
    case AddToInventory(item) => addToInv(item, this)
    case TakeExit(dir, dist) => takeExit(dir, this, self, dist)
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
    case Stun(c) => charStun(c, this, self)
    case Unstun(c) => unstun(c, this, self)
    case SetTransDest(dest) => transDest = dest
    case DOTCmnd(victim, dotType) =>
      dotCmnd(victim, this, self, 20, dotType)
      mending = Some(victim)
    case DOTNow(victim, dotType) => dotNow(victim, this, self, (level * abilityPower), dotType)
    case SendDOT(dmg, dotType, send) => sendDOT(dmg, this, self, dotType, send)
    case DOTTaken(dmg, alive, health, dotType, vic) => dotTaken(dmg, alive, health, dotType, this, vic)
    case CheckDOT =>
      mending match {
        case Some(target) =>
          if (mendCD) Main.activityManager ! ActivityManager.Enqueue(20, DOTNow(target, "mend"), self)
        case None =>
      }
    case ResetDOT(dotType) => mending = None
    case HealCmnd(pl) =>
      if (healCD) {
        output.println("Heal on Cooldown")
      } else {
        output.println("Healing " + pl.path.name)
        healCD = true
        Main.activityManager ! ActivityManager.Enqueue(abilitySpeed, SendHeal(pl), self)
        Main.activityManager ! ActivityManager.Enqueue(50, HealCD, self)
      }
    case SendHeal(c) => c ! ReceiveHeal(healAmnt)
    case ReceiveHeal(hl) => receiveHeal(hl, this, sender)
    case HealCD => healCD = false
    case MendCD => mendCD = false
  }

  val abilitySpeed = 20
  val abilityPower = 3

  val className = "Priest"

  val maxStamina = 100
  var _stamina = maxStamina
  def stamina = _stamina

  val classPower = 150

  val dmgReduc = 10
  val startHealth = 110
  
  val classArmor = Item.cloth
  val classWeapons = List("")


  var transDest = ""

  def classCommands(in: String) = {
    if (in.startsWith("heal")) heal(in.drop(5))
    else if (in.startsWith("mend")) mend(in.drop(5))
    else output.println("What?")

  }

  var healCD = false
  val healAmnt = level * abilityPower * abilityPower
  def heal(nm: String) = {
    if (level < 3) output.println("Level too low to use Heal!")
    else location ! Room.CheckInRoom("heal", nm, self)
  }

  var mending: Option[ActorRef] = None
  var mendCD = false
  def mend(nm: String) = {
    if (level < 5) output.println("Level too low to use Mend!")
    else {
      location ! Room.CheckInRoom("mend", nm, self)
      mendCD = true
      Main.activityManager ! ActivityManager.Enqueue(150, MendCD, self)
    }
  }

  val abilities = Map("Heal: heal a target for " + healAmnt + ". Cooldown: 5 Seconds." -> 3,
    "Mend: mend your targets wounds for " + (level * abilityPower) + " every 2 seconds for 10 seconds. Cooldown: 15 Seconds" -> 5)

}

object Priest {

  val startHealth = 110

  case object HealCD
  case object MendCD

}