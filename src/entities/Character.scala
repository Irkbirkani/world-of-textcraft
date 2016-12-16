package entities

import akka.actor.ActorRef
import room._
import mud.Main
import mud.ActivityManager._

object Character {
  import entities.Player._

  case object ProcessInput
  def processInput(pl: Player, pla: ActorRef) = {
    if (pl.input.ready() && !pl.stunned) {
      val in = pl.input.readLine().trim
      if (in.nonEmpty) {
        pl.processCommand(in, pla)
      }
    }
  }

  case class AddToInventory(item: Option[Item])
  def addToInv(item: Option[Item], pl: Player) = {
    item match {
      case Some(item) =>
        pl.addToInventory(item)
        if (!pl.sneaking) pl.location ! Room.SayMessage("picked up " + item.name + ".", pl.name)
        else pl.output.println("You grabbed " + item.name + "!")
      case None =>
        pl.output.println("Item not found")
    }
  }

  case class TakeExit(dir: Option[ActorRef])
  def takeExit(dir: Option[ActorRef], pl: Player, pla: ActorRef) = {
    dir match {
      case Some(dest) =>
        if (pl.location != null) pl.location ! Room.LeaveRoom(pla, pl.makeFstCap(pl.name), pl.sneaking)
        pl.newLoc(dest)
        pl.changeLoc(pla, dest)
        pl.location ! Room.EnterRoom(pla, pl.makeFstCap(pl.name), pl.sneaking)
        pl.location ! Room.PrintDescription(pla)
      case None =>
        pl.output.println("You can't go that way")
    }
  }

  case class KillCmnd(victim: ActorRef)
  def killCmnd(c: ActorRef, pl: Player, pla: ActorRef) = {
    pl.setVictim(Some(c))
    if (pl.sneaking) {
      pla ! Unsneak
    } else if (pl.victim.get == pla) {
      pl.output.println("You cannot kill yourself.")
      pl.setVictim(None)
    } else if (pl.party.contains(c)) {
      pl.output.println("You cannot attack a party member.")
      pl.setVictim(None)
    } else {
      pl.output.println("You are hitting " + pl.makeFstCap(c.path.name))
      Main.activityManager ! Enqueue(pl.speed, AttackNow, pla)
    }
  }

  case object AttackNow
  def attack(pl: Player) = {
    if (pl.isAlive && !pl.stunned) {
      pl.victim.foreach(c => c ! SendDamage(pl.location, pl.damage))
    }
  }

  case class SendDamage(loc: ActorRef, dmg: Double)
  def sendDmg(loc: ActorRef, dmg: Double, pl: Player, sender: ActorRef, pla: ActorRef) = {
    if (loc == pl.location) {
      val realDamage = pl.takeDamage(dmg)
      sender ! DamageTaken(realDamage, pl.isAlive, pl.health.toInt)
      pl.output.println(pl.makeFstCap(sender.path.name) + " dealt " + realDamage + " damage! Health is at " + pl.health)
      if (!pl.isAlive) {
        pl.clearInventory
        pl.location ! Room.HasDied(pla, pl.name)
        sender ! ResetVictim
        sender ! SendExp(pl.pvpXP)
        pl.setVictim(None)
        Main.activityManager ! Enqueue(50, ResetChar, pla)
      } else if (pl.victim.isEmpty) {
        pl.setVictim(Some(sender))
        Main.activityManager ! Enqueue(pl.speed, AttackNow, pla)
      }
    } else {
      sender ! PrintMessage("You are having a hard time finding them.")
    }
  }

  case class DamageTaken(dmg: Double, alive: Boolean, Health: Int)

  def dmgTaken(dmg: Double, alive: Boolean, hp: Int, pl: Player, pla: ActorRef) = {
    if (alive && pl.victim.nonEmpty) {
      pl.output.println("You dealt " + dmg + " damage to " + pl.makeFstCap(pl.victim.get.path.name) + "! " +
        pl.makeFstCap(pl.victim.get.path.name) + " has " + hp + " health left!")
      pl.kill(pl.victim.get.path.name, pla)
    } else if (pl.victim.nonEmpty) {
      pl.output.println("you killed " + pl.victim.get.path.name + ".")
      pl.setVictim(None)
    }
  }

  case object ResetChar
  def resetChar(pl: Player, pla: ActorRef) = {
    pl.resetHlth
    pl.setAlive
    pl.setVictim(None)
    Main.roomManager ! RoomManager.EnterRoom("FirstRoom", pla)
  }
  val armorReduc = 0.1
  //Exit CCs

  //view CCs
  case class View(name: ActorRef)
  case object Stats
  //cooldown
  case class Cooldown(cd: Int)
  //kill CCs
  case class SendExp(xp: Int)
  case object ResetVictim
  //heal CCs
  case class HealCmnd(player: ActorRef)
  case class SendHeal(c: ActorRef)
  case class ReceiveHeal(hl: Int)
  def receiveHeal(hl: Int, pl: Player, sender: ActorRef) = {
    pl.addHlth(hl)
    pl.output.println("Healed for " + hl + "!")
    sender ! PrintMessage("Healed " + pl.makeFstCap(pl.name) + " for " + hl + "!")
  }
  //stun/feeze CCs
  case class StunCmnd(victim: ActorRef)
  case class SendStun(ar: ActorRef)
  case class Stun(victim: ActorRef)
  def charStun(c: ActorRef, pl: Player, pla: ActorRef) = {
    pl.setStun
    Main.activityManager ! Enqueue(30, Unstun(c), pla)
    pl.output.println("You've been stunned!")
  }
  case class Unstun(victim: ActorRef)
  def unstun(c: ActorRef, pl: Player, pla:ActorRef) = {
    pl.setStun
    pl.output.println("You're no longer stunned!")
    pl.kill(c.path.name, pla)
  }
  //poison CCs
  case class PoisonCmnd(victim: ActorRef)
  case class SendPoison(ar: ActorRef, dmg: Int)
  case class Poisoned(dmg: Int)
  case object Unpoison

  case class PrintMessage(msg: String)

  case class AddExp(xp: Int)

  case class SendInvite(pl: ActorRef, pt: scala.collection.mutable.Map[ActorRef, ActorRef])
  def invite(pla: ActorRef, pt: scala.collection.mutable.Map[ActorRef, ActorRef], pl: Player) = {
    if (pl.party.size <= 1) {
      pl.output.println(pl.makeFstCap(pla.path.name) + " invited you to a group. y/_")
      val in = pl.input.readLine
      if ("yes".startsWith(in)) {
        pl.output.println("You joined the group")
        pla ! AcceptInvite(pla, pl.location)
        pl.addParty(pt)
      } else pla ! PrintMessage(pl.name + " declined your invatation.")
    } else pla ! PrintMessage(pl.name + " is already in a group!")
  }

  case class AcceptInvite(pl: ActorRef, loc: ActorRef)
  def acceptInvite(pla: ActorRef, loc: ActorRef, pl: Player) = {
    pl.party.filter(p => p._1 != pl && p._1 != pla).foreach(p => p._1 ! AddMember(pla, loc))
    pl.output.println(pl.makeFstCap(pla.path.name) + " joined the group.")
    pl.addMem(pla, loc)
  }
  case class AddMember(pl: ActorRef, loc: ActorRef)
  def addMember(pla: ActorRef, loc: ActorRef, pl: Player) = {
    pl.addMem(pla, loc)
    pl.output.println(pl.makeFstCap(pla.path.name) + " joined the group.")

  }
  case class ChangeLoc(pl: ActorRef, loc: ActorRef)
  def newLocation(pla: ActorRef, loc: ActorRef, pl: Player) = {
    pl.setLoc(pla, loc)
  }
  case class RemoveMember(pl: ActorRef)
  def removeMember(pla: ActorRef, pl: Player) = {
    pl.rmvMember(pla)
    pl.output.println((pl.makeFstCap(pla.path.name) + " left the group."))

  }

}