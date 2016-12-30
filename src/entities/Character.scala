package entities

import akka.actor.ActorRef
import room._
import mud.Main
import mud.ActivityManager._
import java.io.PrintStream
import java.net.Socket
import java.io.BufferedReader

object Character {
  import Player._

  case class CheckPass(pass: String, in: BufferedReader, out: PrintStream, sock: Socket)

  def checkPass(pass: String, pl: Player, pla: ActorRef, in: BufferedReader, out: PrintStream, sock: Socket) = {
    if (pass == pl.password) {
      pl.setInput(in)
      pl.setOutput(out)
      pl.setSock(sock)
      println(pl.location.path.name)
      pl.output.println("Login Successful.")
      pla ! EnterGame(pl.location)
      println(pla.path.name)
    } else {
      out.println("Invalid password. Try again.")
      Main.startScreen(in, out, sock)
    }
  }

  //Inpuut case classes
  case class SetMode(mode: Int)
  
  case object ProcessInput
  def processInput(pl: Player, self: ActorRef) = {
    if (pl.input.ready() && !pl.stunned) {
      var in = pl.input.readLine()
      if (in.startsWith("\t")) in = "\t" + in.trim else in = in.trim
      pl.mode match {
        case 0 =>
          if (in.nonEmpty) {
            pl.processCommand(in, self)
          }
        case 1 =>
          if (in.startsWith("y")) {
            pl.newMem ! InviteAccepted(true, self)
            pl.setNewMem(null)
            pl.changeMode(0)
          } else {
            pl.newMem ! InviteAccepted(false, self)
            pl.setNewMem(null)
            pl.changeMode(0)
          }
        case 2 =>
          if ("yes".startsWith(in)) {
            Main.roomManager ! RoomManager.EnterRoom(pl.transDest, self)
            pl.transDest = ""
            pl.changeMode(0)
          } else {
            pl.transDest = ""
            pl.changeMode(0)
          }
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

  //Exit case classes
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

  case class EnterGame(loc: ActorRef)
  def enterGame(loc: ActorRef, pl: Player, pla: ActorRef) = {
    pl.location ! Room.EnterRoom(pla, pl.makeFstCap(pl.name), pl.sneaking)
    pl.location ! Room.PrintDescription(pla)
  }

  // Basic combat case classes
  case class KillCmnd(victim: ActorRef)

  def killCmnd(c: ActorRef, pl: Player, pla: ActorRef) = {
    pl.setVictim(Some(c))
    if (pl.sneaking) {
      pla ! Rogue.Unsneak
    } else if (pl.victim.get == pla) {
      pl.output.println("You cannot kill yourself.")
      pl.setVictim(None)
    } else if (pl.party.contains(c)) {
      pl.output.println("You cannot attack a party member.")
      pl.setVictim(None)
    } else {
      pl.output.println("You are hitting " + pl.makeFstCap(c.path.name))
      Main.activityManager ! Enqueue(pl.speed, AttackNow(pla), pla)
    }
  }

  case class AttackNow(sender: ActorRef)

  def attack(pl: Player, sender: ActorRef) = {
    if (pl.isAlive && !pl.stunned) {
      pl.victim.foreach(c => c ! SendDamage(pl.location, pl.damage, sender))
    }
  }

  case class SendDamage(loc: ActorRef, dmg: Double, sender: ActorRef)

  def sendDmg(loc: ActorRef, dmg: Double, pl: Player, pla: ActorRef, sender: ActorRef) = {
    if (loc == pl.location) {
      val realDamage = pl.takeDamage(dmg)
      sender ! DamageTaken(realDamage, pl.isAlive, pl.health.toInt)
      pl.output.println(pl.makeFstCap(sender.path.name) + " dealt " + realDamage + " damage! Health is at " + (if (pl.health <= 0) 0 else pl.health))
      if (!pl.isAlive) {
        pl.clearInventory
        pl.location ! Room.HasDied(pla, pl.name)
        sender ! ResetVictim
        sender ! SendExp(pl.pvpXP)
        pl.setVictim(None)
        Main.activityManager ! Enqueue(50, ResetChar, pla)
      } else if (pl.victim.isEmpty) {
        pl.setVictim(Some(sender))
        Main.activityManager ! Enqueue(pl.speed, AttackNow(pla), pla)
      }
    } else {
      pl.setVictim(None)
      pla ! PrintMessage("You are having a hard time finding them.")
    }
  }

  case class DamageTaken(dmg: Double, alive: Boolean, Health: Int)

  def dmgTaken(dmg: Double, alive: Boolean, hp: Int, pl: Player, pla: ActorRef) = {
    if (alive && pl.victim.nonEmpty) {
      pl.output.println("You dealt " + dmg + " damage to " + pl.makeFstCap(pl.victim.get.path.name) + "! " +
        pl.makeFstCap(pl.victim.get.path.name) + " has " + hp + " health left!")
      pl.kill(pl.victim.get.path.name, pla)
    } else if (pl.victim.nonEmpty) {
      pl.output.println("you killed " + pl.makeFstCap(pl.victim.get.path.name) + ".")
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

  //Death case classes
  case class SendExp(xp: Int)
  case class AddExp(xp: Int)
  case object ResetVictim

  //view case classes
  case class View(name: ActorRef)
  case object Stats

  //heal case classes
  case class HealCmnd(player: ActorRef)
  case class SendHeal(c: ActorRef)
  case class ReceiveHeal(hl: Int)
  def receiveHeal(hl: Int, pl: Player, sender: ActorRef) = {
    pl.addHlth(hl)
    pl.output.println("Healed for " + hl + "!")
    sender ! PrintMessage("Healed " + pl.makeFstCap(pl.name) + " for " + hl + "!")
  }

  //stun case classes
  case class StunCmnd(victim: ActorRef)
  case class SendStun(ar: ActorRef)
  case class Stun(victim: ActorRef)
  def charStun(c: ActorRef, pl: Player, pla: ActorRef) = {
    pl.setStun
    Main.activityManager ! Enqueue(30, Unstun(c), pla)
    pl.output.println("You've been stunned!")
  }
  case class Unstun(victim: ActorRef)
  def unstun(c: ActorRef, pl: Player, pla: ActorRef) = {
    pl.setStun
    pl.output.println("You're no longer stunned!")
    pl.kill(c.path.name, pla)
  }

  //Transport case classes
  case class SetTransDest(dest:String)

  //DOT case classes
  case class DOTCmnd(victim: ActorRef, dotType: String)
  def dotCmnd(vic: ActorRef, pl: Player, pla: ActorRef, speed: Int, dotType: String) = {
    if (vic == pla) {
      pl.output.println("You cannot " + dotType + " yourself.")
    } else if (pl.party.contains(vic)) {
      pl.output.println("You cannot " + dotType + " a party member.")
    } else {
      dotType match {
        case "cut" =>
          pl.output.println("You are " + dotType + "ting " + pl.makeFstCap(vic.path.name))
        case _ =>
          pl.output.println("You are " + dotType + "ing " + pl.makeFstCap(vic.path.name))
      }
      Main.activityManager ! Enqueue(speed, DOTNow(vic, dotType), pla)
      if (!pl.sneaking) pl.kill(vic.path.name, pla)
    }
  }

  case class DOTNow(ar: ActorRef, dotType: String)
  def dotNow(vic: ActorRef, pl: Player, pla: ActorRef, dmg: Int, dotType: String) = {
    if (pl.isAlive && !pl.stunned) vic ! SendDOT(dmg, dotType, pla)
  }

  case class SendDOT(dmg: Int, dotType: String, sender: ActorRef)
  def sendDOT(dmg: Int, pl: Player, pla: ActorRef, dotType: String, sender: ActorRef) = {
    dotType match {
      case "poison" =>
        pl.rmvHlth(dmg, pla)
        sender ! DOTTaken(dmg, pl.isAlive, pl.health.toInt, dotType, pla)
        sender ! CheckDOT
        pl.output.println(pl.makeFstCap(pla.path.name) + " dealt " + dmg + " " + dotType + " " + "damage! Health is at " + pl.health)
      case "burn" =>
        pl.rmvHlth(dmg, pla)
        sender ! DOTTaken(dmg, pl.isAlive, pl.health.toInt, dotType, pla)
        sender ! CheckDOT
        pl.output.println(pl.makeFstCap(pla.path.name) + " dealt " + dmg + " fire damage! Health is at " + pl.health)
      case "cut" =>
        pl.rmvHlth(dmg, pla)
        sender ! DOTTaken(dmg, pl.isAlive, pl.health.toInt, dotType, pla)
        sender ! CheckDOT
        pl.output.println(pl.makeFstCap(pla.path.name) + " dealt " + dmg + " damage! Health is at " + pl.health)
      case "mend" =>
        pl.addHlth(dmg)
        sender ! DOTTaken(dmg, pl.isAlive, pl.health.toInt, dotType, pla)
        sender ! CheckDOT
        pl.output.println(pl.makeFstCap(pla.path.name) + " healed you for" + dmg + "! Health is at " + pl.health)
    }
    if (!pl.isAlive) {
      pl.clearInventory
      pl.location ! Room.HasDied(pla, pl.name)
      pla ! ResetVictim
      pla ! SendExp(pl.pvpXP)
      pla ! ResetDOT(dotType)
      Main.activityManager ! Enqueue(50, ResetChar, pla)
    }
  }

  case class DOTTaken(dmg: Int, alive: Boolean, health: Int, dotType: String, vic: ActorRef)
  def dotTaken(dmg: Int, alive: Boolean, health: Int, dotType: String, pl: Player, vic: ActorRef) = {
    dotType match {
      case "poison" =>
        if (alive) {
          pl.output.println("You dealt " + dmg + " " + dotType + " " + "damage to " + pl.makeFstCap(vic.path.name) + "!")
        } else pl.output.println("You killed " + pl.makeFstCap(vic.path.name) + "!")
      case "burn" =>
        if (alive) {
          pl.output.println("You dealt " + dmg + " fire damage to " + pl.makeFstCap(vic.path.name) + "!")
        } else pl.output.println("You killed " + pl.makeFstCap(vic.path.name) + "!")
      case "mend" =>
        if (alive) {
          pl.output.println("You healed " + pl.makeFstCap(vic.path.name) + " for " + dmg + "!")
        } else pl.output.println(pl.makeFstCap(vic.path.name) + " has died!")
      case "cut" =>
        if (alive) {
          pl.output.println("You dealt " + dmg + " damage to " + pl.makeFstCap(vic.path.name) + "!")
        } else pl.output.println("You killed " + pl.makeFstCap(vic.path.name) + "!")
    }
  }
  case object CheckDOT
  case class ResetDOT(dotType: String)

  // Party case classes
  case class Invite(pla: ActorRef)

  def invite(pla: ActorRef, pl: Player) = {
    if (pl.party.size <= 1) {
      pl.output.println(pl.makeFstCap(pla.path.name) + " invited you to a group. y/_")
      pl.changeMode(1)
      pl.setNewMem(pla)
    } else pla ! PrintMessage(pl.makeFstCap(pl.name) + " is already in a party.")
  }

  case class InviteAccepted(accept: Boolean, pla: ActorRef)
  def inviteAccpt(acc: Boolean, pla: ActorRef, pl: Player, self: ActorRef) = {
    if (acc) {
      pla ! AddToParty(pl.party, self)
    } else pl.output.println(pl.makeFstCap(pla.path.name) + " declined your invitation.")
  }

  case class AddToParty(pt: scala.collection.mutable.Map[ActorRef, ActorRef], sender: ActorRef)
  def addToParty(pt: scala.collection.mutable.Map[ActorRef, ActorRef], pl: Player, self: ActorRef, sender: ActorRef) = {
    pl.output.println("You joined the group")
    sender ! UpdateParty(self, pl.location)
    pl.addParty(pt)
  }

  case class UpdateParty(pl: ActorRef, loc: ActorRef)
  def updateParty(pla: ActorRef, loc: ActorRef, pl: Player) = {
    pl.party.filter(p => p._1 != pl && p._1 != pla).foreach(p => p._1 ! AddMember(pla, loc))
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

  //Miscellaneous case classes and values
  case class PrintMessage(msg: String)
}