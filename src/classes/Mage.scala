package classes

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import entities.Player
import mud.Main
import room.RoomManager

class Mage extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("teleport")) teleport(in.drop(9), pl, pla)
    else pla ! Player.PrintMessage("What?")

  }

  def teleport(dest: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 10) pla ! Player.PrintMessage("Level not heigh ehough to teleport!")
    else Main.roomManager ! RoomManager.CheckExists(dest, pla)
  }

  val abilityPower = 3
  val abilitySpeed = 15
  val abilities = Map("Teleport: teleport to a specific room" -> 10)

  val name = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15

  val hlthInc = 10

}