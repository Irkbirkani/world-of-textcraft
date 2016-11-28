package TextMUD

import akka.actor.ActorRef

class Mage extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("teleport")) teleport(in.drop(9), pl, pla)
    else pla ! Player.PrintMessage("What?")

  }

  def teleport(dest: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 1) pla ! Player.PrintMessage("Level not heigh ehough to teleport!")
    else Main.roomManager ! RoomManager.CheckExists(dest, pla)
  }

  val abilityPower = 3
  val abilitySpeed = 15
  val abilities = Map("Teleport" -> 10)

  val name = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15

  val hlthInc = 10

}