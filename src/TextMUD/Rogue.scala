package TextMUD

import akka.actor.ActorRef

class Rogue extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
      pla ! Player.PrintMessage("What?")
  }

  val abilityPower = 2
  val abilitySpeed = 15
  
  val name = "Rogue"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 20

  val hlthInc = 15
}