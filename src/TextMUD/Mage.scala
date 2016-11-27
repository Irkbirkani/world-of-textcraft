package TextMUD

import akka.actor.ActorRef

class Mage extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    pla ! Player.PrintMessage("What?")

  }

  val abilityPower = 3
  val abilitySpeed = 15
  
  val name = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15

  val hlthInc = 10

}