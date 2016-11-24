package TextMUD

import akka.actor.ActorRef

class Mage extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {

  }

  val name = "Mage"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 15

  val hlthInc = 10

}