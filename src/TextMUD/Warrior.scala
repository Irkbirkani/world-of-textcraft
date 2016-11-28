package TextMUD

import akka.actor.ActorRef

class Warrior extends Class {

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("stun")) {
      stun(pl, in.drop(5), pla)
    } else pla ! Player.PrintMessage("What?")
  }

  def stun(pl: Player, nm: String, pla: ActorRef) = {
    if (pl.level < 1) pla ! Player.PrintMessage("Level too low to use stun!")
    else pl.location ! Room.CheckInRoom("stun", nm, pla)
  }

  val abilityPower = 3
  val abilitySpeed = 20
  val abilities = Map("Stun" -> 3)

  val name = "Warrior"

  val stamina = 125

  val classPower = 100

  val dmgReduc = 25

  val hlthInc = 25
}
