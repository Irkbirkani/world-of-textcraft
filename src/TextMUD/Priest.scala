package TextMUD

import akka.actor.ActorRef

class Priest extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("heal")) heal(pl, in.drop(5), pla)
    else pla ! Player.PrintMessage("What?")

  }

  def heal(pl: Player, nm: String, pla: ActorRef) = {
    if (pl.level < 5) {
      pla ! Player.PrintMessage("Your level isn't high enough for that ability!")
    } else if (pl.level >= 5) {
      pl.location ! Room.CheckInRoom("heal", nm, pla)
    }
  }

  val abilitySpeed = 20
  val abilityPower = 3

  val name = "Priest"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 10

  val hlthInc = 10

}