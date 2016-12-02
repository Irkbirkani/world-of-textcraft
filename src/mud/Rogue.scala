package mud

import akka.actor.ActorRef

class Rogue extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("poison")) poison(in.drop(7), pl, pla)
    else if (in.startsWith("sneak")) pla ! Player.Sneak
    else pla ! Player.PrintMessage("What?")
  }

  def poison(vc: String, pl: Player, pla: ActorRef) = {
    if (pl.level < 1000) pla ! Player.PrintMessage("Level too low to use Poison!")
    else pl.location ! Room.CheckInRoom("poison", vc, pla)
  }

  val abilityPower = 2
  val abilitySpeed = 15
  val abilities = Map("Sneak: become invisible for 1 minute" -> 1)//, "Poison" -> 3)

  val name = "Rogue"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 20

  val hlthInc = 15
}