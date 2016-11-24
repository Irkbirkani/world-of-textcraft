package TextMUD

import akka.actor.ActorRef

class Warrior extends Class {

  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    //        if (in.startsWith("stun") 
  }
  val name = "Warrior"

  val stamina = 125

  val classPower = 100

  val dmgReduc = 25

  val hlthInc = 25

}