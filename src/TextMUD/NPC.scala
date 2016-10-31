package src.TextMUD

import akka.actor.Actor

case class NPC (name:String, desc:String) extends Actor {
  def receive = {
    case _ => 
  }
  
}

object NPC {
   def apply(n: xml.Node): NPC = {
    new NPC((n \ "@name").text, n.text)
  }
}