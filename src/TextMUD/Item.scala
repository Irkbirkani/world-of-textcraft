

package TextMUD

case class Item(name: String, description: String, damage: Int, speed: Int, armor: Int, itype: String) {

}

object Item {
  def apply(n: xml.Node): Item = {
    new Item((n \ "@name").text,
      n.text, (n \ "@damage").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@itype").text)
  }
  val head = "head"
  val chest = "chest"
  val legs = "legs"
  val hand = "hand"
  val twoHand = "twoHand"
  val offHand = "offHand"
}
case class EquippedItem(bodyPart: String, item: Item)