package entities

case class Item(name: String, description: String, damage: Int, speed: Int, armor: Int, food: Double, itype: (String, String)) {

}

object Item {
  def apply(n: xml.Node): Item = {
    new Item((n \ "@name").text,
      n.text,
      (n \ "@damage").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@food").text.toDouble,
      ((n \ "@slot").text, (n \ "@itype").text))
  }

  val weapon = "weapon"
  val head = "head"
  val chest = "chest"
  val legs = "legs"
  
  
  val cloth = "cloth"
  val leather = "leather"
  val plate = "plate"
  val hand = "hand"
  val sword = "sword"
  val twoHand = "twoHand"
  val offHand = "offHand"
  
  val misc = "misc"
  val food = "food"
}
case class EquippedItem(slot: String, itype: String, item: Item)