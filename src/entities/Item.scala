package entities

case class Item(name: String, description: String, damage: Int, speed: Int, armor: Int, food: Double, itype: (String, String), wepType: String) {

}

object Item {
  def apply(n: xml.Node): Item = {
    val slot = (n \ "@slot").text
    val itype = (n \ "@itype").text

    new Item((n \ "@name").text,
      n.text,
      (if (slot == weapon && itype != offHand) (n \ "@damage").text.toInt else 0),
      (if (slot == weapon && itype != offHand) (n \ "@speed").text.toInt else 0),
      (if (armor.contains(slot) || slot == offHand) (n \ "@armor").text.toInt else 0),
      (if (slot == food) (n \ "@food").text.toDouble else 0),
      (if (slot != food || slot != misc) (slot, itype) else ("", "")),
      (if (slot == weapon) (n \ "@wepType").text else ""))
  }

  val weapon = "weapon"
  val head = "head"
  val chest = "chest"
  val legs = "legs"
  val armor = List(head, chest, legs)

  val cloth = "cloth"
  val leather = "leather"
  val plate = "plate"
  val hand = "hand"
  val twoHand = "twoHand"
  val offHand = "offHand"

  val sword = "sword"
  val wand = "wand"
  val dagger = "dagger"
  val staff = "staff"
  val hammer = "hammer"
  val twoHSword = "2Hsword"

  val shield = "shield"
  val book = "book"

  val misc = "misc"
  val food = "food"
}
case class EquippedItem(slot: String, itype: String, item: Item)