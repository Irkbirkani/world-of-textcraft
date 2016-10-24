

package TextMUD

case class Item (name: String, description: String) {
 
}

object Item {
  def apply(n: xml.Node): Item = {
    new Item((n \ "@name").text, n.text)
  }
}