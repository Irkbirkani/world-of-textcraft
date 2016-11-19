package TextMUD

trait Stack[A] {
  /**
   * This adds an element to the stack.
   */
  def push(a: A): Unit
  /**
   * Removes element from the stack.
   * The element that is removed it the one that was most recently added.
   */
  def pop(): A
  /**
   * Gives back the next item that would be popped.
   */
  def peek: A
  /**
   * Tells if there are no items on the stack to pop.
   */
  def isEmpty: Boolean
}