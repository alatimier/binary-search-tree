import scala.annotation.tailrec

sealed case class Node[T <: AnyVal](value: T, left: Node[T], right: Node[T])

object Node {
  def create[T <: AnyVal](value: T): Node[T] = {
    Node(value, null, null)
  }
}

class BinaryTree[T <: AnyVal] private(val rootNode: Node[T])(implicit val compare: (T, T) => Int) {

  def this()(implicit compare: (T, T) => Int) {
    this(null)
  }

  def isEmpty: Boolean = rootNode == null

  def add(value: T): BinaryTree[T] = {
    if (rootNode == null) new BinaryTree(Node.create(value)) else new BinaryTree(add(rootNode, value))
  }

  private def add(node: Node[T], value: T): Node[T] = {
    compare(node.value, value) match {
      case 1 =>
        val left = if (node.left == null) Node.create(value) else add(node.left, value)
        Node(node.value, left, node.right)
      case -1 =>
        val right = if (node.right == null) Node.create(value) else add(node.right, value)
        Node(node.value, node.left, right)
      case 0 => node
    }
  }

  def contains(value: T): Boolean = contains(rootNode, value)

  @tailrec
  private def contains(node: Node[T], value: T): Boolean = {
    compare(node.value, value) match {
      case 1 => node.left != null && contains(node.left, value)
      case -1 => node.right != null && contains(node.right, value)
      case 0 => true
    }
  }

  override def toString: String = {
    reduceLeft("BST :", (acc: String, value) => s"$acc $value")
  }

  def reduceLeft[R](acc: R, f: (R, T) => R): R = {
    reduceLeft(rootNode, acc, f)
  }

  private def reduceLeft[R](node: Node[T], acc: R, f: (R, T) => R): R = {
    var res = acc
    if (node.left != null) res = reduceLeft(node.left, res, f)
    res = f(res, node.value)
    if (node.right != null) res = reduceLeft(node.right, res, f)
    res
  }

}
