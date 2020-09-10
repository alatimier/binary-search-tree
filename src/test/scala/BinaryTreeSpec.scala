import org.scalatest.funsuite.AnyFunSuite

class BinaryTreeSpec extends AnyFunSuite {

  implicit val intCompare: (Int, Int) => Int = (a, b) => a.compareTo(b)

  test("an empty tree should be empty") {
    // When
    val tree = new BinaryTree[Int](null)

    // Then
    assert(tree.isEmpty)
  }

  test("should find existing values") {
    // When
    val tree = new BinaryTree[Int](null) add 1 add 2 add 3

    // Then
    assert(!tree.isEmpty)
    assert(tree.contains(1))
    assert(tree.contains(2))
    assert(tree.contains(3))
  }

  test("should not find missing values") {
    // When
    val tree = new BinaryTree[Int](null) add 1 add 2

    // Then
    assert(!tree.isEmpty)
    assert(tree.contains(1))
    assert(tree.contains(2))
    assert(!tree.contains(3))
  }

  test("should print values in order") {
    // When
    val tree = new BinaryTree[Int](null) add 4 add 1 add 2 add 6

    // Then
    assert(tree.toString() === "BST : 1 2 4 6")
  }

}
