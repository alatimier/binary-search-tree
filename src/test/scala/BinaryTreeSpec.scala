import org.scalatest.funsuite.AnyFunSuite

class BinaryTreeSpec extends AnyFunSuite {

  implicit val intCompare: (Int, Int) => Int = (a, b) => a.compareTo(b)

  test("an empty tree should be empty") {
    // When
    val tree = new BinaryTree

    // Then
    assert(tree.isEmpty)
  }

  test("should add and find existing values") {
    // When
    val tree = new BinaryTree add 1 add 2 add 3

    // Then
    assert(!tree.isEmpty)
    assert(tree.contains(1))
    assert(tree.contains(2))
    assert(tree.contains(3))
  }

  test("should add and not find missing values") {
    // When
    val tree = new BinaryTree add 1 add 2

    // Then
    assert(!tree.isEmpty)
    assert(tree.contains(1))
    assert(tree.contains(2))
    assert(!tree.contains(3))
  }

  test("should remove leaf value") {
    // Given
    var tree = new BinaryTree add 4 add 2 add 3 add 1

    // When
    tree = tree remove 1

    // Then
    assert(tree.toString() === "BST : 2 3 4")
  }

  test("should remove node with one leaf") {
    // Given
    var tree = new BinaryTree add 4 add 2 add 3 add 1

    // When
    tree = tree remove 4

    // Then
    assert(tree.toString() === "BST : 1 2 3")
  }

  test("should remove node with two leaves") {
    // Given
    var tree = new BinaryTree add 4 add 2 add 3 add 1

    // When
    tree = tree remove 2

    // Then
    assert(tree.toString() === "BST : 1 3 4")
  }

  test("should print values in order") {
    // When
    val tree = new BinaryTree add 4 add 1 add 2 add 6

    // Then
    assert(tree.toString() === "BST : 1 2 4 6")
  }

}
