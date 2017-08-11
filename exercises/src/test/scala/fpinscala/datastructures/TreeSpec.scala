package fpinscala.datastructures

import org.scalatest._
import Tree._

class TreeSpec extends FlatSpec {
  val tree1 = Branch( Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val tree2 = Branch( Branch(Leaf(11), Leaf(12)), Branch(Leaf(13), Branch(Leaf(14), Leaf(15))))
  val tree3 = Branch(tree1, tree2)

  "Exercise 25 - size(tree)" should "return the number of elements in tree" in {
    assertResult(1) {size(Leaf(42))}
    assertResult(4) {size(tree1)}
    assertResult(5) {size(tree2)}
    assertResult(9) {size(tree3)}
  }

  "Exercise 26 - maximum(tree)" should "return the maximum element in tree" in {
    assertResult(42) {maximum(Leaf(42))}
    assertResult(4) {maximum(tree1)}
    assertResult(15) {maximum(tree2)}
    assertResult(15) {maximum(tree3)}
  }

  "Exercise 27 - depth(tree)" should "return max path length from the root of tree to any leaf" in {
    assertResult(1) {depth(Leaf(42))}
    assertResult(3) {depth(tree1)}
    assertResult(4) {depth(tree2)}
    assertResult(5) {depth(tree3)}
  }

  "Exercise 28 - map(tree)(f)" should "return a new tree with each value modified by f" in {
    val mtree1 = Branch( Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
    val mtree2 = Branch( Branch(Leaf(22), Leaf(24)), Branch(Leaf(26), Branch(Leaf(28), Leaf(30))))
    val mtree3 = Branch(mtree1, mtree2)
    assertResult(Leaf("42")) {map(Leaf(42))(x => x.toString)}
    assertResult(mtree1) {map(tree1)(_ * 2)}
    assertResult(mtree2) {map(tree2)(_ * 2)}
    assertResult(mtree3) {map(tree3)(_ * 2)}
  }

  "Exercise 29 - fold(tree, init)(f)" should "generalize the work of the previous function" in {
    // sizeFld
    assertResult(1) {sizeFld(Leaf(42))}
    assertResult(4) {sizeFld(tree1)}
    assertResult(5) {sizeFld(tree2)}
    assertResult(9) {sizeFld(tree3)}

    // maximumFld
    assertResult(42) {maximumFld(Leaf(42))}
    assertResult(4)  {maximumFld(tree1)}
    assertResult(15) {maximumFld(tree2)}
    assertResult(15) {maximumFld(tree3)}

    // depthFld
    assertResult(1) {depthFld(Leaf(42))}
    assertResult(3) {depthFld(tree1)}
    assertResult(4) {depthFld(tree2)}
    assertResult(5) {depthFld(tree3)}

    // mapFld
    val mtree1 = Branch( Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
    val mtree2 = Branch( Branch(Leaf(22), Leaf(24)), Branch(Leaf(26), Branch(Leaf(28), Leaf(30))))
    val mtree3 = Branch(mtree1, mtree2)
    assertResult(Leaf("42")) {mapFld(Leaf(42))(x => x.toString)}
    assertResult(mtree1)     {mapFld(tree1)(_ * 2)}
    assertResult(mtree2)     {mapFld(tree2)(_ * 2)}
    assertResult(mtree3)     {mapFld(tree3)(_ * 2)}
  }
}
