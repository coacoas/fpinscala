package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  "addWith" should "add two lists together" in {
    List.addWith(List(1, 2, 3), List(4, 5, 6)) should equal(List(5, 7, 9))
  }
}
