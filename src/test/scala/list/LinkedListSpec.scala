package list

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LinkedListSpec extends AnyFunSpec with Matchers {
  describe("LinkedList") {
    it("has a head") {
      val actual = LinkedList("cats")

      actual.head shouldEqual "cats"
    }

    it("has a tail") {
      val actual = LinkedList("foo", LinkedList("bar"))

      actual.tail shouldEqual LinkedList("bar")
      actual.tail.head shouldEqual "bar"
    }

    it("has a nil tail by default") {
      val actual = LinkedList("dogs")

      actual.tail shouldBe Nil
    }

    it("should construct a linked list from many elements") {
      val actual = LinkedList("cats", "dogs", "mice")

      actual.head shouldEqual "cats"
      actual.tail shouldEqual LinkedList("dogs", LinkedList("mice"))
    }

    it("should cons an element") {
      val original = LinkedList("bar")
      val consed = original.cons("foo")

      consed shouldEqual LinkedList("foo", "bar")
    }

    it("should not cons nil") {
      val original = LinkedList("foo")
      val consed = original.cons(Nil)

      consed shouldEqual original
    }

    it("should get the last element in the list") {
      val actual = LinkedList("a", "b", "c")

      actual.last shouldEqual "c"
    }

    it("should get the init of the list") {
      val actual = LinkedList("a", "b", "c")

      actual.init shouldEqual LinkedList("a", "b")
    }

    it("should get the init of a single element list") {
      val actual = LinkedList("foo")

      actual.init shouldEqual actual
    }

    it("should reverse the elements in the list") {
      val original = LinkedList("a", "b", "c")

      original.reverse shouldEqual LinkedList("c", "b", "a")
    }

    it("should reverse the elements in a single element list") {
      val original = LinkedList("foo")

      original.reverse shouldEqual original
    }

    it("should map its elements") {
      val original = LinkedList("cats", "dogs", "mice")
      val mapped = original.map(_.toUpperCase)

      mapped shouldEqual LinkedList("CATS", "DOGS", "MICE")
    }

    it("should map single element list") {
      val original = LinkedList("ice cream")
      val mapped = original.map(_.toUpperCase)

      mapped shouldEqual LinkedList("ICE CREAM")
    }

    it("should filter its elements") {
      val original = LinkedList("a", "b", "c")
      val filtered = original.filter(_ != "b")

      filtered shouldEqual LinkedList("a", "c")
    }

    it("should retain all elements when filter function is always true") {
      val original = LinkedList("a", "b", "b")
      val filtered = original.filter(_ => true)

      filtered shouldEqual original
    }

    it("should filter single element list") {
      val original = LinkedList("foo")
      val filtered = original.filter(_ != "foo")

      filtered shouldBe Nil
    }
  }

  describe("Nil") {
    it("should not have a head") {
      intercept[NoSuchElementException](
        Nil.head
      ).getMessage shouldEqual "Cannot get head of empty list"
    }

    it("should not have a tail") {
      intercept[NoSuchElementException](
        Nil.tail
      ).getMessage shouldEqual "Cannot get tail of empty list"
    }

    it("should not have an init") {
      intercept[NoSuchElementException](
        Nil.init
      ).getMessage shouldEqual "Cannot get init of empty list"
    }

    it("should still be nil in reverse") {
      Nil.reverse shouldEqual Nil
    }

    it("should not have a last element") {
      intercept[NoSuchElementException](
        Nil.last
      ).getMessage shouldEqual "Cannot get last element of empty list"
    }

    it("should map to nil") {
      Nil.map(identity) shouldEqual Nil
    }

    it("should filter to nil") {
      Nil.filter(_ => true) shouldEqual Nil
      Nil.filter(_ => false) shouldEqual Nil
    }
  }
}
