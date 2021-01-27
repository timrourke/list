package list

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LinkedListSpec extends AnyFunSpec with Matchers {
  describe("LinkedList") {
    it("should create an empty list when nothing provided") {
      val actual = LinkedList()

      actual shouldBe Nil
    }

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
      val consed = "foo" :: original

      consed shouldEqual LinkedList("foo", "bar")
    }

    it("should not cons nil") {
      val original = LinkedList("foo")
      val consed = Nil :: original

      consed shouldEqual original
    }

    it("should cons multiple elements into a list") {
      val list = 1 :: 2 :: 3 :: LinkedList(4)

      list shouldEqual LinkedList(1, 2, 3, 4)
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

    it("should reverse the elements in a longer list") {
      val original = LinkedList(1, 2, 3, 4, 5, 6, 7)

      original.reverse shouldEqual LinkedList(7, 6, 5, 4, 3, 2, 1)
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

    it("should find element at its index") {
      val original = LinkedList("a", "b", "c")

      original(1) shouldEqual "b"
    }

    it("should find last element at its index") {
      val original = LinkedList("a", "b", "c")

      original(2) shouldEqual "c"
    }

    it("should throw if no elements exist at index") {
      val original = LinkedList("a", "b", "c")

      intercept[NoSuchElementException](
        original(45)
      ).getMessage shouldEqual "List does not contain element at index 45"
    }

    it("should throw if index is less than zero") {
      val original = LinkedList("a", "b", "c")

      intercept[NoSuchElementException](
        original(-2)
      ).getMessage shouldEqual "List does not contain element at index -2"
    }

    it("should not be empty") {
      LinkedList(4, 2, 9).isEmpty shouldBe false
    }

    it("should have a size") {
      LinkedList(1, 2, 1).size shouldBe 3
    }

    it("should have a size of 1 for a single element list") {
      LinkedList("yo").size shouldBe 1
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

    it("should not have elements at index in empty list") {
      intercept[NoSuchElementException](
        Nil(4)
      ).getMessage shouldEqual "Cannot access index of empty list"
    }

    it("should cons elements to an empty list to create a complete list") {
      val actual = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

      actual shouldEqual LinkedList(1, 2, 3, 4, 5)
    }

    it("should be empty") {
      Nil.isEmpty shouldBe true
    }

    it("should have a size of 0") {
      Nil.size shouldBe 0
    }
  }
}
