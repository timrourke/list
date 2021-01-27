package list

sealed trait SinglyLinkedList[+A] {

  /**
    * Get the first element in the list
    */
  def head: A

  /**
    * Get every element in the list after the first element
    */
  def tail: SinglyLinkedList[A]

  /**
    * Get the initial elements of the list excluding the final element
    */
  def init: SinglyLinkedList[A]

  /**
    * Get the last non-nil element in the list
    */
  def last: A

  /**
    * Get the list in reverse order
    */
  def reverse: SinglyLinkedList[A]

  /**
    * Prepend an element to the beginning of the list
    */
  def cons[B >: A](other: B): SinglyLinkedList[B]

  /**
    * Produce a new list containing the results of calling a given function
    * on each element in the list
    */
  def map[B](fn: A => B): SinglyLinkedList[B]

  /**
    * Return a new list where each element in the new list passes a boolean
    * predicate function
    */
  def filter(fn: A => Boolean): SinglyLinkedList[A]
}

case object Nil extends SinglyLinkedList[Nothing] {
  def head: Nothing =
    throw new NoSuchElementException("Cannot get head of empty list")

  def tail: SinglyLinkedList[Nothing] =
    throw new NoSuchElementException("Cannot get tail of empty list")

  def init =
    throw new NoSuchElementException("Cannot get init of empty list")

  def last =
    throw new NoSuchElementException("Cannot get last element of empty list")

  def reverse: SinglyLinkedList[Nothing] = Nil
  def cons[B](other: B): SinglyLinkedList[B] = LinkedList(other)
  def map[B](fn: Nothing => B): SinglyLinkedList[Nothing] = Nil
  def filter(fn: Nothing => Boolean): SinglyLinkedList[Nothing] = Nil
}

case class LinkedList[A](
    head: A = Nil,
    tail: SinglyLinkedList[A] = Nil
) extends SinglyLinkedList[A] {
  def last: A = {
    tail match {
      case Nil                 => head
      case LinkedList(_, tail) => tail.last
    }
  }

  def init: LinkedList[A] = {
    LinkedList(
      head,
      tail match {
        case Nil                          => Nil
        case LinkedList(_, t) if t == Nil => Nil
        case LinkedList(_, _)             => tail.init
      }
    )
  }

  def reverse: SinglyLinkedList[A] = {
    tail match {
      case Nil => LinkedList(head)
      case LinkedList(_, t) if t != Nil =>
        LinkedList(last, LinkedList(head, tail.init).reverse)
      case LinkedList(h, Nil) => LinkedList(h, LinkedList(head))
    }
  }

  def cons[B >: A](other: B): SinglyLinkedList[B] = {
    other match {
      case Nil => this
      case _   => LinkedList(other, this)
    }
  }

  def map[B](fn: A => B): LinkedList[B] = {
    LinkedList(fn(head), tail.map(fn))
  }

  def filter(fn: A => Boolean): SinglyLinkedList[A] = {
    if (fn(head)) {
      LinkedList(head, tail.filter(fn))
    } else {
      tail match {
        case Nil                       => tail
        case LinkedList(h, t) if fn(h) => LinkedList(h, t.filter(fn))
        case LinkedList(_, t)          => t.filter(fn)
      }
    }
  }
}

object LinkedList {
  def apply[A](elems: A*): SinglyLinkedList[A] = {
    if (elems.isEmpty) {
      Nil
    } else {
      LinkedList(elems.head, LinkedList(elems.tail: _*))
    }
  }
}
