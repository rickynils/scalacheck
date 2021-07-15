package org.scalacheck.commands

import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink}
import scala.reflect.ClassTag
import scala.util.{Success, Try}

object CommandsShrinkSpecification extends Properties("Commands Shrinking") {

  property("Shrunk command sequences always satisfy preconditions") = {
    forAll(Arbitrary.arbitrary[Long]) {
      case seedValue =>
        val seed = Seed(seedValue)
        val parameters = Gen.Parameters.default.withInitialSeed(seed)
        val result = BoundedQueueSpec.property(1)(parameters)

        // If the property fails shrinking will kick in.  If shrinking does
        // not guarantee that preconditions are satisfied and the shrunk
        // command sequence contains the Dequeue command, we will invoke
        // Nil.tail which will raise UnsupportedOperationException.
        result.status match {
          case Prop.Exception(e: UnsupportedOperationException) => false
          case _ => true
        }
    }
  }

  class BoundedQueue[Element: ClassTag](val capacity: Int) {
    require(capacity >= 0)

    var buffer = new Array[Element](capacity + 1)
    var reads = 0
    var writes = 0

    def size: Int = {
      (writes + buffer.length - reads) % buffer.length
    }

    def enqueue(element: Element): Unit = {
      require(size < capacity)
      buffer(writes) = element
      writes += 1
      // writes %= buffer.length // deliberately broken to provoke failure
    }

    def dequeue(): Element = {
      require(size > 0)
      val index = reads
      reads += 1
      reads %= buffer.length
      buffer(index)
    }
  }

  object BoundedQueueSpec extends Commands {
    type Element = Byte

    type Sut = BoundedQueue[Element]

    case class State(capacity: Int, elements: Seq[Element])

    case object Capacity extends Command {
      type Result = Int
      def run(sut: Sut): Result = sut.capacity
      def nextState(state: State): State = state
      def preCondition(state: State): Boolean = true
      def postCondition(state: State, result: Try[Result]): Prop =
        result == Success(state.capacity)
    }

    case object Size extends Command {
      type Result = Int
      def run(sut: Sut): Result = sut.size
      def nextState(state: State): State = state
      def preCondition(state: State): Boolean = true
      def postCondition(state: State, result: Try[Result]): Prop =
        result == Success(state.elements.length)
    }

    case class Enqueue(element: Element) extends UnitCommand {
      def run(sut: Sut): Unit =
        sut.enqueue(element)
      def nextState(state: State): State =
        State(state.capacity, state.elements ++ List(element))
      def preCondition(state: State): Boolean =
        state.elements.length < state.capacity
      def postCondition(state: State, success: Boolean): Prop =
        success
    }

    case object Dequeue extends Command {
      type Result = Element
      def run(sut: Sut): Result =
        sut.dequeue()
      def nextState(state: State): State =
        State(state.capacity, state.elements.tail)
      def preCondition(state: State): Boolean =
        state.elements.nonEmpty
      def postCondition(state: State, result: Try[Result]): Prop =
        result == Success(state.elements.head)
    }

    def genInitialState: Gen[State] =
      Gen.choose(10, 20).map(State(_, Nil))

    def genCommand(state: State): Gen[Command] =
      Gen.oneOf(
        Gen.const(Capacity),
        Gen.const(Size),
        Gen.const(Dequeue),
        Arbitrary.arbitrary[Element].map(Enqueue(_))
      ).retryUntil(_.preCondition(state), 100)

    def newSut(state: State): Sut =
      new BoundedQueue(state.capacity)

    def initialPreCondition(state: State): Boolean =
      state.capacity > 0 && state.elements.isEmpty

    override def shrinkState: Shrink[State] =
      Shrink.xmap[(Int, Seq[Element]), State](
        { case (capacity, elems) => State(capacity, elems take capacity) },
        { case State(capacity, elems) => (capacity, elems) }
      ).suchThat(_.capacity >= 0)

    def canCreateNewSut(
      newState: State,
      initSuts: Traversable[State],
      runningSuts: Traversable[Sut]
    ): Boolean =
      true

    def destroySut(sut: Sut): Unit =
      ()
  }
}
