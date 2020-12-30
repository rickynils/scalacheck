/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2021 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Prop.{forAll, forAllNoShrink, propBoolean}
import Shrink.shrink

import scala.concurrent.duration.{Duration, FiniteDuration}

object ShrinkSpecification extends Properties("Shrink") {

  def shrinkClosure[T : Shrink](x: T): Stream[T] = {
    val xs = shrink[T](x)
    if(xs.isEmpty) xs
    else xs.append(xs.take(1).map(shrinkClosure[T]).flatten)
  }

  property("shrink[Byte]") = forAll { (n: Byte) =>
    !shrink(n).contains(n)
  }

  property("shrink[Short]") = forAll { (n: Short) =>
    !shrink(n).contains(n)
  }

  property("shrink[Int]") = forAll { (n: Int) =>
    !shrink(n).contains(n)
  }

  property("shrink[Long]") = forAll { (n: Long) =>
    !shrink(n).contains(n)
  }

  property("shrink[Float]") = forAll { (n: Float) =>
    !shrink(n).contains(n)
  }

  property("shrink[Double]") = forAll { (n: Double) =>
    !shrink(n).contains(n)
  }

  property("shrink[Duration]") = forAll { (n: Duration) =>
    !shrink(n).contains(n)
  }

  property("shrink[FiniteDuration]") = forAll { (n: FiniteDuration) =>
    !shrink(n).contains(n)
  }

  property("shrink[Byte] != 0") = forAll { (n: Byte) =>
    (n != 0) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[Short] != 0") = forAll { (n: Short) =>
    (n != 0) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[Int] != 0") = forAll { (n: Int) =>
    (n != 0) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[Long] != 0") = forAll { (n: Long) =>
    (n != 0) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[Float] != 0") = forAll { (n: Float) =>
    (math.abs(n) > 1E-5f) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[Double] != 0") = forAll { (n: Double) =>
    (math.abs(n) > 1E-5d) ==> shrinkClosure(n).contains(0)
  }

  property("shrink[FiniteDuration] != 0") = forAll { (n: FiniteDuration) =>
    (n != Duration.Zero) ==> shrinkClosure(n).contains(Duration.Zero)
  }

  property("shrink[Duration] != 0") = forAll { (n: Duration) =>
    (n.isFinite && n != Duration.Zero) ==> shrinkClosure(n).contains(Duration.Zero)
  }

  implicit def vectorShrink[A: Shrink]: Shrink[Vector[A]] = Shrink.xmap[List[A],Vector[A]](Vector(_: _*), _.toList)

  property("shrink[Either]") = forAll { (e: Either[Int, Long]) =>
    !shrink(e).contains(e)
  }

  property("shrink[Left]") = forAll { (i: Int) =>
    val e: Either[Int, Long] = Left(i)
    shrink(e).forall(_.isLeft)
  }

  property("shrink[Right]") = forAll { (i: Int) =>
    val e: Either[Long, Int] = Right(i)
    shrink(e).forall(_.isRight)
  }

  property("shrink[Unit].isEmpty") = Prop(shrink(()).isEmpty)

  // Tuples shrink even when one component doesn't
  property("shrink[(T, Unit)] eqv shrink[T]") =
    forAllNoShrink { (u: Unit, i: Int) =>
      shrink(((), i)) == shrink(i).map(((), _))
    }

  property("shrink[(Unit, T)] eqv shrink[T]") =
    forAllNoShrink { (i: Int, u: Unit) =>
      shrink((i, ())) == shrink(i).map((_, ()))
    }

  // Tuple shrinking is associative* for all arities, and can be inductively
  // defined for n in terms of 2 and n-1. (* modulo ordering)
  def eqvTupleShrinks[T: Ordering](xs: Stream[T], ys: Stream[T]): Boolean =
    xs.toList.sorted == ys.toList.sorted

  property("shrink[(T, U, V)] eqv shrink[(T, (U, V))]") =
    forAllNoShrink { (b: Byte, c: Char, s: Short) =>
      eqvTupleShrinks(
        shrink((b, c, s)),
        shrink((b, (c, s))).map { case (b, (c, s)) => (b, c, s) }
      )
    }

  property("shrink[(T, U, V, W)] eqv shrink[((T, U, V), W)]") =
    forAllNoShrink { (b: Byte, c: Char, s: Short, i: Int) =>
      eqvTupleShrinks(
        shrink((b, c, s, i)),
        shrink(((b, c, s), i)).map { case ((b, c, s), i) => (b, c, s, i) }
      )
    }

  property("suchThat") = {
    implicit def shrinkEvenLength[A]: Shrink[List[A]] =
      Shrink.shrinkContainer[List,A].suchThat(evenLength _)
    val genEvenLengthLists =
      Gen.containerOf[List,Int](Arbitrary.arbitrary[Int]).suchThat(evenLength _)
    forAll(genEvenLengthLists) { (l: List[Int]) =>
      evenLength(l)
    }
  }

  def evenLength(value: List[_]) = value.length % 2 == 0
  def shrinkEvenLength[A]: Shrink[List[A]] =
    Shrink.shrinkContainer[List,A].suchThat(evenLength _)

  property("shrink[List[Int]].suchThat") = {
    forAll { (l: List[Int]) =>
      shrink(l)(shrinkEvenLength).forall(evenLength _)
    }
  }

  /* Ensure that shrink[T] terminates. (#244)
   *
   * Let's say shrinking "terminates" when the stream of values
   * becomes empty. We can empirically determine the longest possible
   * sequence for a given type before termination. (Usually this
   * involves using the type's MinValue.)
   *
   * For example, shrink(Byte.MinValue).toList gives us 15 values:
   *
   *   List(-64, 64, -32, 32, -16, 16, -8, 8, -4, 4, -2, 2, -1, 1, 0)
   *
   * Similarly, shrink(Double.MinValue).size gives us 2081.
   */

  property("shrink[Byte].nonEmpty") =
    forAllNoShrink((n: Byte) => Shrink.shrink(n).drop(15).isEmpty)

  property("shrink[Char].nonEmpty") =
    forAllNoShrink((n: Char) => Shrink.shrink(n).drop(16).isEmpty)

  property("shrink[Short].nonEmpty") =
    forAllNoShrink((n: Short) => Shrink.shrink(n).drop(31).isEmpty)

  property("shrink[Int].nonEmpty") =
    forAllNoShrink((n: Int) => Shrink.shrink(n).drop(63).isEmpty)

  property("shrink[Long].nonEmpty") =
    forAllNoShrink((n: Long) => Shrink.shrink(n).drop(127).isEmpty)

  property("shrink[Float].nonEmpty") =
    forAllNoShrink((n: Float) => Shrink.shrink(n).drop(2081).isEmpty)

  property("shrink[Double].nonEmpty") =
    forAllNoShrink((n: Double) => Shrink.shrink(n).drop(2081).isEmpty)

  property("shrink[FiniteDuration].nonEmpty") =
    forAllNoShrink((n: FiniteDuration) => Shrink.shrink(n).drop(2081).isEmpty)

  property("shrink[Duration].nonEmpty") =
    forAllNoShrink((n: Duration) => Shrink.shrink(n).drop(2081).isEmpty)

  // make sure we handle sentinel values appropriately for Float/Double.

  property("shrink(Float.PositiveInfinity)") =
    Prop(Shrink.shrink(Float.PositiveInfinity).isEmpty)

  property("shrink(Float.NegativeInfinity)") =
    Prop(Shrink.shrink(Float.NegativeInfinity).isEmpty)

  property("shrink(Float.NaN)") =
    Prop(Shrink.shrink(Float.NaN).isEmpty)

  property("shrink(Double.PositiveInfinity)") =
    Prop(Shrink.shrink(Double.PositiveInfinity).isEmpty)

  property("shrink(Double.NegativeInfinity)") =
    Prop(Shrink.shrink(Double.NegativeInfinity).isEmpty)

  property("shrink(Double.NaN)") =
    Prop(Shrink.shrink(Double.NaN).isEmpty)

  property("shrink(Duration.Inf)") =
    Prop(Shrink.shrink(Duration.Inf: Duration).isEmpty)

  property("shrink(Duration.MinusInf)") =
    Prop(Shrink.shrink(Duration.MinusInf: Duration).isEmpty)

  property("shrink(Duration.Undefined)") =
    Prop(Shrink.shrink(Duration.Undefined: Duration).isEmpty)
}
