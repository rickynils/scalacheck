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

  property("shrink[List[Int].suchThat") = {
    forAll { (l: List[Int]) =>
      shrink(l)(shrinkEvenLength).forall(evenLength _)
    }
  }

  /* Ensure that shrink[T] terminates. (#244)
   *
   * Shrinks must be acyclic, otherwise the shrinking process loops.
   *
   * A cycle is a set of values $x_1, x_2, ..., x_n, x_{n+1} = x_1$ such
   * that $shrink(x_i).contains(x_{i+1})$ for all i.  If the shrinking to a
   * minimal counterexample ever encounters a cycle, it will loop forever.
   *
   * To prove that a shrink is acyclic you can prove that all shrinks are
   * smaller than the shrinkee, for some strict partial ordering (proof: by
   * transitivity conclude that x_i < x_i which violates anti-reflexivity.)
   *
   * Shrinking of numeric types is ordered by magnitude and then sign, where
   * positive goes before negative, i.e. x may shrink to -x when x < 0 < -x.
   *
   * For unsigned types (e.g. Char) this is the standard ordering (<).
   * For signed types, m goes before n iff |m| < |n| or m = -n > 0.
   * (Be careful about abs(MinValue) representation issues.)
   *
   * Also, for each shrinkee the stream of shrunk values must be finite.  We
   * can empirically determine the length of the longest possible stream for a
   * given type.  Usually this involves using the type's MinValue in the case
   * of fractional types, or MinValue for integral types.
   *
   * For example, shrink(Byte.MinValue).toList gives us 8 values:
   *
   *   List(0, -64, -96, -112, -120, -124, -126, -127)
   *
   * Similarly, shrink(Double.MinValue).size gives us 2081.
   */

  property("shrink[Byte].nonEmpty") =
    forAllNoShrink((n: Byte) => Shrink.shrink(n).drop(8).isEmpty)

  property("shrink[Char].nonEmpty") =
    forAllNoShrink((n: Char) => Shrink.shrink(n).drop(16).isEmpty)

  property("shrink[Short].nonEmpty") =
    forAllNoShrink((n: Short) => Shrink.shrink(n).drop(16).isEmpty)

  property("shrink[Int].nonEmpty") =
    forAllNoShrink((n: Int) => Shrink.shrink(n).drop(32).isEmpty)

  property("shrink[Long].nonEmpty") =
    forAllNoShrink((n: Long) => Shrink.shrink(n).drop(64).isEmpty)

  property("shrink[Float].nonEmpty") =
    forAllNoShrink((n: Float) => Shrink.shrink(n).drop(289).isEmpty)

  property("shrink[Double].nonEmpty") =
    forAllNoShrink((n: Double) => Shrink.shrink(n).drop(2081).isEmpty)

  property("shrink[FiniteDuration].nonEmpty") =
    forAllNoShrink((n: FiniteDuration) => Shrink.shrink(n).drop(64).isEmpty)

  property("shrink[Duration].nonEmpty") =
    forAllNoShrink((n: Duration) => Shrink.shrink(n).drop(64).isEmpty)

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

  // That was finiteness of a single step of shrinking.  Now let's prove that
  // you cannot shrink for infinitely many steps, by showing that shrinking
  // always goes to smaller values, ordered by magnitude and then sign.
  // This is a strict partial ordering, hence there can be no cycles.  It is
  // also a well-ordering on BigInt, and all other types we test are finite,
  // hence there can be no infinite regress.

  def numericMayShrinkTo[T: Numeric](n: T, m: T): Boolean = {
    val num = implicitly[Numeric[T]]
    import num.{abs, equiv, lt, zero}
    lt(abs(m), abs(n)) || (lt(n, zero) && equiv(m, abs(n)))
  }

  def twosComplementMayShrinkTo[T: Integral: TwosComplement](n: T, m: T): Boolean =
  {
    val lowerBound = implicitly[TwosComplement[T]].minValue
    val integral = implicitly[Integral[T]]
    import integral.{abs, equiv, lt, zero}

    // Note: abs(minValue) = minValue < 0 for two's complement signed types
    require(equiv(lowerBound, abs(lowerBound)))
    require(lt(abs(lowerBound), zero))

    // Due to this algebraic issue, we have to special case `lowerBound`
    if (n == lowerBound) m != lowerBound
    else if (m == lowerBound) false
    else numericMayShrinkTo(n, m) // simple algebra Just Works(TM)
  }

  case class TwosComplement[T](minValue: T)
  implicit val minByte: TwosComplement[Byte] = TwosComplement(Byte.MinValue)
  implicit val minShort: TwosComplement[Short] = TwosComplement(Short.MinValue)
  implicit val minInt: TwosComplement[Int] = TwosComplement(Integer.MIN_VALUE)
  implicit val minLong: TwosComplement[Long] = TwosComplement(Long.MinValue)

  // Let's first verify that this is in fact a strict partial ordering.
  property("twosComplementMayShrinkTo is antireflexive") =
    forAllNoShrink { (n: Int) => !twosComplementMayShrinkTo(n, n) }

  val transitive = for {
    a <- Arbitrary.arbitrary[Int]
    b <- Arbitrary.arbitrary[Int]
    if twosComplementMayShrinkTo(a, b)
    c <- Arbitrary.arbitrary[Int]
    if twosComplementMayShrinkTo(b, c)
  } yield twosComplementMayShrinkTo(a, c)

  property("twosComplementMayShrinkTo is transitive") =
    forAllNoShrink(transitive.retryUntil(Function.const(true)))(identity)

  // let's now show that shrinks are acyclic for integral types

  property("shrink[Byte] is acyclic") = forAllNoShrink { (n: Byte) =>
    shrink(n).forall(twosComplementMayShrinkTo(n, _))
  }

  property("shrink[Short] is acyclic") = forAllNoShrink { (n: Short) =>
    shrink(n).forall(twosComplementMayShrinkTo(n, _))
  }

  property("shrink[Char] is acyclic") = forAllNoShrink { (n: Char) =>
    shrink(n).forall(numericMayShrinkTo(n, _))
  }

  property("shrink[Int] is acyclic") = forAllNoShrink { (n: Int) =>
    shrink(n).forall(twosComplementMayShrinkTo(n, _))
  }

  property("shrink[Long] is acyclic") = forAllNoShrink { (n: Long) =>
    shrink(n).forall(twosComplementMayShrinkTo(n, _))
  }

  property("shrink[BigInt] is acyclic") = forAllNoShrink { (n: BigInt) =>
    shrink(n).forall(numericMayShrinkTo(n, _))
  }

  property("shrink[Float] is acyclic") = forAllNoShrink { (x: Float) =>
    shrink(x).forall(numericMayShrinkTo(x, _))
  }

  property("shrink[Double] is acyclic") = forAllNoShrink { (x: Double) =>
    shrink(x).forall(numericMayShrinkTo(x, _))
  }

  property("shrink[Duration] is acyclic") = forAllNoShrink { (x: Duration) =>
    shrink(x).forall(y => twosComplementMayShrinkTo(x.toNanos, y.toNanos))
  }

  property("shrink[FiniteDuration] is acyclic") =
    forAllNoShrink { (x: FiniteDuration) =>
      shrink(x).forall(y => twosComplementMayShrinkTo(x.toNanos, y.toNanos))
    }

  // Recursive integral shrinking stops at a success/failure boundary,
  // i.e. some m such that m fails and m-1 succeeds if 0 < m and m+1
  // succeeds if m < 0, or shrinks to 0.
  //
  // Test that shrink(n) contains n-1 if positive or n+1 if negative.
  //
  // From this our conclusion follows:
  //  - If 0 < n and n fails and n-1 fails then we can shrink to n-1.
  //  - If n < 0 and n fails and n+1 fails then we can shrink to n+1.
  // In neither case do we stop shrinking at n.
  //
  // Since shrinking only stops at failing values, we stop shrinking at:
  //  - Some n such that 0 < n and n fails and n-1 succeeds
  //  - Some n such that n < 0 and n fails and n+1 succeeds
  //  - 0
  // which is exactly what we wanted to conclude.

  def stepsByOne[T: Arbitrary: Numeric: Shrink]: Prop = {
    val num = implicitly[Numeric[T]]
    import num.{equiv, lt, negate, one, plus, zero}
    val minusOne = negate(one)

    forAll {
      (n: T) => (!equiv(n, zero)) ==> {
        val delta = if (lt(n, zero)) one else minusOne
        shrink(n).contains(plus(n, delta))
      }
    }
  }

  property("shrink[Byte](n).contains(n - |n|/n)") = stepsByOne[Byte]
  property("shrink[Short](n).contains(n - |n|/n)") = stepsByOne[Short]
  property("shrink[Char](n).contains(n - |n|/n)") = stepsByOne[Char]
  property("shrink[Int](n).contains(n - |n|/n)") = stepsByOne[Int]
  property("shrink[Long](n).contains(n - |n|/n)") = stepsByOne[Long]
  property("shrink[BigInt](n).contains(n - |n|/n)") = stepsByOne[BigInt]

  // As a special case of the above, if n succeeds iff lo < n < hi for some
  // pair of limits lo <= 0 <= hi, then shrinking stops at lo or hi.  Let's
  // test this concrete consequence.

  def minimalCounterexample[T: Shrink](ok: T => Boolean, x: T): T =
    shrink(x).dropWhile(ok).headOption.fold(x)(minimalCounterexample(ok, _))

  def findsBoundary[T: Arbitrary: Numeric: Shrink]: Prop = {
    val num = implicitly[Numeric[T]]
    import num.{lt, lteq, zero}

    def valid(lo: T, hi: T, start: T): Boolean =
      lteq(lo, zero) && lteq(zero, hi) && (lteq(start, lo) || lteq(hi, start))

    forAll(Arbitrary.arbitrary[(T, T, T)].retryUntil((valid _).tupled)) {
      case (lo, hi, start) => valid(lo, hi, start) ==> {
        val ok = (n: T) => lt(lo, n) && lt(n, hi)
        val stop = minimalCounterexample[T](ok, start)
        s"($lo, $hi, $start) => $stop" |: (stop == lo || stop == hi)
      }
    }
  }

  property("shrink finds the exact boundary: Byte")   = findsBoundary[Byte]
  property("shrink finds the exact boundary: Short")  = findsBoundary[Short]
  property("shrink finds the exact boundary: Int")    = findsBoundary[Int]
  property("shrink finds the exact boundary: Long")   = findsBoundary[Long]
  property("shrink finds the exact boundary: BigInt") = findsBoundary[BigInt]

  // Unsigned types are one-sided.  Test on the range (0 until limit).
  property("shrink finds the exact boundary: Char")   = forAll {
    (a: Char, b: Char) =>
    val (limit, start) = (a min b, a max b)
    require(limit <= start)
    val result = minimalCounterexample[Char](_ < limit, start)
    s"(${limit.toInt}, ${start.toInt}) => ${result.toInt}" |: result == limit
  }
}
