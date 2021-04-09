/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2021 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import language.higherKinds

import util.Buildable
import util.SerializableCanBuildFroms._
import scala.concurrent.duration.{Duration, FiniteDuration}

sealed abstract class Shrink[T] extends Serializable {
  def shrink(x: T): Stream[T]

  /** Create a new shrink that only produces values satisfying the given
   *  condition.
   */
  def suchThat(f: T => Boolean): Shrink[T] = Shrink(v => this.shrink(v).filter(f))
}

trait ShrinkLowPriority {
  /** Default shrink instance */
  implicit def shrinkAny[T]: Shrink[T] = Shrink(_ => Stream.empty)
}

object Shrink extends ShrinkLowPriority with ShrinkVersionSpecific with time.JavaTimeShrink {

  import Stream.{cons, empty}
  import scala.collection._

  /** Interleaves two streams */
  private def interleave[T](xs: Stream[T], ys: Stream[T]): Stream[T] =
    if(xs.isEmpty) ys
    else if(ys.isEmpty) xs
    else cons(xs.head, cons(ys.head, interleave(xs.tail, ys.tail)))

  /** Shrink instance factory */
  def apply[T](s: T => Stream[T]): Shrink[T] = new Shrink[T] {
    override def shrink(x: T) = s(x)
  }

  /** Shrink a value */
  def shrink[T](x: T)(implicit s: Shrink[T]): Stream[T] = s.shrink(x)

  /** Shrink a value, but also return the original value as the first element in
   *  the resulting stream */
  def shrinkWithOrig[T](x: T)(implicit s: Shrink[T]): Stream[T] =
    cons(x, s.shrink(x))

  /** Shrink instance of container */
  implicit def shrinkContainer[C[_],T](implicit v: C[T] => Traversable[T], s: Shrink[T],
    b: Buildable[T,C[T]]
  ): Shrink[C[T]] = Shrink { (xs: C[T]) =>
    val ys = v(xs)
    val zs = ys.toStream
    removeChunks(ys.size,zs).append(shrinkOne(zs)).map(b.fromIterable)
  }

  /** Shrink instance of container2 */
  implicit def shrinkContainer2[C[_,_],T,U](implicit v: C[T,U] => Traversable[(T,U)], s: Shrink[(T,U)],
    b: Buildable[(T,U),C[T,U]]
  ): Shrink[C[T,U]] = Shrink { (xs: C[T,U]) =>
    val ys = v(xs)
    val zs = ys.toStream
    removeChunks(ys.size,zs).append(shrinkOne(zs)).map(b.fromIterable)
  }

  private def removeChunks[T](n: Int, xs: Stream[T]): Stream[Stream[T]] =
    if (xs.isEmpty) empty
    else if (xs.tail.isEmpty) cons(empty, empty)
    else {
      val n1 = n / 2
      val n2 = n - n1
      lazy val xs1 = xs.take(n1)
      lazy val xs2 = xs.drop(n1)
      lazy val xs3 =
        for (ys1 <- removeChunks(n1, xs1) if !ys1.isEmpty) yield ys1 append xs2
      lazy val xs4 =
        for (ys2 <- removeChunks(n2, xs2) if !ys2.isEmpty) yield xs1 append ys2

      cons(xs1, cons(xs2, interleave(xs3, xs4)))
    }

  private def shrinkOne[T : Shrink](zs: Stream[T]): Stream[Stream[T]] =
    if (zs.isEmpty) empty
    else {
      val x = zs.head
      val xs = zs.tail
      shrink(x).map(cons(_,xs)).append(shrinkOne(xs).map(cons(x,_)))
    }

  /** Shrink instances for numeric data types */

  implicit def shrinkFractional[T: Fractional]: Shrink[T] =
    new ShrinkFractional[T]

  implicit def shrinkIntegral[T: Integral]: Shrink[T] =
    new ShrinkIntegral[T]

  /** Shrink instance of String */
  implicit lazy val shrinkString: Shrink[String] = Shrink { s =>
    shrinkContainer[List,Char].shrink(s.toList).map(_.mkString)
  }

  /** Shrink instance of Option */
  implicit def shrinkOption[T : Shrink]: Shrink[Option[T]] = Shrink {
    case None => empty
    case Some(x) => cons(None, for(y <- shrink(x)) yield Some(y))
  }

  /** Shrink instance of 2-tuple */
  implicit def shrinkTuple2[
    T1:Shrink, T2:Shrink
  ]: Shrink[(T1,T2)] =
    Shrink { case (t1,t2) =>
      shrink(t1).map((_,t2)) append
      shrink(t2).map((t1,_))
    }

  /** Shrink instance of 3-tuple */
  implicit def shrinkTuple3[
    T1:Shrink, T2:Shrink, T3:Shrink
  ]: Shrink[(T1,T2,T3)] =
    Shrink { case (t1,t2,t3) =>
      shrink(t1).map((_, t2, t3)) append
      shrink(t2).map((t1, _, t3)) append
      shrink(t3).map((t1, t2, _))
    }

  /** Shrink instance of 4-tuple */
  implicit def shrinkTuple4[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink
  ]: Shrink[(T1,T2,T3,T4)] =
    Shrink { case (t1,t2,t3,t4) =>
      shrink(t1).map((_, t2, t3, t4)) append
      shrink(t2).map((t1, _, t3, t4)) append
      shrink(t3).map((t1, t2, _, t4)) append
      shrink(t4).map((t1, t2, t3, _))
    }

  /** Shrink instance of 5-tuple */
  implicit def shrinkTuple5[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink, T5:Shrink
  ]: Shrink[(T1,T2,T3,T4,T5)] =
    Shrink { case (t1,t2,t3,t4,t5) =>
      shrink(t1).map((_, t2, t3, t4, t5)) append
      shrink(t2).map((t1, _, t3, t4, t5)) append
      shrink(t3).map((t1, t2, _, t4, t5)) append
      shrink(t4).map((t1, t2, t3, _, t5)) append
      shrink(t5).map((t1, t2, t3, t4, _))
    }

  /** Shrink instance of 6-tuple */
  implicit def shrinkTuple6[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink, T5:Shrink, T6:Shrink
  ]: Shrink[(T1,T2,T3,T4,T5,T6)] =
    Shrink { case (t1,t2,t3,t4,t5,t6) =>
      shrink(t1).map((_, t2, t3, t4, t5, t6)) append
      shrink(t2).map((t1, _, t3, t4, t5, t6)) append
      shrink(t3).map((t1, t2, _, t4, t5, t6)) append
      shrink(t4).map((t1, t2, t3, _, t5, t6)) append
      shrink(t5).map((t1, t2, t3, t4, _, t6)) append
      shrink(t6).map((t1, t2, t3, t4, t5, _))
    }

  /** Shrink instance of 7-tuple */
  implicit def shrinkTuple7[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink, T5:Shrink, T6:Shrink, T7:Shrink
  ]: Shrink[(T1,T2,T3,T4,T5,T6,T7)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7) =>
      shrink(t1).map((_, t2, t3, t4, t5, t6, t7)) append
      shrink(t2).map((t1, _, t3, t4, t5, t6, t7)) append
      shrink(t3).map((t1, t2, _, t4, t5, t6, t7)) append
      shrink(t4).map((t1, t2, t3, _, t5, t6, t7)) append
      shrink(t5).map((t1, t2, t3, t4, _, t6, t7)) append
      shrink(t6).map((t1, t2, t3, t4, t5, _, t7)) append
      shrink(t7).map((t1, t2, t3, t4, t5, t6, _))
    }

  /** Shrink instance of 8-tuple */
  implicit def shrinkTuple8[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink, T5:Shrink, T6:Shrink,
    T7:Shrink, T8:Shrink
  ]: Shrink[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8) =>
      shrink(t1).map((_, t2, t3, t4, t5, t6, t7, t8)) append
      shrink(t2).map((t1, _, t3, t4, t5, t6, t7, t8)) append
      shrink(t3).map((t1, t2, _, t4, t5, t6, t7, t8)) append
      shrink(t4).map((t1, t2, t3, _, t5, t6, t7, t8)) append
      shrink(t5).map((t1, t2, t3, t4, _, t6, t7, t8)) append
      shrink(t6).map((t1, t2, t3, t4, t5, _, t7, t8)) append
      shrink(t7).map((t1, t2, t3, t4, t5, t6, _, t8)) append
      shrink(t8).map((t1, t2, t3, t4, t5, t6, t7, _))
    }

  /** Shrink instance of 9-tuple */
  implicit def shrinkTuple9[
    T1:Shrink, T2:Shrink, T3:Shrink, T4:Shrink, T5:Shrink, T6:Shrink,
    T7:Shrink, T8:Shrink, T9:Shrink
  ]: Shrink[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8,t9) =>
      shrink(t1).map((_, t2, t3, t4, t5, t6, t7, t8, t9)) append
      shrink(t2).map((t1, _, t3, t4, t5, t6, t7, t8, t9)) append
      shrink(t3).map((t1, t2, _, t4, t5, t6, t7, t8, t9)) append
      shrink(t4).map((t1, t2, t3, _, t5, t6, t7, t8, t9)) append
      shrink(t5).map((t1, t2, t3, t4, _, t6, t7, t8, t9)) append
      shrink(t6).map((t1, t2, t3, t4, t5, _, t7, t8, t9)) append
      shrink(t7).map((t1, t2, t3, t4, t5, t6, _, t8, t9)) append
      shrink(t8).map((t1, t2, t3, t4, t5, t6, t7, _, t9)) append
      shrink(t9).map((t1, t2, t3, t4, t5, t6, t7, t8, _))
    }

  implicit def shrinkEither[T1:Shrink, T2:Shrink]: Shrink[Either[T1, T2]] =
    Shrink { x =>
      x.fold(shrink(_).map(Left(_)), shrink(_).map(Right(_)))
    }

  implicit val shrinkFiniteDuration: Shrink[FiniteDuration] =
    xmap[Long, FiniteDuration](Duration.fromNanos, _.toNanos)

  implicit val shrinkDuration: Shrink[Duration] = Shrink {
    case d: FiniteDuration => shrinkFiniteDuration.shrink(d)
    case _ => Stream.empty
  }

  /** Transform a Shrink[T] to a Shrink[U] where T and U are two isomorphic types
    *  whose relationship is described by the provided transformation functions.
    *  (exponential functor map) */
  def xmap[T, U](from: T => U, to: U => T)(implicit st: Shrink[T]): Shrink[U] = Shrink[U] { (u: U) =>
    st.shrink(to(u)).map(from)
  }
}

final class ShrinkIntegral[T](implicit ev: Integral[T]) extends Shrink[T] {
  import ev.{ fromInt, quot, negate, equiv, zero, lt, minus }

  val two = fromInt(2)

  // We shrink x to ceil(x * (1 - 1/2^i)) for i = 0,1,….  We also shrink x
  // to -x if x < 0 < -x (implying x != MinValue for two's complement types).

  // We assume that x - ((((x/2)/2)/...)/2) = x for some repetition count;
  // otherwise shrinking may diverge.  It holds if x - 0 = x and 0 is closer
  // to x/2 than to x and there are finitely many values y such that 0 is
  // closer to y than to x: then the sequence x, x/2, (x/2)/2, ... eventually
  // arrives at 0, but then (x - x/2/2/.../2) = x - 0 = x.

  private def bisectFromZeroToX(x: T, current: T): Stream[T] = {
    val head = minus(x, current)

    if (equiv(head, x)) Stream.empty
    else head #:: bisectFromZeroToX(x, quot(current, two))
  }

  def shrink(x: T): Stream[T] = {
    lazy val approach = bisectFromZeroToX(x, x)
    if (lt(x, zero) && lt(zero, negate(x))) negate(x) #:: approach
    else approach
  }
}

final class ShrinkFractional[T](implicit ev: Fractional[T]) extends Shrink[T] {
  import ev.{ fromInt, abs, zero, one, div, lteq, negate, lt }

  val two = fromInt(2)

  // this makes some assumptions, namely that we can support 1e-5 as a
  // fractional value. fortunately, if that is not true, it's likely
  // that our divisions will converge to zero quickly enough anyway.
  val small = div(one, fromInt(100000))
  def closeToZero(x: T): Boolean = lteq(abs(x), small)

  // assumes x is not close to zero
  private def halves(x: T): Stream[T] = {
    val q = div(x, two)
    if (closeToZero(q)) Stream(zero)
    else q #:: negate(q) #:: halves(q)
  }

  // this catches things like NaN and Infinity. it's not clear that we
  // should be shrinking those values, since they are sentinels.
  def isUnusual(x0: T): Boolean = {
    val x = abs(x0)
    // the negation here is important -- if we wrote this as gteq(...)
    // then it would not handle the NaN case correctly.
    !lt(div(x, two), x)
  }

  def shrink(x: T): Stream[T] =
    if (closeToZero(x) || isUnusual(x)) Stream.empty[T]
    else halves(x)
}
