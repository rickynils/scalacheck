/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2021 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

// Generates Arbitrary instance for tuples and functions

case class GeneratedFile(name: String, code: String)

object codegen {
  
  def csv(s: Seq[String]) = s mkString ", "
  def typed(s: Seq[(String,String)]) = s map { case (v,t) => s"$v:$t"}
  def wrapType(t: String, w:String) = s"$w[$t]"
  
  def idents(name: String, i: Int) = 1 to i map(i0 => name+i0)
  def types(i: Int) = idents("T",i).mkString(",")
  
  def valsWithTypes(i: Int) =
    idents("t",i) zip
    idents("T",i) map
    { case (v,t) => s"$v:$t"} mkString ","
  
  def wrappedArgs(wrapper: String, i: Int) = 
    csv(typed(
      idents(wrapper.take(1).toLowerCase,i) zip
      idents("T",i).map(wrapType(_,wrapper))
    ))
  
  def flatMappedGenerators(i: Int, s: Seq[(String,String)]): String =
    s.init.foldRight(s"${s.last._2}.map { ${s.last._1} => (${vals(i)}) }") {
      case ((t, g), acc) =>
        val T = t.toUpperCase
        s"${g}.flatMap(new Function1[${T}, Gen[(${types(i)})]] { def apply(${t}: ${T}): Gen[(${types(i)})] = $acc })"
    }
  
  def vals(i: Int) = csv(idents("t",i))
  
  def coImplicits(i: Int) = (1 to i).map(n => s"co$n: Cogen[T$n]").mkString(",")
  
  def fnArgs(i: Int) = (1 to i).map(n => s"t$n: T$n").mkString(",")
  
  def nestedPerturbs(i: Int) =
    (1 to i).foldLeft("seed0") { (s, n) => s"co$n.perturb($s, t$n)" }
  
  def fntype(i: Int) =
    s"(${types(i)}) => Z"
  
  def shrinkTuple(i: Int): String =
    s"""|  /** Shrink instance of ${i}-tuple */
        |  implicit def shrinkTuple${i}[
        |    ${shrinkTypes(i)}
        |  ]: Shrink[(${types(i)})] =
        |    Shrink { case (${vals(i)}) =>
        |      ${shrinkEachComponentOneByOne(i)}
        |    }""".stripMargin

  def shrinkTypes(n: Int): String =
    (1 to n).map(i => s"T${i}:Shrink").mkString(", ")

  def shrinkEachComponentOneByOne(n: Int): String =
    (1 to n).map(shrinkSingleComponent(n, _)).mkString(" append\n      ")

  def shrinkSingleComponent(n: Int, i: Int): String =
    s"""Shrink.shrink(t${i}).map((${partialShrinkTuple(n, i)}))"""

  def partialShrinkTuple(n: Int, i: Int): String =
    (1 to n).map(k => if (k == i) "_" else s"t${k}").mkString(", ")

  def arbfn(i: Int) = s"""
    |  /** Arbitrary instance of Function${i} */
    |  implicit def arbFunction${i}[${types(i)},Z](implicit g: Arbitrary[Z], ${coImplicits(i)}): Arbitrary[${fntype(i)}] =
    |    Arbitrary(Gen.function${i}(g.arbitrary))
    |""".stripMargin
  
  def genfn(i: Int) = s"""
    |  /** Gen creator for Function${i} */
    |  def function${i}[${types(i)},Z](g: Gen[Z])(implicit ${coImplicits(i)}): Gen[${fntype(i)}] =
    |    Gen.gen { (p, seed0) =>
    |      val f: ${fntype(i)} =
    |        (${fnArgs(i)}) => g.pureApply(p, ${nestedPerturbs(i)})
    |      Gen.r[${fntype(i)}](
    |        r = Some(f),
    |        sd = seed0.next
    |      )
    |    }
    |""".stripMargin
  
  def tuple(i: Int) = {
    val gens = idents("a",i).map(_+".arbitrary") mkString ","
  
    if (i == 1) {
      s"""
          |  /** Arbitrary instance of ${i}-Tuple */
          |  implicit def arbTuple$i[${types(i)}](implicit
          |    ${wrappedArgs("Arbitrary",i)}
          |  ): Arbitrary[Tuple$i[${types(i)}]]
          |    = Arbitrary(${gens}.map(Tuple1(_)))
          |""".stripMargin
    } else {
      s"""
          |  /** Arbitrary instance of ${i}-Tuple */
          |  implicit def arbTuple$i[${types(i)}](implicit
          |    ${wrappedArgs("Arbitrary",i)}
          |  ): Arbitrary[Tuple$i[${types(i)}]]
          |    = Arbitrary(Gen.zip(${gens}))
          |""".stripMargin
    }
  }
  
  def zip(i: Int) = {
    val gens = flatMappedGenerators(i, idents("t",i) zip idents("g",i))
    s"""
        |  /** Combines the given generators into one generator that produces a
        |   *  tuple of their generated values. */
        |  def zip[${types(i)}](
        |    ${wrappedArgs("Gen",i)}
        |  ): Gen[(${types(i)})] =
        |    $gens
        |""".stripMargin
  }
  
  def resultOf(i: Int) = {
    def delegate = idents("T",i).drop(1).map("_:"+_).mkString(",")
    s"""
        |  /** Takes a function and returns a generator that generates arbitrary
        |   *  results of that function by feeding it with arbitrarily generated input
        |   *  parameters. */
        |  def resultOf[${types(i)},R]
        |    (f: (${types(i)}) => R)
        |    (implicit
        |      ${wrappedArgs("Arbitrary",i)}
        |    ): Gen[R] = arbitrary[T1] flatMap {
        |      t => resultOf(f(t,$delegate))
        |    }
        |""".stripMargin
  }
  
  def tupleCogen(i: Int) = {
    s"""
        |  implicit final def tuple${i}[${types(i)}](implicit ${wrappedArgs("Cogen",i)}): Cogen[Tuple$i[${types(i)}]] =
        |    Cogen((seed, t) =>
        |      ${idents("c", i).zipWithIndex.foldLeft("seed"){
                 case (str, (c, n)) => s"$c.perturb($str, t._${n + 1})"
               }}
        |    )
        |""".stripMargin
  }
  
  /*
    implicit def cogenFunction2[A: Arbitrary, B: Arbitrary, Z: Cogen]: Cogen[(A, B) => Z] =
      Cogen { (seed, f) =>
        val r0 = arbitrary[A].doPureApply(params, seed)
        val r1 = arbitrary[B].doPureApply(params, r0.seed)
        Cogen[Z].perturb(r1.seed, f(r0.retrieve.get, r1.retrieve.get))
      }
   */
  
  def functionCogen(i: Int) = {
    def stanza(j: Int): String = {
      val s = if (j == 1) "seed" else s"r${j-1}.seed"
      s"      val r${j} = arbitrary[T$j].doPureApply(params, $s)\n"
    }
  
    val ap = (1 to i).map(j => s"r${j}.retrieve.get").mkString("f(", ", ", ")")
    val fn = s"Function$i[${types(i)}, Z]"
  
    s"""
        |  implicit final def function${i}[${types(i)}, Z](implicit ${wrappedArgs("Arbitrary",i)}, z: Cogen[Z]): Cogen[$fn] =
        |    Cogen { (seed: Seed, f: $fn) =>
        |${(1 to i).map(stanza).mkString}
        |      Cogen[Z].perturb(seed, $ap)
        |    }
        |""".stripMargin
  }
  
  val genAll: Seq[GeneratedFile] = Seq(
    GeneratedFile(
      "ShrinkArities.scala",
      s"""/**
          |Defines implicit [[org.scalacheck.Shrink]] instances for tuples
          |
          |Auto-generated using project/codegen.scala
          |*/
          |package org.scalacheck
          |
          |private[scalacheck] trait ShrinkArities{
          |
          |${2 to 22 map shrinkTuple mkString("\n\n")}
          |}
          |""".stripMargin),
    GeneratedFile(
      "ArbitraryArities.scala",
      s"""/**
          |Defines implicit [[org.scalacheck.Arbitrary]] instances for tuples and functions
          |
          |Auto-generated using project/codegen.scala
          |*/
          |package org.scalacheck
          |
          |private[scalacheck] trait ArbitraryArities{
          |  // Functions //
          |  ${1 to 22 map arbfn mkString ""}
          |
          |  // Tuples //
          |  ${1 to 22 map tuple mkString ""}
          |}""".stripMargin),
    GeneratedFile(
      "GenArities.scala",
      s"""/**
          |Defines zip and resultOf for all arities
          |
          |Auto-generated using project/codegen.scala
          |*/
          |package org.scalacheck
          |private[scalacheck] trait GenArities{
          |
          |  // genfn //
          |${1 to 22 map genfn mkString ""}
          |
          |  // zip //
          |${1 to 22 map zip mkString ""}
          |
          |  // resultOf //
          |  import Arbitrary.arbitrary
          |  def resultOf[T,R](f: T => R)(implicit a: Arbitrary[T]): Gen[R]
          |${2 to 22 map resultOf mkString ""}
          |}""".stripMargin
    ),
    GeneratedFile(
      "CogenArities.scala",
      s"""/**
          |Auto-generated using project/codegen.scala
          |*/
          |package org.scalacheck
          |private[scalacheck] abstract class CogenArities{
          |
          |  ${1 to 22 map tupleCogen mkString ""}
          |
          |  import Arbitrary.arbitrary
          |  import Gen.Parameters.{ default => params }
          |  import rng.Seed
          |
          |  ${1 to 22 map functionCogen mkString ""}
          |
          |}""".stripMargin
    )
  )
}
