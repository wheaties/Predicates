package com.wheaties.predicate

import org.scalatest.{ShouldMatchers, Matchers, WordSpecLike}
import com.wheaties.predicate._

class PredicateSpec extends WordSpecLike
  with Matchers
  with ShouldMatchers {

  "Predicate" should {
    "have proper `and` method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) and Modulo(3, 0))
      filtered should equal(List(6))
    }

    "`and` should be lazy" in new SpecExamples {
      val predicate = modulo_2 and modulo_3
      predicate(1) should equal(false)
      modulo_2.applied should equal(true)
      modulo_3.applied should equal(false)
    }

    "have proper or method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) or Modulo(3, 0))
      filtered should equal(List(2, 3, 4, 6))
    }

    "`or` should be lazy" in new SpecExamples {
      val predicate = modulo_2 or modulo_3
      predicate(2) should equal(true)
      modulo_2.applied should equal(true)
      modulo_3.applied should equal(false)
    }

    "have proper xor method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) xor Modulo(3, 0))
      filtered should equal(List(2, 3, 4))
    }

    "have proper nor method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) nor Modulo(3, 0))
      filtered should equal(List(5))
    }

    "have proper nand method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) nand Modulo(3, 0))
      filtered should equal(List(2, 3, 4, 5))
    }

    "have proper nxor method" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) nxor Modulo(3, 0))
      filtered should equal(List(5, 6))
    }

    "are left-associative by default" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) or Modulo(3, 0) and LessThen(6))
      filtered should equal(List(2, 3, 4))
    }

    "can be evaluated in different order by specifing parenthesis" in new SpecExamples {
      val filtered = sampleList.filter(Modulo(2, 0) or (Modulo(3, 0) and LessThen(6)))
      filtered should equal(List(2, 3, 4, 6))
    }
  }

  trait SpecExamples {
    trait AppliedAware {
      var _applied = false
      def applied: Boolean = _applied
    }

    case class LessThen(x: Int) extends Function[Int,Boolean] with AppliedAware {
      def apply(arg: Int) = {
        _applied = true
        arg < x
      }
    }

    case class Modulo(group: Int, x: Int) extends Function[Int,Boolean] with AppliedAware {
      def apply(arg: Int) = {
        _applied = true
        (arg % group) == x
      }
    }

    val sampleList = (2 to 6).toList
    val modulo_2 = Modulo(2, 0)
    val modulo_3 = Modulo(3, 0)
  }
  
}
