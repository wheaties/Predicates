package com.wheaties.choice

import org.scalatest.{WordSpec, Matchers}
import com.wheaties.predicate.Always1

class ChooseSpec extends WordSpec with Matchers{
  //TODO: These tests are wrong, Need to do this with "Always" and "Never"
  "Choose" should {
    val choose = Choose({x: Int => x % 2 == 0})

    "get" should {
      val out = choose get List(1, 2, 3)

      out should contain(2)
      out should have length(1)
    }
    "set" should{
      val out = choose set (List(1, 2, 3), 4)

      out should contain(1)
      out should contain(4)
      out should contain(3)
      out should have length(3)
    }
    "put back what you take out changes nothing" in{
      val out = choose set (List(1, 2, 3), choose get List(1, 2, 3))

      out should contain(1)
      out should contain(2)
      out should contain(3)
      out should have length(3)
    }
    "get back what you put in, as long as it satifies the condition" in {
      val out = choose get(choose set (List(1, 2, 3), 4))

      out should contain(4)
      out should have length(1)
    }
    "not get back what you put in, if it fails the condition" in {
      val out = choose get(choose set (List(1, 2, 3), 5))

      out should have length(0)
    }
    "putting in twice same as putting in once" in {
      val out = choose set(List(1, 2, 3), choose set (List(1, 2, 3), List(1, 2, 3)))

      out should contain(1)
      out should not contain(2)
      out should contain(3)
      out should have length(3)
    }
  }

  "Choose.set" should{
    val choose = Choose(Always1)

    "change everything" in {
      val out = choose set (List(1, 2, 3), List(4, 5, 6))

      out should contain(4)
      out should contain(5)
      out should contain(6)
    }
    "work" in {
      val out = choose set (Set(1, 2, 3), List(4, 5, 6))

      out should contain(4)
      out should contain(5)
      out should contain(6)
    }
    "substitute a value" in {
      val out = choose set (List(1, 2, 3), 4)

      out should contain(4)
      out should have length(3)
    }
  }

  "Choose.mod" should {
    val choose = Choose(Always1)

    "change everything" in {
      val out = choose mod (List(1, 2, 3), {x: Int => x + 1})

      out should contain(2)
      out should contain(3)
      out should contain(4)
      out should have length(3)
    }
  }

  //just in case I screw something up.
  "variance" should {
    val choose = Choose(Always1)

    "work" in{
      def that(in: Choose[Int]) = in get List(1, 2, 3)

      that(choose) should have length(3)
    }
  }
}
