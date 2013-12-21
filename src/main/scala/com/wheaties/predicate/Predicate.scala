package com.wheaties.predicate

//TODO: Add in hashCode, equals, toString, ## so that these classes are transparent
package object predicate{
  type Predicate[A] = Predicate1[A]
  //type Not[A] = Not1[A]

  def not(value: Boolean) = !value
  implicit def sugarIs[A](value: A) = new{ def is(pred: A => Boolean) = pred(value) }
}