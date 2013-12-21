package com.wheaties.predicate

package object predicate{
  def not(value: Boolean) = !value
  implicit def sugarIs[A](value: A) = new{ def is(pred: A => Boolean) = pred(value) }
}