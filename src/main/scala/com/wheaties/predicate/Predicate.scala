package com.wheaties.predicate

package object predicate{
  implicit def sugarIs[A](value: A) = new{ def is(pred: A => Boolean) = pred(value) }
}