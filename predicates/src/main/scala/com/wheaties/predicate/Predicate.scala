package com.wheaties

package object predicate{
  implicit class SugarIs[A](value: A){
    def is(pred: A => Boolean) = pred(value)
  }
}