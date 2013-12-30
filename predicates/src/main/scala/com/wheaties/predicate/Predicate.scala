package com.wheaties

import com.wheaties.ops._

package object predicate {
  import FunctionOps1._
  import FunctionOps2._

  implicit class SugarIs[A](value: A){
    def is(pred: A => Boolean) = pred(value)
  }

  implicit class SugarIsF[T1, R](f: T1 => R){
    def is(pred: R => Boolean): T1 => Boolean = f andThen pred
  }
}