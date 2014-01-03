package com.wheaties

import com.wheaties.predicate.ops._

package object predicate {
  import FunctionOps1._
  import FunctionOps2._
  import FunctionOps3._
  import FunctionOps4._
  import FunctionOps5._
  import FunctionOps6._
  import FunctionOps7._
  import FunctionOps8._
  import FunctionOps9._
  import FunctionOps10._
  import FunctionOps11._
  import FunctionOps12._
  import FunctionOps13._
  import FunctionOps14._
  import FunctionOps15._
  import FunctionOps16._
  import FunctionOps17._
  import FunctionOps18._
  import FunctionOps19._
  import FunctionOps20._
  import FunctionOps21._
  import FunctionOps22._

  implicit class SugarIs[A](value: A){
    def is(pred: A => Boolean) = pred(value)
  }

  implicit class SugarIsF[T1, R](f: T1 => R){
    def is(pred: R => Boolean): T1 => Boolean = f andThen pred
  }
}