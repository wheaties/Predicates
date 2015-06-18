package com.wheaties

import com.wheaties.predicate.ops._

package object predicate
  extends FunctionOps1
  with FunctionOps2
  with FunctionOps3
  with FunctionOps4
  with FunctionOps5
  with FunctionOps6
  with FunctionOps7
  with FunctionOps8
  with FunctionOps9
  with FunctionOps10
  with FunctionOps11
  with FunctionOps12
  with FunctionOps13
  with FunctionOps14
  with FunctionOps15
  with FunctionOps16
  with FunctionOps17
  with FunctionOps18
  with FunctionOps19
  with FunctionOps20
  with FunctionOps21
  with FunctionOps22 {

  implicit class SugarIs[A](value: A){
    def is(pred: A => Boolean) = pred(value)
  }

  implicit class SugarIsF[T1, R](f: T1 => R){
    def is(pred: R => Boolean): T1 => Boolean = f andThen pred
  }
}