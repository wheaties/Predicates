package com.wheaties.function

import annotation.tailrec

trait Logic[A]{
  type Output = Result[A]
}

trait DepthFirst[A,B,C] extends (A => Result[C]) with Logic[C] {
  val base: Node[A,B]

  @tailrec final protected[function] def process(init: List[Output]):Output = init match{
    case Pass(_) :: xs => init.head
    case Fail :: xs => process(xs)
    case x :: xs => process(iterate(x) ::: xs)
    case Nil => Fail
  }

  protected[function] def iterate(value: Output): List[Output]
}

trait AllPaths[A,B,C] extends (A => List[Result[C]]) with Logic[C]{
  val base: Node[A,B]

  @tailrec final protected[function] def process(init: List[Output], acc: List[Output]):List[Output] ={
    init match{
      case Pass(_) :: xs => process(xs, init.head :: acc)
      case Fail :: xs => process(xs, acc)
      case x :: xs => process(xs ::: iterate(x), acc)
      case Nil => acc
    }
  }

  protected[function] def iterate(value: Output): List[Output]
}