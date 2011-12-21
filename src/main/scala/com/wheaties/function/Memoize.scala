package com.wheaties.function

trait MemoizeIteration[A,B]{
  self: Logic[(B,A => B)] =>

  private[function] case class Continue[C](value: C, cont: A => C, path: Node[C,B]) extends Output

  protected[function] def iterate(next: Output) ={
    val Continue(value, exec, node) = next

    node match{
      case Leaf(Elem(pred, func)) =>
        val out = func(value)
        List(if(pred(out)) Pass(out, func compose exec) else Fail)
      case Root(func, nodes) =>
        val out = func(value)
        nodes.map(Continue(out, func compose exec, _))
      case Tree(Elem(pred, func), nodes) =>
        val out = func(value)
        if(pred(out)){
          nodes.map(Continue(out, func compose exec, _))
        }
        else List(Fail)
    }
  }
}

case class Memoize[A,B](base: Node[A,B]) extends DepthFirst[A,B,(B,A => B)] with MemoizeIteration[A,B]{
  def apply(arg0: A)= base match{
    case Leaf(Elem(pred, func)) => if(pred(arg0)) Pass(func(arg0), func) else Fail
    case Root(func, nodes) =>
      val out = func(arg0)
      process(nodes.map(Continue(out, func, _)))
    case Tree(Elem(pred, func), nodes) => if(pred(arg0)){
      val out = func(arg0)
      process(nodes.map(Continue(out, func, _)))
    } else Fail
  }
}

case class MemoizeAll[A,B](base: Node[A,B]) extends AllPaths[A,B,(B,A => B)] with MemoizeIteration[A,B]{
  def apply(arg0: A)= base match{
    case Leaf(Elem(pred, func)) => List(if(pred(arg0)) Pass(func(arg0), func) else Fail)
    case Root(func, nodes) =>
      val out = func(arg0)
      process(nodes.map(Continue(out, func, _)), Nil)
    case Tree(Elem(pred, func), nodes) => if(pred(arg0)){
      val out = func(arg0)
      process(nodes.map(Continue(out, func, _)), Nil)
    } else List(Fail)
  }
}