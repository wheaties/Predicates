package com.wheaties.function

/**
 * Evaulation defines the iteration strategy
 */
trait EvalIteration[A]{
  self: Logic[A] =>

  private[function] case class Continue[C](value: C, path: Node[C,A]) extends Output

  protected[function] def iterate(next: Output) ={
    val Continue(value, node) = next

    node match {
      case Leaf(Elem(pred,func)) => List(if(pred(value)) Pass(func(value)) else Fail)
      case Root(func, nodes) =>
        val out = func(value)
        nodes.map(Continue(out, _))
      case Tree(Elem(pred, func), nodes) => if(pred(value)) nodes.map(Continue(func(value), _)) else List(Fail)
    }
  }
}

/**
 * Eval performs a depth-first traversal of the nodes.
 */
case class Eval[A,B](base: Node[A,B]) extends DepthFirst[A,B,B] with EvalIteration[B]{
  def apply(arg0: A) = base match {
    case Leaf(Elem(pred, func)) => if(pred(arg0)) Pass(func(arg0)) else Fail
    case Root(func, nodes) => process(nodes.map(Continue(func(arg0), _)))
    case Tree(Elem(pred, func), nodes) => if(pred(arg0)) process(nodes.map(Continue(func(arg0), _))) else Fail
  }
}

case class Conditionally[A,B](base: Node[A,B]) extends AllPaths[A,B,B] with EvalIteration[B]{
  def apply(arg0: A)= base match{
    case Leaf(Elem(pred, func)) => List(if(pred(arg0)) Pass(func(arg0)) else Fail)
    case Root(func, nodes) => process(nodes.map(Continue(func(arg0), _)), Nil)
    case Tree(Elem(pred, func), nodes) => if(pred(arg0)) process(nodes.map(Continue(func(arg0), _)), Nil) else List(Fail)
  }
}