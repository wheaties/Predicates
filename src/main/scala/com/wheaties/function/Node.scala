package com.wheaties.function

import com.wheaties.predicate.predicate._

sealed trait Node[A,B]{
  def fmap[C](f: B => C):Node[A,C]
  def defined(arg0: A):Boolean
}

case class Root[A,B,C](func: A => B, nodes: List[Node[B,C]]) extends Node[A,C]{
  def fmap[D](f: C => D) = copy(nodes = nodes.map(_.fmap(f)))
  def defined(arg0: A) ={
    val out = func(arg0)
    nodes.exists(_.defined(out))
  }
}

case class Tree[A,B,C](elem: Elem[A,B], nodes: List[Node[B,C]]) extends Node[A,C]{
  def fmap[D](f: C => D) = Tree(elem, nodes.map(_.fmap(f)))
  def defined(arg0: A) = if(elem.pred(arg0)){
    val out = elem.func(arg0)
    nodes.exists(_.defined(out))
  }
  else false
}

case class Leaf[A,B](elem: Elem[A,B]) extends Node[A,B]{
  def fmap[C](f: B => C) = Leaf(elem.fmap(f))
  def defined(arg0: A) = elem.pred(arg0)
}

case class Elem[A,B](pred: Predicate[A], func: A => B){
  def fmap[C](f: B => C) = Elem(pred, func andThen f)
}