package com.wheaties.predicate

trait Predicate7[A,B,C,D,E,F,G] extends Function7[A,B,C,D,E,F,G,Boolean] {
	def or(that: Predicate7[A,B,C,D,E,F,G]) = Or7(this, that)
	def orNot(that: Predicate7[A,B,C,D,E,F,G]) = OrNot7(this, that)
	def and(that: Predicate7[A,B,C,D,E,F,G]) = And7(this, that)
	def andNot(that: Predicate7[A,B,C,D,E,F,G]) = AndNot7(this, that)
	def xor(that: Predicate7[A,B,C,D,E,F,G]) = Xor7(this, that)
	def nand(that: Predicate7[A,B,C,D,E,F,G]) = Nand7(this, that)
	def nor(that: Predicate7[A,B,C,D,E,F,G]) = Nor7(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G):Boolean
}

trait CompoundPredicate7[A,B,C,D,E,F,G] extends Predicate7[A,B,C,D,E,F,G]{
  val pred1: Predicate7[A,B,C,D,E,F,G]
  val pred2: Predicate7[A,B,C,D,E,F,G]
}

object CompoundPredicate7{
  def unapply[A,B,C,D,E,F,G](pred: CompoundPredicate7[A,B,C,D,E,F,G]):Option[(Predicate7[A,B,C,D,E,F,G], Predicate7[A,B,C,D,E,F,G])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class OrNot7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class And7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class AndNot7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class Xor7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class Nand7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6))
}

case class Nor7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6))
}

case class Not7[A,B,C,D,E,F,G](pred: Predicate7[A,B,C,D,E,F,G]) extends Predicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case object Always7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any) = true
}

case object Never7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any) = false
}
