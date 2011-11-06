package com.wheaties.predicate

/**
 * Factory object to produce "not" predicate modifier.
 */
object Not {
  def apply[A](pred: Predicate1[A]) = Not1(pred)
  def apply[A,B](pred: Predicate2[A,B]) = Not2(pred)
  def apply[A,B,C](pred: Predicate3[A,B,C]) = Not3(pred)
  def apply[A,B,C,D](pred: Predicate4[A,B,C,D]) = Not4(pred)
  def apply[A,B,C,D,E](pred: Predicate5[A,B,C,D,E]) = Not5(pred)
  def apply[A,B,C,D,E,F](pred: Predicate6[A,B,C,D,E,F]) = Not6(pred)
  def apply[A,B,C,D,E,F,G](pred: Predicate7[A,B,C,D,E,F,G]) = Not7(pred)
  def apply[A,B,C,D,E,F,G,H](pred: Predicate8[A,B,C,D,E,F,G,H]) = Not8(pred)
  def apply[A,B,C,D,E,F,G,H,I](pred: Predicate9[A,B,C,D,E,F,G,H,I]) = Not9(pred)
  def apply[A,B,C,D,E,F,G,H,I,J](pred: Predicate10[A,B,C,D,E,F,G,H,I,J]) = Not10(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = Not11(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) = Not12(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) = Not13(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = Not14(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = Not15(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = Not16(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = Not17(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Not18(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = Not19(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Not20(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = Not21(pred)
  def apply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](pred: Predicate22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]) = Not22(pred)
}