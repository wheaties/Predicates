package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]extends Predicate1[Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]]{
	def constrain[V](that: Constraint1[V]) = new Constraint22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
		def apply(pred: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = Constraint21.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,_)))
	}

	def suchThat(pred: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = apply(pred)
}