package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]extends Predicate1[Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]]{
	def constrain[T](that: Constraint1[T]) = new Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
		def apply(pred: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = Constraint19.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,_)))
	}

	def suchThat(pred: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = apply(pred)
}