package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]extends Predicate1[Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]]{
	def constrain[U](that: Constraint1[U]) = new Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
		def apply(pred: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = Constraint20.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,_)))
	}

	def suchThat(pred: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = apply(pred)
}