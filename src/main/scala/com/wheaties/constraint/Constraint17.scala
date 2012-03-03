package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]extends Predicate1[Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]]{
	def constrain[R](that: Constraint1[R]) = new Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
		def apply(pred: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = Constraint17.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,_)))
	}

	def suchThat(pred: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = apply(pred)
}