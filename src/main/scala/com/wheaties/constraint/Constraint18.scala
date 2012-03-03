package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]extends Predicate1[Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]]{
	def constrain[S](that: Constraint1[S]) = new Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
		def apply(pred: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = Constraint18.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,_)))
	}

	def suchThat(pred: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = apply(pred)
}