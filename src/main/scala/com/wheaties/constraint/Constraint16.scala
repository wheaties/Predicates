package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]extends Predicate1[Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]]{
	def constrain[Q](that: Constraint1[Q]) = new Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
		def apply(pred: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = Constraint16.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,_)))
	}

	def suchThat(pred: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = apply(pred)
}