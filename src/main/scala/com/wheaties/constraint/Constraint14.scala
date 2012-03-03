package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]extends Predicate1[Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]]{
	def constrain[O](that: Constraint1[O]) = new Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
		def apply(pred: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = Constraint14.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,_)))
	}

	def suchThat(pred: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = apply(pred)
}