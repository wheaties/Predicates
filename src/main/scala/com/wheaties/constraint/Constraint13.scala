package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint13[A,B,C,D,E,F,G,H,I,J,K,L,M]extends Predicate1[Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]]{
	def constrain[N](that: Constraint1[N]) = new Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M]{
		def apply(pred: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = Constraint13.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,_)))
	}

	def suchThat(pred: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = apply(pred)
}