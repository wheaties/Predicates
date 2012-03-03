package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint12[A,B,C,D,E,F,G,H,I,J,K,L]extends Predicate1[Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]]{
	def constrain[M](that: Constraint1[M]) = new Constraint13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
		def apply(pred: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = Constraint12.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,_)))
	}

	def suchThat(pred: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = apply(pred)
}