package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]extends Predicate1[Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]]{
	def constrain[P](that: Constraint1[P]) = new Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
		def apply(pred: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = Constraint15.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,_)))
	}

	def suchThat(pred: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]) = apply(pred)
}