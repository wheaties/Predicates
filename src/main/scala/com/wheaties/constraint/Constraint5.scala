package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint5[A,B,C,D,E]extends Predicate1[Function5[A,B,C,D,E,Boolean]]{
	def constrain[F](that: Constraint1[F]) = new Constraint6[A,B,C,D,E,F]{
		def apply(pred: Function6[A,B,C,D,E,F,Boolean]) = Constraint5.this((x0,x1,x2,x3,x4) => that(pred(x0,x1,x2,x3,x4,_)))
	}

	def suchThat(pred: Function5[A,B,C,D,E,Boolean]) = apply(pred)
}