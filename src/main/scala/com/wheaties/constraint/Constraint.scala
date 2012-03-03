package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

//TODO: add implicit conversion, _.forall() won't convert to a constraint otherwise
//TODO: add in implicit for predicate => constraint
trait Constraint1[A] extends Predicate1[(A => Boolean)] {
  def constrain[B](that: Constraint1[B]) = new Constraint2[A,B]{
    def apply(pred: Function2[A,B,Boolean]) = Constraint1.this(x1 => that(pred(x1, _)))
  }

  def suchThat(pred: A => Boolean) = apply(pred)
}

trait Constraint2[A,B] extends Predicate1[Function2[A,B,Boolean]]{
  def constrain[C](that: Constraint1[C]) = new Constraint3[A,B,C]{
    def apply(pred: Function3[A,B,C,Boolean]) = Constraint2.this((x1,x2) => that(pred(x1, x2, _)))
  }

  def suchThat(pred: Function2[A,B,Boolean]) = apply(pred)
}

trait Constraint3[A,B,C] extends Predicate1[Function3[A,B,C,Boolean]]{
  def constrain[D](that: Constraint1[D]) = new Constraint4[A,B,C,D]{
    def apply(pred: Function4[A,B,C,D,Boolean]) = Constraint3.this((x1,x2,x3) => that(pred(x1, x2, x3, _)))
  }

  def suchThat(pred: Function3[A,B,C,Boolean]) = apply(pred)
}

trait Constraint4[A,B,C,D]extends Predicate1[Function4[A,B,C,D,Boolean]]{
	def constrain[E](that: Constraint1[E]) = new Constraint5[A,B,C,D]{
		def apply(pred: Function4[A,B,C,D,Boolean]) = Constraint4.this((x0,x1,x2,x3) => that(pred(x0,x1,x2,x3,_)))
	}

	def suchThat(pred: Function4[A,B,C,D,Boolean]) = apply(pred)
}
trait Constraint5[A,B,C,D,E]extends Predicate1[Function5[A,B,C,D,E,Boolean]]{
	def constrain[F](that: Constraint1[F]) = new Constraint6[A,B,C,D,E]{
		def apply(pred: Function5[A,B,C,D,E,Boolean]) = Constraint5.this((x0,x1,x2,x3,x4) => that(pred(x0,x1,x2,x3,x4,_)))
	}

	def suchThat(pred: Function5[A,B,C,D,E,Boolean]) = apply(pred)
}
trait Constraint6[A,B,C,D,E,F]extends Predicate1[Function6[A,B,C,D,E,F,Boolean]]{
	def constrain[G](that: Constraint1[G]) = new Constraint7[A,B,C,D,E,F]{
		def apply(pred: Function6[A,B,C,D,E,F,Boolean]) = Constraint6.this((x0,x1,x2,x3,x4,x5) => that(pred(x0,x1,x2,x3,x4,x5,_)))
	}

	def suchThat(pred: Function6[A,B,C,D,E,F,Boolean]) = apply(pred)
}
trait Constraint7[A,B,C,D,E,F,G]extends Predicate1[Function7[A,B,C,D,E,F,G,Boolean]]{
	def constrain[H](that: Constraint1[H]) = new Constraint8[A,B,C,D,E,F,G]{
		def apply(pred: Function7[A,B,C,D,E,F,G,Boolean]) = Constraint7.this((x0,x1,x2,x3,x4,x5,x6) => that(pred(x0,x1,x2,x3,x4,x5,x6,_)))
	}

	def suchThat(pred: Function7[A,B,C,D,E,F,G,Boolean]) = apply(pred)
}
trait Constraint8[A,B,C,D,E,F,G,H]extends Predicate1[Function8[A,B,C,D,E,F,G,H,Boolean]]{
	def constrain[I](that: Constraint1[I]) = new Constraint9[A,B,C,D,E,F,G,H]{
		def apply(pred: Function8[A,B,C,D,E,F,G,H,Boolean]) = Constraint8.this((x0,x1,x2,x3,x4,x5,x6,x7) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,_)))
	}

	def suchThat(pred: Function8[A,B,C,D,E,F,G,H,Boolean]) = apply(pred)
}
trait Constraint9[A,B,C,D,E,F,G,H,I]extends Predicate1[Function9[A,B,C,D,E,F,G,H,I,Boolean]]{
	def constrain[J](that: Constraint1[J]) = new Constraint10[A,B,C,D,E,F,G,H,I]{
		def apply(pred: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = Constraint9.this((x0,x1,x2,x3,x4,x5,x6,x7,x8) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,_)))
	}

	def suchThat(pred: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = apply(pred)
}
trait Constraint10[A,B,C,D,E,F,G,H,I,J]extends Predicate1[Function10[A,B,C,D,E,F,G,H,I,J,Boolean]]{
	def constrain[K](that: Constraint1[K]) = new Constraint11[A,B,C,D,E,F,G,H,I,J]{
		def apply(pred: Function10[A,B,C,D,E,F,G,H,I,J,Boolean]) = Constraint10.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,_)))
	}

	def suchThat(pred: Function10[A,B,C,D,E,F,G,H,I,J,Boolean]) = apply(pred)
}
trait Constraint11[A,B,C,D,E,F,G,H,I,J,K]extends Predicate1[Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]]{
	def constrain[L](that: Constraint1[L]) = new Constraint12[A,B,C,D,E,F,G,H,I,J,K]{
		def apply(pred: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = Constraint11.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,_)))
	}

	def suchThat(pred: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = apply(pred)
}
trait Constraint12[A,B,C,D,E,F,G,H,I,J,K,L]extends Predicate1[Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]]{
	def constrain[M](that: Constraint1[M]) = new Constraint13[A,B,C,D,E,F,G,H,I,J,K,L]{
		def apply(pred: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = Constraint12.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,_)))
	}

	def suchThat(pred: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = apply(pred)
}
trait Constraint13[A,B,C,D,E,F,G,H,I,J,K,L,M]extends Predicate1[Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]]{
	def constrain[N](that: Constraint1[N]) = new Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M]{
		def apply(pred: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = Constraint13.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,_)))
	}

	def suchThat(pred: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = apply(pred)
}

trait Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]extends Predicate1[Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]]{
	def constrain[O](that: Constraint1[O]) = new Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
		def apply(pred: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = Constraint14.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,_)))
	}

	def suchThat(pred: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = apply(pred)
}
trait Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]extends Predicate1[Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]]{
	def constrain[P](that: Constraint1[P]) = new Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
		def apply(pred: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]) = Constraint15.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,_)))
	}

	def suchThat(pred: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]) = apply(pred)
}
trait Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]extends Predicate1[Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]]{
	def constrain[Q](that: Constraint1[Q]) = new Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
		def apply(pred: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = Constraint16.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,_)))
	}

	def suchThat(pred: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = apply(pred)
}
trait Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]extends Predicate1[Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]]{
	def constrain[R](that: Constraint1[R]) = new Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
		def apply(pred: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = Constraint17.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,_)))
	}

	def suchThat(pred: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = apply(pred)
}
trait Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]extends Predicate1[Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]]{
	def constrain[S](that: Constraint1[S]) = new Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
		def apply(pred: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = Constraint18.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,_)))
	}

	def suchThat(pred: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = apply(pred)
}
trait Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]extends Predicate1[Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]]{
	def constrain[T](that: Constraint1[T]) = new Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
		def apply(pred: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = Constraint19.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,_)))
	}

	def suchThat(pred: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = apply(pred)
}
trait Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]extends Predicate1[Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]]{
	def constrain[U](that: Constraint1[U]) = new Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
		def apply(pred: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = Constraint20.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,_)))
	}

	def suchThat(pred: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = apply(pred)
}
trait Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]extends Predicate1[Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]]{
	def constrain[V](that: Constraint1[V]) = new Constraint22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
		def apply(pred: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = Constraint21.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,_)))
	}

	def suchThat(pred: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = apply(pred)
}

trait Constraint22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]extends Predicate1[Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean]]{
	def suchThat(pred: Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean]) = apply(pred)
}