package com.wheaties.choice.setter

import com.wheaties.choice.iteration.IterationScheme

trait Setter[A,B]{
  def set(collection: A, value: B, scheme: IterationScheme): A
}