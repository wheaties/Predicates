package com.wheaties.function

trait Result[+A]

case class Pass[+A](value: A) extends Result[A]
case object Fail extends Result[Nothing]