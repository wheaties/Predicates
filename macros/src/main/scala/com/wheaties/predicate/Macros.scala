package com.wheaties.predicate

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {
	def hello: Unit = macro impl

  def impl(c: blackbox.Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"""println("Hello World")""")
  }
}
