package com.github.nhweston.grp.syntax

import com.github.nhweston.grp.typeclass.Group
import scala.annotation.targetName

extension [A](self: A)(using grp: Group[A])

  def plus(other: A): A = grp.plus(self, other)

  def +(other: A): A = plus(other)

  def minus(other: A): A = grp.minus(self, other)

  def -(other: A): A = minus(other)

  def times(n: Int): A = grp.times(self, n)

  def *(n: Int): A = times(n)

  def negate: A = grp.negate(self)

  def unary_- : A = negate

  def abs: Int = grp.abs(self)
