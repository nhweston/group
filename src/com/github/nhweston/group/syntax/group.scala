package com.github.nhweston.group.syntax

import com.github.nhweston.group.typeclass.Group

extension [A](self: A)(using group: Group[A])

  def plus(other: A): A = group.plus(self, other)

  def +(other: A): A = plus(other)

  def minus(other: A): A = group.minus(self, other)

  def -(other: A): A = minus(other)

  def times(n: Int): A = group.times(self, n)

  def *(n: Int): A = times(n)

  def negate: A = group.negate(self)

  def unary_- : A = negate

  def abs: Int = group.abs(self)
