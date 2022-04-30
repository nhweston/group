package com.github.nhweston.grp.typeclass

import scala.annotation.tailrec

/** A group over `A`. */
trait Group[A]:

  /** The group operation. */
  def plus(x: A, y: A): A

  /** Returns the negation of `x`. */
  def negate(x: A): A

  /** The identity element. */
  def zero: A

  /** Adds the negation of `y` to `x`. */
  def minus(x: A, y: A): A =
    plus(x, negate(y))

  /** Returns the order of `x`. May not terminate for infinite groups. */
  def abs(x: A): Int =
    @tailrec
    def aux(y: A, out: Int): Int =
      if y == zero then out
      else aux(plus(y, x), out + 1)
    aux(x, 1)

  /** Adds `x` to itself `n` times. */
  def times(x: A, n: Int): A =
    if n < 0 then
      times(negate(x), -n)
    else if n == 0 then
      zero
    else
      def aux(n: Int): A =
        if n == 1 then
          x
        else
          val half = aux(n / 2)
          val halfTimesTwo = plus(half, half)
          if n % 2 == 0 then halfTimesTwo
          else plus(halfTimesTwo, x)
      aux(n)
