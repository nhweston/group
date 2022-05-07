package com.github.nhweston.group.typeclass

import scala.annotation.tailrec

/** A finite group over `A`. */
trait Group[A]:

  /** An iterable yielding all constructible values of `A`. */
  def values: Iterable[A]

  /** The group operation. */
  def plus(x: A, y: A): A

  /** Returns the negation of `x`. */
  def negate(x: A): A

  /** The identity element. */
  def zero: A

  /** Adds the negation of `y` to `x`. */
  def minus(x: A, y: A): A =
    plus(x, negate(y))

  /** Returns the order of `x`. */
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

  /**
   * A minimal set of generators for `A`. It is useful to have this cached as
   * it is required for many algorithms. Instantiators are encouraged to
   * override this if a more efficient implementation is possible.
   */
  lazy val gtors: Set[A] =
    @tailrec
    def aux(
      gtors: Set[A],
      gted: Set[A],
      ungted: Set[A],
    ): Set[A] =
      ungted.headOption match
        case Some(gtor) =>
          val gtorsNext = gtors + gtor
          @tailrec
          def generate(
            queue: Seq[A],
            gted: Set[A],
            ungted: Set[A],
          ): (Set[A], Set[A]) =
            queue match
              case x +: tl =>
                val elems = gtorsNext.map(y => plus(x, y))
                val queueNext = queue ++ (elems & ungted)
                val gtedNext = gted ++ elems
                val ungtedNext = ungted -- elems
                generate(queueNext, gtedNext, ungtedNext)
              case Seq() =>
                (gted, ungted)
          val (gtedNext, ungtedNext) = generate(gted.toSeq, gted, ungted)
          aux(gtorsNext, gtedNext, ungtedNext)
        case None =>
          gtors
    aux(Set.empty, Set(zero), values.toSet - zero)
