package com.github.nhweston.group.instance

import com.github.nhweston.group.typeclass.*

given emptyTupleGroup: Group[EmptyTuple] with

  def plus(x: EmptyTuple, y: EmptyTuple) = EmptyTuple

  def negate(x: EmptyTuple) = EmptyTuple

  def zero = EmptyTuple

given emptyTupleFinite: Finite[EmptyTuple] with

  lazy val values = Iterable(EmptyTuple)

given tupleGroup[H, T <: Tuple](using groupH: Group[H], groupT: Group[T]): Group[H *: T] with

  def plus(x: H *: T, y: H *: T) =
    val xh *: xt = x
    val yh *: yt = y
    val h = groupH.plus(xh, yh)
    val t = groupT.plus(xt, yt)
    h *: t

  def negate(x: H *: T) =
    val xh *: xt = x
    val h = groupH.negate(xh)
    val t = groupT.negate(xt)
    h *: t

  def zero =
    groupH.zero *: groupT.zero

given tupleFinite[H, T <: Tuple](using finiteH: Finite[H], finiteT: Finite[T]): Finite[H *: T] with

  override lazy val values =
    for {
      h <- finiteH.values
      t <- finiteT.values
    } yield h *: t
