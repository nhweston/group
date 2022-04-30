package com.github.nhweston.group.instance

import com.github.nhweston.group.typeclass.Group

given emptyTupleInstances: Group[EmptyTuple] with

  override def plus(x: EmptyTuple, y: EmptyTuple) = EmptyTuple

  override def negate(x: EmptyTuple) = EmptyTuple

  override def zero = EmptyTuple

given tupleInstances[H, T <: Tuple](using groupH: Group[H], groupT: Group[T]): Group[H *: T] with

  override def plus(x: H *: T, y: H *: T) =
    val xh *: xt = x
    val yh *: yt = y
    val h = groupH.plus(xh, yh)
    val t = groupT.plus(xt, yt)
    h *: t

  override def negate(x: H *: T) =
    val xh *: xt = x
    val h = groupH.negate(xh)
    val t = groupT.negate(xt)
    h *: t

  override def zero =
    groupH.zero *: groupT.zero
