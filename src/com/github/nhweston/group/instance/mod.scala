package com.github.nhweston.group.instance

import com.github.nhweston.group.data.Mod
import com.github.nhweston.group.typeclass.Group

given modGroup[M <: Int](using m: ValueOf[M]): Group[Mod[M]] with

  lazy val values =
    (0 until m.value).map(n => Mod(n))

  def plus(x: Mod[M], y: Mod[M]) =
    Mod(x.value + y.value)

  override def negate(x: Mod[M]) =
    Mod(-x.value)

  override lazy val zero =
    Mod(0)

  override lazy val gtors =
    if m.value > 1 then Set(Mod(1))
    else Set.empty
