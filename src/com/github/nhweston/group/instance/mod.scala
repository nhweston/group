package com.github.nhweston.group.instance

import com.github.nhweston.group.typeclass.*
import com.github.nhweston.group.data.Mod

given modGroup[M <: Int](using m: ValueOf[M]): Group[Mod[M]] with

  lazy val values =
    (0 until m.value).map(n => Mod(n))

  def plus(x: Mod[M], y: Mod[M]) =
    Mod(x.value + y.value)

  def negate(x: Mod[M]) =
    Mod(-x.value)

  def zero =
    Mod(0)
