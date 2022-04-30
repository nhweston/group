package com.github.nhweston.grp.instance

import com.github.nhweston.grp.typeclass.Group
import com.github.nhweston.grp.data.Mod

given modInstances[M <: Int](using m: ValueOf[M]): Group[Mod[M]] =
  new Group[Mod[M]]:

    assert(m.value > 0)

    override def plus(x: Mod[M], y: Mod[M]) =
      Mod(x.value + y.value)

    override def negate(x: Mod[M]) =
      Mod(-x.value)

    override def zero =
      Mod(0)
