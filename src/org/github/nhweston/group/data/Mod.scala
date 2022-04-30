package com.github.nhweston.group.data

class Mod[M <: Int] private (val value: Int, val modulus: M):

  assert(modulus > 0)
  assert(0 <= value)
  assert(value < modulus)

  override def toString = value.toString

object Mod:

  def apply[M <: Int](value: Int)(using m: ValueOf[M]): Mod[M] =
    val modulus = m.value
    if modulus <= 0 then
      throw IllegalArgumentException("modulus must be positive")
    new Mod(math.floorMod(value, modulus), modulus)
