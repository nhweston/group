package com.github.nhweston.group.data

/** Represents an isomorphism between `A` and `B`. */
class Iso[A, B] private[group] (
  phi: A => B,
  phiInv: B => A,
) extends Hom[A, B](phi):

  def reverse: Iso[B, A] = new Iso(phiInv, phi)

/** Represents an automorphism of `A`. */
type Aut[A] = Iso[A, A]
