package com.github.nhweston.group.instance

import scala.deriving.Mirror

import com.github.nhweston.group.typeclass.Group

type IsPair[AB <: Product, A, B] =
  Mirror.Product {
    type MirroredType = AB
    type MirroredMonoType = AB
    type MirroredElemTypes = (A, B)
  }

def mkSemidirectProduct[AB <: Product, A, B]
  (phi: (B, A) => A)
  (using
    groupA: Group[A],
    groupB: Group[B],
    isPair: IsPair[AB, A, B])
: Group[AB] =
  new Group[AB]:

    def fromTuple(tuple: (A, B)): AB =
      isPair.fromProduct(tuple)

    def toTuple(ab: AB): (A, B) =
      Tuple.fromProductTyped(ab)

    def values =
      ???

    override def plus(x: AB, y: AB) =
      val (xa, xb) = toTuple(x)
      val (ya, yb) = toTuple(y)
      val a = groupA.plus(xa, phi(xb, ya))
      val b = groupB.plus(xb, yb)
      fromTuple((a, b))
      
    override def negate(x: AB) =
      ???

    override lazy val zero =
      fromTuple((groupA.zero, groupB.zero))
