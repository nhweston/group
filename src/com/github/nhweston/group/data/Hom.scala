package com.github.nhweston.group.data

import scala.annotation.tailrec

import com.github.nhweston.group.typeclass.Group

/** Represents a homomorphism between `A` and `B`. */
class Hom[A, B] private[data]
  (phi: A => B)
  (using
    val groupA: Group[A],
    val groupB: Group[B])
extends (A => B):

  override def apply(x: A): B =
    phi(x)

  /** Checks whether this is an isomorphism. */
  def checkIso: Option[Iso[A, B]] =
    val phiInv =
      groupA
        .values
        .iterator
        .map(x => phi(x) -> x)
        .toMap
    if phiInv.size != groupB.values.size then None
    else Some(new Iso(phi, phiInv))

  override def equals(other: Any): Boolean =
    other match
      case other: Hom[?, ?] if this.groupA.zero == other.groupA.zero =>
        groupA.gtors.forall(x => this(x) == other.asInstanceOf[Hom[A, ?]](x))
      case _ =>
        false
  
  override lazy val hashCode: Int =
    groupA.gtors.map(x => x -> this(x)).hashCode

/** Represents an isomorphism between `A` and `B`. */
class Iso[A, B] private[group]
  (phi: A => B, phiInv: B => A)
  (using
    groupA: Group[A],
    groupB: Group[B])
extends Hom[A, B](phi):

  def reverse: Iso[B, A] = new Iso(phiInv, phi)

/** Represents an automorphism of `A`. */
type Aut[A] = Iso[A, A]

/** Asserts that `phi` is a homomorphism. This operation is unsafe. */
def assertHom[A, B]
  (phi: A => B)
  (using
    groupA: Group[A],
    groupB: Group[B])
: Hom[A, B] =
  new Hom(phi)

/**
 * Asserts that `phi` is a homomorphism with `phiInv` its inverse. This
 * operation is unsafe.
 */
def assertIso[A, B]
  (phi: A => B, phiInv: B => A)
  (using groupA: Group[A], groupB: Group[B])
: Iso[A, B] =
  new Iso(phi, phiInv)

/** Checks whether `phi` is a homomorphism. */
def checkHom[A, B]
  (phi: A => B)
  (using
    groupA: Group[A],
    groupB: Group[B])
: Option[Hom[A, B]] =
  def test(x: A, y: A): Boolean =
    val inner = groupB.plus(phi(x), phi(y))
    val outer = phi(groupA.plus(x, y))
    inner == outer
  @tailrec
  def aux(xs: Seq[A]): Option[Hom[A, B]] =
    xs match
      case x +: tl =>
        if groupA.values.forall(y => test(x, y)) then aux(tl)
        else None
      case Seq() =>
        Some(assertHom(phi))
  aux(groupA.values.toSeq)

/** Generates a homomorphism from a collection of mappings. */
def generateHom[A, B]
  (gtors: (A, B)*)
  (using
    groupA: Group[A],
    groupB: Group[B])
: Option[Hom[A, B]] =
  def auxQueue(
    queue: Seq[(A, B)],
    phi: Map[A, B],
  ): Option[Hom[A, B]] =
    queue match
      case (qa, qb) +: qtl =>
        @tailrec
        def auxGtors(
          gtors: Seq[(A, B)],
          phi: Map[A, B],
          queueNext: Seq[(A, B)],
        ): Option[(Map[A, B], Seq[(A, B)])] =
          gtors match
            case (ga, gb) +: gtl =>
              val a = groupA.plus(qa, ga)
              val b = groupB.plus(qb, gb)
              phi.get(a) match
                case Some(b0) =>
                  if b == b0 then auxGtors(gtl, phi, queueNext)
                  else None
                case _ =>
                  auxGtors(gtl, phi + (a -> b), queueNext :+ (a -> b))
            case Seq() =>
              Some((phi, queueNext))
        auxGtors(gtors, phi, qtl) match
          case Some((phi, queueNext)) => auxQueue(queueNext, phi)
          case None => None
      case Seq() =>
        Some(assertHom(phi))
  val i = groupA.zero -> groupB.zero
  auxQueue(Seq(i), Map(i))
