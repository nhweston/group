package com.github.nhweston.group.instance

import com.github.nhweston.group.data.{assertIso, generateHom, Aut}
import com.github.nhweston.group.typeclass.Group

given autGroup[A](using group: Group[A]): Group[Aut[A]] with

  lazy val values: Set[Aut[A]] =
    val pairings =
      group
        .gtors
        .iterator
        .map { gtor =>
          val order = group.abs(gtor)
          gtor ->
            group
              .values
              .iterator
              .filter(group.abs(_) == order)
              .toSeq
        }
        .toSeq
    def aux(pairings: Seq[(A, Seq[A])]): Seq[Seq[(A, A)]] =
      pairings match
        case (gtor, xs) +: tl =>
          for
            tl <- aux(tl)
            x <- xs
          yield
            (gtor -> x) +: tl
        case Seq() =>
          Seq()
    val result =
      for
        candidate <- aux(pairings)
        hom <- generateHom(candidate :_*)
        iso <- hom.checkIso
      yield iso
    result.toSet

  def plus(x: Aut[A], y: Aut[A]) =
    assertIso(x.andThen(y), x.reverse.andThen(y.reverse))

  override lazy val zero =
    assertIso(identity, identity)
