package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Atom, Eval}

case class AtomQRE[In, Out] private(outputF: In => Out, predicate: In => Boolean) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Atom[In, Out, Fn](outputF, predicate, None, None)
  }
}
