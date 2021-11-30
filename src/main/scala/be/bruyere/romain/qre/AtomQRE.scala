package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Atom, Eval}

case class AtomQRE[In, Out] (predicate: In => Boolean, outputF: In => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Atom[In, Out, Fn](predicate, this, None, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): In => Fn = {
    (x: In) => fn(() => outputF(x))
  }
}
