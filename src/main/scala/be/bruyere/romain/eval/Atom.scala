package be.bruyere.romain.eval

import be.bruyere.romain.qre.AtomQRE

case class Atom[In, Out, Fn] private(predicate: In => Boolean, qre: AtomQRE[In, Out], current: Option[In => Fn], output: Option[Fn])
  extends Eval[In, Out, Fn] {

  def start(fn: (() => Out) => Fn): Atom[In, Out, Fn] = {
    val newFn = qre.createNewF(fn)
    Atom( predicate, qre, Some(newFn), None)
  }

  def next(item: In): Atom[In, Out, Fn] = {
    if (predicate(item) && current.isDefined) {
      Atom(predicate, qre, None, current map (c => c(item)))
    } else {
      Atom( predicate, qre, None, None)
    }
  }
}
