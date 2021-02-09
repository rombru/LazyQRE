package be.bruyere.romain.eval

case class Atom[In, Out, Fn] private(outputF: In => Out, predicate: In => Boolean, current: Option[In => Fn], output: Option[Fn])
  extends Eval[In, Out, Fn] {

  def start(fn: (() => Out) => Fn): Atom[In, Out, Fn] = {
    Atom(outputF, predicate, Some((x: In) => fn(() => outputF(x))), None)
  }

  def next(item: In): Atom[In, Out, Fn] = {
    if (predicate(item) && current.isDefined) {
      Atom(outputF, predicate, None, current map (c => c(item)))
    } else {
      Atom(outputF, predicate, None, None)
    }
  }
}
