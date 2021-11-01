package be.bruyere.romain.eval

case class Atom[In, Out, Fn] private(predicate: In => Boolean, outputF: () => In => Out, current: Option[In => Fn], output: Option[Fn])
  extends Eval[In, Out, Fn] {

  def start(fn: (() => Out) => Fn): Atom[In, Out, Fn] = {
    val oF = outputF()

    Atom( predicate, outputF, Some((x: In) => fn(() => oF(x))), None)
  }

  def next(item: In): Atom[In, Out, Fn] = {
    if (predicate(item) && current.isDefined) {
      Atom(predicate, outputF, None, current map (c => c(item)))
    } else {
      Atom( predicate, outputF, None, None)
    }
  }
}
