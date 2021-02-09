package be.bruyere.romain.eval

case class Split[In, ChildLOut, ChildROut, Agg, Out, Fn] private
(childL: Eval[In, ChildLOut, (() => ChildROut) => Fn], childR: Eval[In, ChildROut, Fn], transformF: (ChildLOut, ChildROut) => Agg, outputF: Agg => Out, output: Option[Fn])
  extends Eval[In, Out, Fn] {

  def start(fn: (() => Out) => Fn): Split[In, ChildLOut, ChildROut, Agg, Out, Fn] = {
    def newFn(x: () => ChildLOut) = (y: () => ChildROut) => fn(() => outputF(transformF.curried(x())(y())))

    val newChildL = childL.start(newFn)
    newChildL.output match {
      case Some(childOutput) =>
        def fn(x: () => ChildROut) = childOutput(x)

        val newChildR = childR.start(fn)

        newChildR.output match {
          case Some(_) => Split[In, ChildLOut, ChildROut, Agg, Out, Fn](newChildL, newChildR, transformF, outputF, newChildR.output)
          case None => Split(newChildL, newChildR, transformF, outputF, None)
        }
      case None => Split(newChildL, childR, transformF, outputF, None)
    }
  }

  def next(item: In): Split[In, ChildLOut, ChildROut, Agg, Out, Fn] = {
    val newChildL = childL.next(item)
    val newChildR = childR.next(item)

    val (newChildR2, output) = newChildL.output match {
      case Some(childOutput) => restartRight(childOutput, newChildR)
      case None => (newChildR, newChildR.output)
    }
    Split(newChildL, newChildR2, transformF, outputF, output)

  }

  private def restartRight(outputL: (() => ChildROut) => Fn, newChildR: Eval[In, ChildROut, Fn]) = {
    def fn(x: () => ChildROut) = outputL(x)

    val newChildR2 = newChildR.start(fn)

    newChildR2.output match {
      case Some(_) => (newChildR2, newChildR2.output)
      case None => (newChildR2, newChildR.output)
    }
  }
}
