package be.bruyere.romain.eval

import be.bruyere.romain.qre.SplitQRE

case class Split[In, Out, ChildLOut, ChildROut, Agg, Fn] private
(childL: Eval[In, ChildLOut, (() => ChildROut) => Fn], childR: Eval[In, ChildROut, Fn], qre: SplitQRE[In, Out, ChildLOut, ChildROut, Agg], output: Option[Fn])
  extends Eval[In, Out, Fn] {

  def start(fn: (() => Out) => Fn): Split[In, Out, ChildLOut, ChildROut, Agg, Fn] = {
    val newFn = qre.createNewF(fn)

    val newChildL = childL.start(newFn)
    newChildL.output match {
      case Some(childOutput) =>
        val fn = qre.createOutputLeftF(childOutput)

        val newChildR = childR.start(fn)

        newChildR.output match {
          case Some(_) => Split[In, Out, ChildLOut, ChildROut, Agg, Fn](newChildL, newChildR, qre, newChildR.output)
          case None => Split(newChildL, newChildR, qre, None)
        }
      case None => Split(newChildL, childR, qre, None)
    }
  }

  def next(item: In): Split[In, Out, ChildLOut, ChildROut, Agg, Fn] = {
    val newChildL = childL.next(item)
    val newChildR = childR.next(item)

    val (newChildR2, output) = newChildL.output match {
      case Some(childOutput) => restartRight(childOutput, newChildR)
      case None => (newChildR, newChildR.output)
    }
    Split(newChildL, newChildR2, qre, output)

  }

  private def restartRight(outputL: (() => ChildROut) => Fn, newChildR: Eval[In, ChildROut, Fn]) = {
    val fn = qre.createOutputLeftF(outputL)

    val newChildR2 = newChildR.start(fn)

    newChildR2.output match {
      case Some(_) => (newChildR2, newChildR2.output)
      case None => (newChildR2, newChildR.output)
    }
  }
}
