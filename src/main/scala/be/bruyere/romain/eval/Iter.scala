package be.bruyere.romain.eval

import be.bruyere.romain.qre.IterQRE

case class Iter[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, (() => Agg, (() => Agg) => Fn, Long)], qre: IterQRE[In, ChildOut, Agg, Out], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {

    val newChild = child.next(item)
    newChild.output match {
      case Some((trans, out, max)) =>
        max match {
          case 0 =>
            val t = trans()
            val newFn = qre.createNewF(t, out, qre.iterLimit)
            Iter(newChild.start(newFn), qre, Some(out(() => t)))
          case _ =>
            val newFn = qre.createNewF(trans, out, max-1)
            Iter(newChild.start(newFn), qre, Some(out(trans)))
        }
      case None => Iter(newChild, qre, None)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val out = qre.createOutF(fn)
    val newFn = qre.createNewF(qre.init, out, qre.iterLimit)
    Iter(child.start(newFn), qre, Some(fn(() => qre.outputF(qre.init))))
  }
}
