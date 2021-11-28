package be.bruyere.romain.eval

import be.bruyere.romain.qre.ApplyQRE

case class Apply[In, ChildOut, Out, Fn] private(child: Eval[In, ChildOut, Fn], qre: ApplyQRE[In, ChildOut, Out], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild = child.next(item)
    Apply(newChild, qre, newChild.output)
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newFn = qre.createNewF(fn)
    val newChild = child.start(newFn)
    Apply(newChild, qre, newChild.output)
  }
}
