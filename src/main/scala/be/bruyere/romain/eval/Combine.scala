package be.bruyere.romain.eval

import be.bruyere.romain.qre.CombineQRE

case class Combine[In, Out, Child1Out, Child2Out, Fn] private
(
  child1: Eval[In, Child1Out, (() => Child2Out) => Fn],
  child2: Eval[In, Child2Out, () => Child2Out],
  qre: CombineQRE[In, Out, Child1Out, Child2Out],
  output: Option[Fn]
) extends Eval[In, Out, Fn] {

  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    Combine(newChild1, newChild2, qre, getOutput(newChild1, newChild2))
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newFn = qre.createNewF(fn)
    val newChild1 = child1.start(newFn)
    val newChild2 = child2.start(identity)
    Combine(newChild1, newChild2, qre, getOutput(newChild1, newChild2))
  }

  private def getOutput(newChild1: Eval[In, Child1Out, (() => Child2Out) => Fn], newChild2: Eval[In, Child2Out, () => Child2Out]) = {
    for {out1 <- newChild1.output; out2 <- newChild2.output}
      yield out1.apply(out2)
  }
}
