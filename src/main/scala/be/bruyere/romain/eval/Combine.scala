package be.bruyere.romain.eval

case class Combine[In, Child1Out, Child2Out, Out, Fn] private(child1: Eval[In, Child1Out, (() => Child2Out) => Fn], child2: Eval[In, Child2Out, () => Child2Out], transformF: (Child1Out, Child2Out) => Out, output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    def newFn(x: () => Child1Out) = {
      (y: () => Child2Out) => fn(() => transformF.curried(x())(y()))
    }

    val newChild1 = child1.start(newFn)
    val newChild2 = child2.start(identity)
    Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
  }

  private def getOutput(newChild1: Eval[In, Child1Out, (() => Child2Out) => Fn], newChild2: Eval[In, Child2Out, () => Child2Out]) = {
    for {out1 <- newChild1.output; out2 <- newChild2.output}
      yield out1.apply(out2)
  }
}
