package be.bruyere.romain.eval

case class Else[In, Out, Fn] private(child1: Eval[In, Out, Fn], child2: Eval[In, Out, Fn], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newChild1 = child1.start(fn)
    val newChild2 = child2.start(fn)
    Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
  }
}
