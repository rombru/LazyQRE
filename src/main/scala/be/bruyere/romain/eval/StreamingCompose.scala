package be.bruyere.romain.eval

case class StreamingCompose[In, Out, ChildOut, Fn] private(child1: Eval[In, ChildOut, () => ChildOut], child2: Eval[ChildOut, Out, Fn], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    strcompose(newChild1, child2)
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newChild1 = child1.start(identity)
    val newChild2 = child2.start(fn)
    strcompose(newChild1, newChild2)
  }

  private def strcompose(child1: Eval[In, ChildOut, () => ChildOut], child2: Eval[ChildOut, Out, Fn]) = {
    child1.output match {
      case Some(out) =>
        val newChild2 = child2.next(out())
        StreamingCompose(child1, newChild2, newChild2.output)
      case None =>
        StreamingCompose(child1, child2, None)
    }
  }
}
