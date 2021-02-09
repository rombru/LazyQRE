package be.bruyere.romain.eval

trait Eval[In, Out, Fn] {
  val output: Option[Fn]

  def next(item: In): Eval[In, Out, Fn]

  def start(current: (() => Out) => Fn): Eval[In, Out, Fn]
}
