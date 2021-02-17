package be.bruyere.romain.eval

object EvalExtension {
  implicit class StartEval[In, Out](eval: Eval[In, Out, () => Out]) {
    def next(item: In): StartEval[In, Out] = eval.next(item)

    def result(): Option[Out] = eval.output map (o => o())

    def resultFn(): Option[() => Out] = eval.output
  }
}
