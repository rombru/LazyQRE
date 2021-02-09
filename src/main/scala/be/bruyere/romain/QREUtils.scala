package be.bruyere.romain

import be.bruyere.romain.eval.EvalExtension.StartEval

import scala.annotation.tailrec

object QREUtils {

  @tailrec
  def executeOnList[D, C](list: List[D], sc: StartEval[D, C, () => C], fn: (StartEval[D, C, () => C]) => Unit): Unit = {
    list match {
      case Nil => ()
      case head :: tail =>
        val newSc = sc.next(head)
        fn(newSc);
        executeOnList(tail, newSc, fn)
    }
  }

}
