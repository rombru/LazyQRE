package be.bruyere.romain

import scala.annotation.tailrec

object Main8 {


  def main(args: Array[String]): Unit = {

    //    val atom1 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
    //    val atom2 = AtomQRE[String, Int](x => x.length, x => x.length > 20)
    //    val atom2b = AtomQRE[String, Int](x => 20, x => x.length <= 20)
    //    val else1 = ElseQRE[String, Int](atom2, atom2b)
    //    val iter = IterQRE[String, Int, Int, Int](else1, 20, (x, y) => x + y, x => x)
    //    val split = SplitQRE[String, Int, Int, Int, String](atom1, iter, (x, y) => x + y, x => x.toString)
    //    val atom3 = AtomQRE[String, Int](x => x.length, x => x.isEmpty)
    //    val split2 = SplitQRE[String, String, Int, String, String](split, atom3, (x, y) => x * y, x => x)
    //    val iter2 = IterQRE[String, String, String, String](split2, "10", (x, y) => x.concat(y) , x => x)
    //    val apply = ApplyQRE[String, String, String](iter2, x => x.concat("apply"))

    val atom1 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
    val iter1 = IterQRE[String, Int, Int, Int](atom1, 0, (x,y) => x + y, x => x)
    val atom2 = AtomQRE[Int, Int](x => x + 5, x => true)
    val atom3 = AtomQRE[Int, Int](x => x + 10, _ => true)
    val combine = CombineQRE[Int,Int,Int,Int](atom2, atom3, (x,y) => x * y)
    val iter2 = IterQRE[Int, Int, Int, Int](combine, 0, (x,y) => x + y, x => x)
    val streamcomp = StreamingCompositionQRE[String, Int, Int](iter1, iter2)

    val eval = streamcomp.start()
    val list = List("aaaaaa", "aaaaa");
    executeOnList[String, Int](list, eval, (sc: StartingCombinator[String, Int, () => Int]) => {
      println("Result = " + sc.result())
      println("ResultFn = " + sc.resultFn())
    })
  }

  @tailrec
  def executeOnList[D, C](list: List[D], sc: StartingCombinator[D, C, () => C], fn: (StartingCombinator[D, C, () => C]) => Unit): Unit = {
    list match {
      case Nil => ()
      case head :: tail =>
        val newSc = sc.next(head)
        fn(newSc);
        executeOnList(tail, newSc, fn)
    }
  }

  trait QRE[In, Out] {
    def start(): StartingCombinator[In, Out, () => Out] = {
      create[() => Out]().start(identity)
    };

    protected[Main8] def create[Fn](): Combinator[In, Out, Fn];
  }

  case class AtomQRE[In, Out] private(outputF: In => Out, predicate: In => Boolean) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Atom[In, Out, Fn](outputF, predicate, None, None)
    }
  }

  case class ApplyQRE[In, ChildOut, Out] private(child: QRE[In, ChildOut], outputF: ChildOut => Out) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Apply[In, ChildOut, Out, Fn](child.create(), outputF, None)
    }
  }

  case class CombineQRE[In, Child1Out, Child2Out, Out] private(child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], transformF: (Child1Out,Child2Out) => Out) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Combine[In, Child1Out, Child2Out, Out, Fn](child1.create(), child2.create(), transformF, None)
    }
  }

  case class StreamingCompositionQRE[In, Child1Out, Out] private(child1: QRE[In, Child1Out], child2: QRE[Child1Out, Out]) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      StreamingCompose[In, Child1Out, Out, Fn](child1.create(), child2.create(), None)
    }
  }

  case class ElseQRE[In, Out] private(child1: QRE[In, Out], child2: QRE[In, Out]) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Else[In, Out, Fn](child1.create(), child2.create(), None)
    }
  }

  case class IterQRE[In, ChildOut, Agg, Out] private(child: QRE[In, ChildOut], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Iter[In, ChildOut, Agg, Out, Fn](child.create(), init, transformF, outputF, None)
    }
  }

  case class SplitQRE[In, ChildLOut, ChildROut, Agg, Out] private(childL: QRE[In, ChildLOut], childR: QRE[In, ChildROut], transformF: (ChildLOut, ChildROut) => Agg, outputF: Agg => Out) extends QRE[In, Out] {

    override def create[Fn](): Combinator[In, Out, Fn] = {
      Split[In, ChildLOut, ChildROut, Agg, Out, Fn](childL.create(), childR.create(), transformF, outputF, None)
    }
  }

  implicit class StartingCombinator[In, Out, Fn <: () => Out](combinator: Combinator[In, Out, Fn]) {
    def next(item: In): Combinator[In, Out, Fn] = combinator.next(item)

    def result(): Option[Out] = combinator.output map (o => o())

    def resultFn(): Option[() => Out] = combinator.output
  }

  sealed trait Combinator[In, Out, Fn] {
    val output: Option[Fn]

    def next(item: In): Combinator[In, Out, Fn]

    def start(current: (() => Out) => Fn): Combinator[In, Out, Fn]
  }

  case class Split[In, ChildLOut, ChildROut, Agg, Out, Fn] private
  (childL: Combinator[In, ChildLOut, (() => ChildROut) => Fn], childR: Combinator[In, ChildROut, Fn], transformF: (ChildLOut, ChildROut) => Agg, outputF: Agg => Out, output: Option[Fn])
    extends Combinator[In, Out, Fn] {

    def start(fn: (() => Out) => Fn): Split[In, ChildLOut, ChildROut, Agg, Out, Fn] = {
      def newFn(x: () => ChildLOut) = (y: () => ChildROut) => fn(() => outputF(transformF.curried(x())(y())))

      val newChildL = childL.start(newFn)
      newChildL.output match {
        case Some(childOutput) =>
          def fn(x: () => ChildROut) = childOutput(x)

          val newChildR = childR.start(fn)

          newChildR.output match {
            case Some(_) => Split[In, ChildLOut, ChildROut, Agg, Out, Fn](newChildL, newChildR, transformF, outputF, newChildR.output)
            case None => Split(newChildL, newChildR, transformF, outputF, None)
          }
        case None => Split(newChildL, childR, transformF, outputF, None)
      }
    }

    def next(item: In): Split[In, ChildLOut, ChildROut, Agg, Out, Fn] = {
      val newChildL = childL.next(item)
      val newChildR = childR.next(item)

      val (newChildR2, output) = newChildL.output match {
        case Some(childOutput) => restartRight(childOutput, newChildR)
        case None => (newChildR, newChildR.output)
      }
      Split(newChildL, newChildR2, transformF, outputF, output)

    }

    private def restartRight(outputL: (() => ChildROut) => Fn, newChildR: Combinator[In, ChildROut, Fn]) = {
      def fn(x: () => ChildROut) = outputL(x)

      val newChildR2 = newChildR.start(fn)

      newChildR2.output match {
        case Some(_) => (newChildR2, newChildR2.output)
        case None => (newChildR2, newChildR.output)
      }
    }
  }

  case class Atom[In, Out, Fn] private(outputF: In => Out, predicate: In => Boolean, current: Option[In => Fn], output: Option[Fn])
    extends Combinator[In, Out, Fn] {

    def start(fn: (() => Out) => Fn): Atom[In, Out, Fn] = {
      Atom(outputF, predicate, Some((x: In) => fn(() => outputF(x))), None)
    }

    def next(item: In): Atom[In, Out, Fn] = {
      if (predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, current map (c => c(item)))
      } else {
        Atom(outputF, predicate, None, None)
      }
    }
  }

  case class Iter[In, ChildOut, Agg, Out, Fn] private(child: Combinator[In, ChildOut, (() => Agg, (() => Agg) => Fn)], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out, output: Option[Fn]) extends Combinator[In, Out, Fn] {
    override def next(item: In): Combinator[In, Out, Fn] = {
      val newChild = child.next(item)
      newChild.output match {
        case Some((trans, out)) =>
          def newFn(x: () => ChildOut) = {
            def newTrans = () => transformF.curried(trans())(x())

            (newTrans, out)
          }

          Iter(newChild.start(newFn), init, transformF, outputF, Some(out(trans)))
        case None => Iter(newChild, init, transformF, outputF, None)
      }
    }

    override def start(fn: (() => Out) => Fn): Combinator[In, Out, Fn] = {
      def newFn(x: () => ChildOut) = {
        def trans = () => transformF.curried(init)(x())

        def out = (y: () => Agg) => fn(() => outputF(y()))

        (trans, out)
      }

      Iter(child.start(newFn), init, transformF, outputF, Some(fn(() => outputF(init))))
    }
  }

  case class Apply[In, ChildOut, Out, Fn] private(child: Combinator[In, ChildOut, Fn], outputF: ChildOut => Out, output: Option[Fn]) extends Combinator[In, Out, Fn] {
    override def next(item: In): Combinator[In, Out, Fn] = {
      val newChild = child.next(item)
      Apply(newChild, outputF, newChild.output)
    }

    override def start(fn: (() => Out) => Fn): Combinator[In, Out, Fn] = {
      def newFn(x: () => ChildOut) = fn(() => outputF(x()))

      val newChild = child.start(newFn)
      Apply(newChild, outputF, newChild.output)
    }
  }

  case class Else[In, Out, Fn] private(child1: Combinator[In, Out, Fn], child2: Combinator[In, Out, Fn], output: Option[Fn]) extends Combinator[In, Out, Fn] {
    override def next(item: In): Combinator[In, Out, Fn] = {
      val newChild1 = child1.next(item)
      val newChild2 = child2.next(item)
      Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
    }

    override def start(fn: (() => Out) => Fn): Combinator[In, Out, Fn] = {
      val newChild1 = child1.start(fn)
      val newChild2 = child2.start(fn)
      Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
    }
  }

  case class Combine[In, Child1Out, Child2Out, Out, Fn] private(child1: Combinator[In, Child1Out, (() => Child2Out) => Fn], child2: Combinator[In, Child2Out, () => Child2Out], transformF: (Child1Out, Child2Out) => Out, output: Option[Fn]) extends Combinator[In, Out, Fn] {
    override def next(item: In): Combinator[In, Out, Fn] = {
      val newChild1 = child1.next(item)
      val newChild2 = child2.next(item)
      Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
    }

    override def start(fn: (() => Out) => Fn): Combinator[In, Out, Fn] = {
      def newFn(x: () => Child1Out) = {
        (y: () => Child2Out) => fn(() => transformF.curried(x())(y()))
      }

      val newChild1 = child1.start(newFn)
      val newChild2 = child2.start(identity)
      Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
    }

    private def getOutput(newChild1: Combinator[In, Child1Out, (() => Child2Out) => Fn], newChild2: Combinator[In, Child2Out, () => Child2Out]) = {
      for {out1 <- newChild1.output; out2 <- newChild2.output}
        yield out1.apply(out2)
    }
  }

  case class StreamingCompose[In, Child1Out, Out, Fn] private(child1: Combinator[In, Child1Out, () => Child1Out], child2: Combinator[Child1Out, Out, Fn], output: Option[Fn]) extends Combinator[In, Out, Fn] {
    override def next(item: In): Combinator[In, Out, Fn] = {
      val newChild1 = child1.next(item)
      strcompose(newChild1, child2)
    }

    override def start(fn: (() => Out) => Fn): Combinator[In, Out, Fn] = {
      val newChild1 = child1.start(identity)
      val newChild2 = child2.start(fn)
      strcompose(newChild1, newChild2)
    }

    private def strcompose(child1: Combinator[In, Child1Out, () => Child1Out], child2: Combinator[Child1Out, Out, Fn]) = {
      child1.output match {
        case Some(out) =>
          val newChild2 = child2.next(out())
          StreamingCompose(child1, newChild2, newChild2.output)
        case None =>
          StreamingCompose(child1, child2, None)
      }
    }
  }

}