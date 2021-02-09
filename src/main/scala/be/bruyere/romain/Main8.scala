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
    val atom3 = AtomQRE[Int, Int](x => x + 10, x => true)
    val combine = CombineQRE[Int,Int,Int,Int](atom2, atom3, (x,y) => x * y)
    val iter2 = IterQRE[Int, Int, Int, Int](combine, 0, (x,y) => x + y, x => x)
    val streamcomp = StreamingCompositionQRE[String, () => Int, Int, Int](iter1, iter2)

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

  trait QRE[Domain, Cost] {
    def start(): StartingCombinator[Domain, Cost, () => Cost] = {
      create[() => Cost]().start(identity)
    };

    protected[Main8] def create[Output](): Combinator[Domain, Cost, Output];
  }

  case class AtomQRE[Domain, OutFDomain] private(outputF: Domain => OutFDomain, predicate: Domain => Boolean) extends QRE[Domain, OutFDomain] {

    override def create[Output](): Combinator[Domain, OutFDomain, Output] = {
      Atom[Domain, OutFDomain, Output](outputF, predicate, None, None)
    }
  }

  case class ApplyQRE[Domain, TransOp, OutFDomain] private(child: QRE[Domain, TransOp], outputF: TransOp => OutFDomain) extends QRE[Domain, OutFDomain] {

    override def create[Output](): Combinator[Domain, OutFDomain, Output] = {
      Apply[Domain, TransOp, OutFDomain, Output](child.create(), outputF, None)
    }
  }

  case class CombineQRE[Domain, Result1, Result2, OutF] private(child1: QRE[Domain, Result1], child2: QRE[Domain, Result2], transformF: (Result1,Result2) => OutF) extends QRE[Domain, OutF] {

    override def create[Output](): Combinator[Domain, OutF, Output] = {
      Combine[Domain, Result1, Result2, OutF, Output](child1.create(), child2.create(), transformF, None)
    }
  }

  case class StreamingCompositionQRE[Domain1, Output1 <: () => Domain2, Domain2, Result2] private(child1: QRE[Domain1, Domain2], child2: QRE[Domain2, Result2]) extends QRE[Domain1, Result2] {

    override def create[Output2](): Combinator[Domain1, Result2, Output2] = {
      StreamingCompose[Domain1, Output1, Domain2, Result2, Output2](child1.create(), child2.create(), None)
    }
  }

  case class ElseQRE[Domain, Result] private(child1: QRE[Domain, Result], child2: QRE[Domain, Result]) extends QRE[Domain, Result] {

    override def create[Output](): Combinator[Domain, Result, Output] = {
      Else[Domain, Result, Output](child1.create(), child2.create(), None)
    }
  }

  case class IterQRE[Domain, TransOp, AggOp, OutF] private(child: QRE[Domain, TransOp], init: AggOp, transformF: (AggOp, TransOp) => AggOp, outputF: AggOp => OutF) extends QRE[Domain, OutF] {

    override def create[Output](): Combinator[Domain, OutF, Output] = {
      Iter[Domain, TransOp, AggOp, OutF, Output](child.create(), init, transformF, outputF, None)
    }
  }

  case class SplitQRE[Domain, TransOp1, TransOp2, OutF, Result] private(childL: QRE[Domain, TransOp1], childR: QRE[Domain, TransOp2], transformF: (TransOp1, TransOp2) => OutF, outputF: OutF => Result) extends QRE[Domain, Result] {

    override def create[Output](): Combinator[Domain, Result, Output] = {
      val left = childL.create[(() => TransOp2) => Output]()
      val right = childR.create[Output]()
      Split(left, right, transformF, outputF, None)
    }
  }

  implicit class StartingCombinator[Domain, FnDomain, Output <: () => FnDomain](combinator: Combinator[Domain, FnDomain, Output]) {
    def next(item: Domain): Combinator[Domain, FnDomain, Output] = combinator.next(item)

    def result(): Option[FnDomain] = combinator.output map (o => o())

    def resultFn(): Option[() => FnDomain] = combinator.output
  }

  sealed trait Combinator[Domain, FnDomain, Output] {
    val output: Option[Output]

    def next(item: Domain): Combinator[Domain, FnDomain, Output]

    def start(current: (() => FnDomain) => Output): Combinator[Domain, FnDomain, Output]
  }

  case class Split[Domain, TransOp1, TransOp2, OutF, Result, Output] private
  (childL: Combinator[Domain, TransOp1, (() => TransOp2) => Output], childR: Combinator[Domain, TransOp2, Output], transformF: (TransOp1, TransOp2) => OutF, outputF: OutF => Result, output: Option[Output])
    extends Combinator[Domain, Result, Output] {

    def start(fn: (() => Result) => Output): Split[Domain, TransOp1, TransOp2, OutF, Result, Output] = {
      def newFn(x: () => TransOp1) = (y: () => TransOp2) => fn(() => outputF(transformF.curried(x())(y())))

      val newChildL = childL.start(newFn)
      newChildL.output match {
        case Some(childOutput) =>
          def fn(x: () => TransOp2) = childOutput(x)

          val newChildR = childR.start(fn)

          newChildR.output match {
            case Some(_) => Split[Domain, TransOp1, TransOp2, OutF, Result, Output](newChildL, newChildR, transformF, outputF, newChildR.output)
            case None => Split(newChildL, newChildR, transformF, outputF, None)
          }
        case None => Split(newChildL, childR, transformF, outputF, None)
      }
    }

    def next(item: Domain): Split[Domain, TransOp1, TransOp2, OutF, Result, Output] = {
      val newChildL = childL.next(item)
      val newChildR = childR.next(item)

      val (newChildR2, output) = newChildL.output match {
        case Some(childOutput) => restartRight(childOutput, newChildR)
        case None => (newChildR, newChildR.output)
      }
      Split(newChildL, newChildR2, transformF, outputF, output)

    }

    private def restartRight(outputL: (() => TransOp2) => Output, newChildR: Combinator[Domain, TransOp2, Output]) = {
      def fn(x: () => TransOp2) = outputL(x)

      val newChildR2 = newChildR.start(fn)

      newChildR2.output match {
        case Some(_) => (newChildR2, newChildR2.output)
        case None => (newChildR2, newChildR.output)
      }
    }
  }

  case class Atom[Domain, OutFDomain, Output] private(outputF: Domain => OutFDomain, predicate: Domain => Boolean, current: Option[Domain => Output], output: Option[Output])
    extends Combinator[Domain, OutFDomain, Output] {

    def start(fn: (() => OutFDomain) => Output): Atom[Domain, OutFDomain, Output] = {
      Atom(outputF, predicate, Some((x: Domain) => fn(() => outputF(x))), None)
    }

    def next(item: Domain): Atom[Domain, OutFDomain, Output] = {
      if (predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, current map (c => c(item)))
      } else {
        Atom(outputF, predicate, None, None)
      }
    }
  }

  case class Iter[Domain, TransOp, AggOp, OutF, Output] private(child: Combinator[Domain, TransOp, (() => AggOp, (() => AggOp) => Output)], init: AggOp, transformF: (AggOp, TransOp) => AggOp, outputF: AggOp => OutF, output: Option[Output]) extends Combinator[Domain, OutF, Output] {
    override def next(item: Domain): Combinator[Domain, OutF, Output] = {
      val newChild = child.next(item)
      newChild.output match {
        case Some((trans, out)) =>
          def newFn(x: () => TransOp) = {
            def newTrans = () => transformF.curried(trans())(x())

            (newTrans, out)
          }

          Iter(newChild.start(newFn), init, transformF, outputF, Some(out(trans)))
        case None => Iter(newChild, init, transformF, outputF, None)
      }
    }

    override def start(fn: (() => OutF) => Output): Combinator[Domain, OutF, Output] = {
      def newFn(x: () => TransOp) = {
        def trans = () => transformF.curried(init)(x())

        def out = (y: () => AggOp) => fn(() => outputF(y()))

        (trans, out)
      }

      Iter(child.start(newFn), init, transformF, outputF, Some(fn(() => outputF(init))))
    }
  }

  case class Apply[Domain, TransOp, OutF, Output] private(child: Combinator[Domain, TransOp, Output], outputF: TransOp => OutF, output: Option[Output]) extends Combinator[Domain, OutF, Output] {
    override def next(item: Domain): Combinator[Domain, OutF, Output] = {
      val newChild = child.next(item)
      Apply(newChild, outputF, newChild.output)
    }

    override def start(fn: (() => OutF) => Output): Combinator[Domain, OutF, Output] = {
      def newFn(x: () => TransOp) = fn(() => outputF(x()))

      val newChild = child.start(newFn)
      Apply(newChild, outputF, newChild.output)
    }
  }

  case class Else[Domain, Result, Output] private(child1: Combinator[Domain, Result, Output], child2: Combinator[Domain, Result, Output], output: Option[Output]) extends Combinator[Domain, Result, Output] {
    override def next(item: Domain): Combinator[Domain, Result, Output] = {
      val newChild1 = child1.next(item)
      val newChild2 = child2.next(item)
      Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
    }

    override def start(fn: (() => Result) => Output): Combinator[Domain, Result, Output] = {
      val newChild1 = child1.start(fn)
      val newChild2 = child2.start(fn)
      Else(newChild1, newChild2, newChild1.output orElse newChild2.output)
    }
  }

  case class Combine[Domain, Result1, Result2, OutF, Output] private(child1: Combinator[Domain, Result1, (() => Result2) => Output], child2: Combinator[Domain, Result2, () => Result2], transformF: (Result1, Result2) => OutF, output: Option[Output]) extends Combinator[Domain, OutF, Output] {
    override def next(item: Domain): Combinator[Domain, OutF, Output] = {
      val newChild1 = child1.next(item)
      val newChild2 = child2.next(item)
      Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
    }

    override def start(fn: (() => OutF) => Output): Combinator[Domain, OutF, Output] = {
      def newFn(x: () => Result1) = {
        (y: () => Result2) => fn(() => transformF.curried(x())(y()))
      }

      val newChild1 = child1.start(newFn)
      val newChild2 = child2.start(identity)
      Combine(newChild1, newChild2, transformF, getOutput(newChild1, newChild2))
    }

    private def getOutput(newChild1: Combinator[Domain, Result1, (() => Result2) => Output], newChild2: Combinator[Domain, Result2, () => Result2]) = {
      for {out1 <- newChild1.output; out2 <- newChild2.output}
        yield out1.apply(out2)
    }
  }

  case class StreamingCompose[Domain1, Output1 <: () => Domain2, Domain2, Result2, Output2] private(child1: Combinator[Domain1, Domain2, () => Domain2], child2: Combinator[Domain2, Result2, Output2], output: Option[Output2]) extends Combinator[Domain1, Result2, Output2] {
    override def next(item: Domain1): Combinator[Domain1, Result2, Output2] = {
      val newChild1 = child1.next(item)
      strcompose(newChild1, child2)
    }

    override def start(fn: (() => Result2) => Output2): Combinator[Domain1, Result2, Output2] = {
      val newChild1 = child1.start(identity)
      val newChild2 = child2.start(fn)
      strcompose(newChild1, newChild2)
    }

    private def strcompose(child1: Combinator[Domain1, Domain2, () => Domain2], child2: Combinator[Domain2, Result2, Output2]) = {
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