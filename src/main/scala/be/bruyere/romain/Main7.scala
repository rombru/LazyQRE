//package be.bruyere.romain
//
//object Main7 {
//
//  sealed trait Combinator[D,C,T] {
//    val maybeOutput: Option[() => T];
//    def next(item: D): Combinator[D,C,T];
//    def start(current: C => T): Combinator[D,C,T];
//  }
//
//  case class Atom[D,C,T] private(outputF: D => C, predicate: D => Boolean, current: Option[D => T], maybeOutput: Option[() => T]) extends Combinator[D,C,T] {
//
//    def this(outputF: D => C, predicate: D => Boolean) = this(outputF, predicate, None, None);
//
//    def next(item: D): Atom[D,C,T] = {
//      if(predicate(item) && current.isDefined) {
//        Atom[D,C,T](outputF, predicate, None, Some(() => current.get(item)));
//      } else {
//        Atom[D,C,T](outputF, predicate, None, None);
//      }
//    }
//
//    def start(current: C => T): Atom[D,C,T] = {
//      //andThen : x -> current(output(x))
//      Atom[D,C,T](outputF, predicate, Some(outputF andThen current), None);
//    }
//
//    def start(): Atom[D,C,C] = {
//      Atom[D,C,C](outputF,predicate,Some(outputF andThen identity),None)
//    }
//  }
//
//  case class Iter[D,A,B,C,T] private(child: Combinator[D,A,B], init: B, transformF: (B,A) => B, outputF: B => C, maybeOutput: Option[() => T]) extends Combinator[D,C,T] {
//
//    def this(child: Combinator[D,A,B], init: B, transformF: (B,A) => B, outputF: B => C) = this(child, init, transformF, outputF, None)
//
//    def next(item: D): Iter[D,A,B,C,T] = {
//      val combinator = child.next(item);
//
//      combinator.maybeOutput match {
//        case Some(output) => {
//          def h(x:A) : B = transformF.curried(output())(x);
//          Iter[D,A,B,C,T](child.start(h), init, transformF, outputF, combinator.maybeOutput);
//        }
//        case None => Iter(combinator, init, transformF, outputF, None);
//      }
//    }
//
//    def start(current: C => T): Iter[D,A,B,C,T] = {
//      def h(x:A) : B = transformF.curried(init)(current(x));
//      Iter[D,A,B,C,T](child.start(h), init, transformF, outputF, Some(() => init));
//    }
//  }
//
//  case class Split[D] private(childL: Combinator[D], childR: Combinator[D], transformF: (D,D) => D, maybeOutput: Option[() => D]) extends Combinator[D] {
//
//    def this(childL: Combinator[D], childR: Combinator[D], transformF: (D,D) => D) = this(childL, childR, transformF, None)
//
//    def next(item: D): Split[D] = {
//      val combinatorL = childL.next(item);
//      val combinatorR = childR.next(item);
//
//      val (newCombinatorR, output) = combinatorL.maybeOutput match {
//        case Some(outputL) => restartRight(outputL, combinatorR)
//        case None => (combinatorR, combinatorR.maybeOutput)
//      }
//      Split(combinatorL, newCombinatorR, transformF, output)
//    }
//
//    private def restartRight(outputL: () => D, cc: Combinator[D]) = {
//      def h(x: D): D = transformF.curried(outputL())(x);
//      val combinatorR = childR.start(h);
//      combinatorR.maybeOutput match {
//        case Some(_) => (combinatorR, combinatorR.maybeOutput)
//        case None => (combinatorR, cc.maybeOutput)
//      }
//    }
//
//    def start(current: D => D): Split[D] = {
//      val combinatorL = childL.start(current);
//
//      combinatorL.maybeOutput match {
//        case Some(outputL) => {
//          def h(x:D) : D = transformF.curried(outputL())(x);
//          val combinatorR = childR.start(h);
//
//          combinatorR.maybeOutput match {
//            case Some(_) => Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
//            case None => Split(combinatorL, combinatorR, transformF, None)
//          }
//        }
//        case None => Split(combinatorL, childR, transformF, None)
//      }
//    }
//
//    def start(): Split[D] = {
//      start(identity);
//    }
//  }
//
//
//  def main(args: Array[String]): Unit = {
//
//    val atomPos = new Atom[Int](x => x, x => x != 0);
//    val iterPos = new Iter[Int](atomPos, 0, (x, y) => x + y, x => x);
//    val atomNeg = new Atom[Int](x => x, x => x < 0);
//    val split1 = new Split[Int](iterPos, atomNeg, (x,y) => x * y);
//
//    val atomPos1 = new Atom[Int](x => x, x => x != 0);
//    val atomPos2 = new Atom[Int](x => x, x => x != 0);
//    val split2 = new Split[Int](atomPos1, atomPos2, (x,y) => x + y);
//
//    val atomNeg1 = new Atom[Int](x => x, x => x < 0);
//    val split3 = new Split[Int](split2, atomNeg1, (x,y) => x - y);
//
//    val split = new Split[Int](split1, split3, (x,y) => x * y);
//
//    var eval = split.start();
//    List(2, 2, -1, 4, 5, -1, 4, 6, -7, 9, 9, 9, -3, 2, 2, -8).foreach(item => {
//      eval = eval.next(item);
//      eval.maybeOutput match {
//        case Some(output) => println(output());
//        case None => println("Undefined")
//      }
//    })
//  }
//}