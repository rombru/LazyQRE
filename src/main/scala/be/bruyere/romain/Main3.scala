package be.bruyere.romain

object Main3 {

  sealed trait Combinator[Domain,T,Cost,N] {
    val maybeOutput: Option[() => Cost];
    def next(item: Domain): Combinator[Domain,T,Cost,N];
    def start(current: N => T): Combinator[Domain,T,Cost,N];
    //def start(): Combinator[Domain,N,N,N];
    def map[E,F](f: Combinator[Domain,T,Cost,N] => Combinator[E,F,Cost,N]): Combinator[E,F,Cost,N] = f(this)
  }

  case class Atom[D,M,N] private (outputF: D => N, predicate: D => Boolean, current: Option[D => M], maybeOutput: Option[() => M]) extends Combinator[D,M,M,N] {

    def this(outputF: D => N, predicate: D => Boolean) = this(outputF, predicate, None, None);

    def next(item: D): Atom[D,M,N] = {
      if(predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, Some(() => current.get(item)));
      } else {
        Atom(outputF, predicate, None, None);
      }
    }

    def start(current: N => M): Atom[D,M,N] = {
      //andThen : x -> current(output(x))
      Atom(outputF, predicate, Some(outputF andThen current), None);
    }

    def start(): Atom[D,N,N] = {
      Atom[D,N,N](outputF,predicate,Some(outputF),None)
    }
  }

  case class Iter[D,A,B,M] private (child: Combinator[D,B,B,A], init: B, transformF: (B,A) => B, outputF: B => M, maybeOutput: Option[() => M]) extends Combinator[D,A,M,A] {

    def this(child: Combinator[D,B,B,A], init: B, transformF: (B,A) => B, outputF: B => M) = this(child, init, transformF, outputF, None)

    def next(item: D): Iter[D,A,B,M] = {
      val combinator = child.next(item);

      combinator.maybeOutput match {
        case Some(output) => {
          def h(x:A) : B = transformF.curried(output())(x);
          Iter[D,A,B,M](child.start(h), init, transformF, outputF, Some(() => outputF(output())));
        }
        case None => Iter[D,A,B,M](combinator, init, transformF, outputF, None);
      }
    }

    def start(current: A => A): Iter[D,A,B,M] = {
      def h(x:A) : B = transformF.curried(init)(current(x));
      Iter(child.start(h), init, transformF, outputF, Some(() => outputF(init)));
    }

    def start(): Iter[D,A,B,M] = {
      def h(x:A) : B = transformF.curried(init)(x);
      Iter[D,A,B,M](child.start(h), init, transformF, outputF,Some(() => outputF(init)))
    }
  }
//
//  case class Split[D,A,B,C] private (childL: Combinator[D,Any,A,Any], childR: Combinator[D,C,B,B], transformF: (A,B) => C, maybeOutput: Option[() => C]) extends Combinator[D,Any,C,Any] {
//
//    def this(childL: Combinator[D,Any,A,Any], childR: Combinator[D,C,B,B], transformF: (A,B) => C) = this(childL, childR, transformF, None)
//
//    def next(item: D): Split[D,A,B,C] = {
//      val combinatorL = childL.next(item);
//
//      combinatorL.maybeOutput match {
//        case Some(outputL) => {
//          def h(x:B) : C = transformF.curried(outputL())(x);
//          val combinatorR = childR.start(h);
//          Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
//        }
//        case None =>  {
//          val combinatorR = childR.next(item);
//          Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
//        }
//      }
//    }
//
//    def start(current: Any => Any): Split[D,A,B,C] = {
//      val combinatorL = childL.start(current);
//
//      combinatorL.maybeOutput match {
//        case Some(outputL) => {
//          def h(x:B) : C = transformF.curried(outputL())(x);
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
//    def start(): Split[D,A,B,C] = {
//      start(identity);
//    }
//  }

  def main(args: Array[String]): Unit = {

    val atomPos = new Atom[Int,String,Int](x => {
      println("AtomPos");
      x
    }, x => x > 0);
    val iterPos = new Iter[Int,Int,String,String](atomPos, "0", (x, y) => {
      println("Iter");
      x.concat(" + " + y.toString)
    }, x => x);
//    val atomNeg = new Atom[Int,Int,Int](x => {
//      println("AtomNeg")
//      x
//    }, x => x < 0);
//    val split = new Split[Int,Int,Int,Int](atomNeg, iterPos, (x,y) => {
//      println("Split")
//      x * y
//    });

    var eval = iterPos.start();
    List(1, 1, 1, 1, 1).foreach(item => {
      eval = eval.next(item);
      eval.maybeOutput match {
        case Some(output) => println(output());
        case None => println("Undefined")
      }
    })
  }

//  def test1(): Unit = {
//    val atomPos = new Atom[Int](x => x, x => x > 0);
//    val iterPos = new Iter[Int](atomPos, 0, (x, y) => x + y, x => x);
//    val atomNeg = new Atom[Int](x => x, x => x < 0);
//    val split1 = new Split[Int](iterPos, atomNeg, (x,y) => x * y);
//
//    val atomPos1 = new Atom[Int](x => x, x => x > 0);
//    val atomPos2 = new Atom[Int](x => x, x => x > 0);
//    val split2 = new Split[Int](atomPos1, atomPos2, (x,y) => x + y);
//
//    val atomNeg1 = new Atom[Int](x => x, x => x < 0);
//    val split3 = new Split[Int](split2, atomNeg1, (x,y) => x - y);
//
//    val split = new Split[Int](split1, split3, (x,y) => x * y);
//
//    var eval = split.start();
//    List(1, 1, 3, 4, -1, 10, 11, -3).foreach(item => {
//      eval = eval.next(item);
//      eval.maybeOutput match {
//        case Some(output) => println(output());
//        case None => println("Undefined")
//      }
//    })
//  }
}