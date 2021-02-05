package be.bruyere.romain

object Main3 {

  sealed trait QRE[InDomain,OutDomain] {
    def configure[FnCost](): Combinator[_,_,_,_];
  }

  case class IterQRE[InDomain,AggDomain,ChildDomain,OutDomain](init: AggDomain, transformF: (AggDomain,ChildDomain) => AggDomain, outputF: AggDomain => OutDomain) {
//    override def configure[FnCost](): Combinator[_, _, _, _] = {
//      Iter(child.configure[ChildDomain](),init,transformF,outputF,None)
//    }

//    def configure(child: QRE[InDomain,ChildDomain]): Combinator[InDomain,AggDomain,ChildDomain,OutDomain] = {
//      Iter(child.configure[AggDomain](),init,transformF,outputF,None)
//    }
  }

  case class AtomQRE[InDomain,OutDomain](outputF: InDomain => OutDomain, predicate: InDomain => Boolean) extends QRE[InDomain,OutDomain] {

    //InDomain,FnCost,OutDomain,FnCost
    override def configure[FnCost](): Combinator[_, _, _, _] = {
      Atom[InDomain,OutDomain,FnCost](outputF,predicate,None,None)
    }
  }

  def test(): Unit = {
    val atomPos = new AtomQRE[Int,String](x => x.toString, x => x > 0);
    val iterPos = new IterQRE[Int,String,String,Char]("0", (x, y) => x.concat(" + " + y), x => x.charAt(0));

    val eval = atomPos.configure[Double]()



//    iterPos.configure(atomPos);
  }

  sealed trait Combinator[Domain,Cost,FnDomain,FnCost] {
    val maybeOutput: Option[() => Cost];
    def next(item: Domain): Combinator[Domain,Cost,FnDomain,FnCost];
    def start(current: FnDomain => FnCost): Combinator[Domain,Cost,FnDomain,FnCost];
  }

  case class Atom[InDomain,OutDomain,FnCost] private(outputF: InDomain => OutDomain, predicate: InDomain => Boolean, current: Option[InDomain => FnCost], maybeOutput: Option[() => FnCost]) extends Combinator[InDomain,FnCost,OutDomain,FnCost] {

    def this(outputF: InDomain => OutDomain, predicate: InDomain => Boolean) = this(outputF, predicate, None, None);

    def next(item: InDomain): Atom[InDomain,OutDomain,FnCost] = {
      if(predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, Some(() => current.get(item)));
      } else {
        Atom(outputF, predicate, None, None);
      }
    }

    def start(current: OutDomain => FnCost): Atom[InDomain,OutDomain,FnCost] = {
      //andThen : x -> current(output(x))
      Atom(outputF, predicate, Some(outputF andThen current), None);
    }

    def start(): Atom[InDomain,OutDomain,OutDomain] = {
      Atom[InDomain,OutDomain,OutDomain](outputF,predicate,Some(outputF),None)
    }
  }

  case class Iter[D,A,B,M] private (child: Combinator[D,B,A,B], init: B, transformF: (B,A) => B, outputF: B => M, maybeOutput: Option[() => M]) extends Combinator[D,M,A,A] {

    def this(child: Combinator[D,B,A,B], init: B, transformF: (B,A) => B, outputF: B => M) = this(child, init, transformF, outputF, None)

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


//  sealed trait Combinator[Domain,T,Cost,N] {
//    val maybeOutput: Option[() => Cost];
//    def next(item: Domain): Combinator[Domain,T,Cost,N];
//    def start(current: N => T): Combinator[Domain,T,Cost,N];
//    def start(): Combinator[Domain,N,_,N];
//    def map[E,F](f: Combinator[Domain,T,Cost,N] => Combinator[E,F,Cost,N]): Combinator[E,F,Cost,N] = f(this)
//  }
//
//  case class Atom[D,M,N] private (outputF: D => N, predicate: D => Boolean, current: Option[D => M], maybeOutput: Option[() => M]) extends Combinator[D,M,M,N] {
//
//    def this(outputF: D => N, predicate: D => Boolean) = this(outputF, predicate, None, None);
//
//    def next(item: D): Atom[D,M,N] = {
//      if(predicate(item) && current.isDefined) {
//        Atom(outputF, predicate, None, Some(() => current.get(item)));
//      } else {
//        Atom(outputF, predicate, None, None);
//      }
//    }
//
//    def start(current: N => M): Atom[D,M,N] = {
//      //andThen : x -> current(output(x))
//      Atom(outputF, predicate, Some(outputF andThen current), None);
//    }
//
//    def start(): Atom[D,N,N] = {
//      Atom[D,N,N](outputF,predicate,Some(outputF),None)
//    }
//  }
//
//  case class Iter[D,A,B,M] private (child: Combinator[D,B,B,A], init: B, transformF: (B,A) => B, outputF: B => M, maybeOutput: Option[() => M]) extends Combinator[D,A,M,A] {
//
//    def this(child: Combinator[D,B,B,A], init: B, transformF: (B,A) => B, outputF: B => M) = this(child, init, transformF, outputF, None)
//
//    def next(item: D): Iter[D,A,B,M] = {
//      val combinator = child.next(item);
//
//      combinator.maybeOutput match {
//        case Some(output) => {
//          def h(x:A) : B = transformF.curried(output())(x);
//          Iter[D,A,B,M](child.start(h), init, transformF, outputF, Some(() => outputF(output())));
//        }
//        case None => Iter[D,A,B,M](combinator, init, transformF, outputF, None);
//      }
//    }
//
//    def start(current: A => A): Iter[D,A,B,M] = {
//      def h(x:A) : B = transformF.curried(init)(current(x));
//      Iter(child.start(h), init, transformF, outputF, Some(() => outputF(init)));
//    }
//
//    def start(): Iter[D,A,B,M] = {
//      def h(x:A) : B = transformF.curried(init)(x);
//      Iter[D,A,B,M](child.start(h), init, transformF, outputF,Some(() => outputF(init)))
//    }
//  }

  // childL : Iter[Int, Int, String, Int]
//  case class Split[D,A,B,C] private (childL: Combinator[D,Int,A,String], childR: Combinator[D,Any,B,B], transformF: (A,B) => C, maybeOutput: Option[() => C]) extends Combinator[D,Any,C,Any] {
//
//    def this(childL: Combinator[D,String,A,Int], childR: Combinator[D,Any,B,B], transformF: (A,B) => C) = this(childL, childR, transformF, None)
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
//    def start(current: Int => String): Split[D,A,B,C] = {
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

    test();
//    val atomPos = new Atom[Int,String,Int](x => {
//      println("AtomPos");
//      x
//    }, x => x > 0);
//    val iterPos = new Iter[Int,Int,String,String](atomPos, "0", (x, y) => {
//      println("Iter");
//      x.concat(" + " + y.toString)
//    }, x => x);
//    val atomNeg = new Atom[Int,Int,Int](x => {
//      println("AtomNeg")
//      x
//    }, x => x < 0);
//    val split = new Split[Int,String,Int,String](iterPos,atomNeg, (x,y) => {
//      println("Split")
//      x + " / " + y.toString
//    });
//
//    var eval = split.start();
//    List(1, 1, 1, 1, 1).foreach(item => {
//      eval = eval.next(item);
//      eval.maybeOutput match {
//        case Some(output) => println(output());
//        case None => println("Undefined")
//      }
//    })
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