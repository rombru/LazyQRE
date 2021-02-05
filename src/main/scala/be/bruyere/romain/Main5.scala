package be.bruyere.romain

object Main5 {

    sealed trait Combinator[D,C,T] {
      val maybeOutput: Option[() => T];
      def next(item: D): Combinator[D,C,T];
      def start(current: C => T): Combinator[D,C,T];
      def start(): Combinator[D,C,C];
    }

    case class Atom[D,C,T] private(outputF: D => C, predicate: D => Boolean, current: Option[C => T], maybeOutput: Option[() => T]) extends Combinator[D,C,T] {

      def this(outputF: D => C, predicate: D => Boolean) = this(outputF, predicate, None, None);

      def next(item: D): Atom[D,C,T] = {
        if(predicate(item) && current.isDefined) {
          Atom(outputF, predicate, None, Some(() => current.get(outputF(item))));
        } else {
          Atom(outputF, predicate, None, None);
        }
      }

      def start(current: C => T): Atom[D,C,T] = {
        Atom[D,C,T](outputF, predicate, Some(current), None);
      }

      def start(): Atom[D,C,C] = {
        Atom[D,C,C](outputF,predicate,Some(identity),None)
      }
    }

  // "5 / " + x =====> "5 / " + (3 + x).toString
    case class Iter[D,A,B,C,T] private(child: Combinator[D,A,B], init: B, transformF: (B,A) => B, outputF: B => C, current: Option[C => T], maybeOutput: Option[() => T]) extends Combinator[D,C,T] {

      def this(child: Combinator[D,A,B], init: B, transformF: (B,A) => B, outputF: B => C) = this(child, init, transformF, outputF, None, None)

      def next(item: D): Iter[D,A,B,C,T] = {
        val combinator = child.next(item);

        combinator.maybeOutput match {
          case Some(output) => {
            def h(x:A) : B = transformF.curried(output())(x);
            Iter[D,A,B,C,T](child.start(h), init, transformF, outputF, current, Some(() => current.get(outputF(output()))));
          }
          case None => Iter[D,A,B,C,T](combinator, init, transformF, outputF, current, None);
        }
      }

      def start(current: C => T): Iter[D,A,B,C,T] = {
        def h(x:A) : B = transformF.curried(init)(x);
        Iter(child.start(h), init, transformF, outputF, Some(current), Some(() => current(outputF(init))));
      }

      def start(): Iter[D,A,B,C,C] = {
        def h(x:A) : B = transformF.curried(init)(x);
        Iter[D,A,B,C,C](child.start(h), init, transformF,outputF,Some(identity),Some(() => outputF(init)))
      }
    }

  // 13 + 12 + 14 + (("Test" + 5) -> 5.9) ///// "Test" => "Test" / 5 => 5
//    case class Split[D,A,B,C,T] private (childL: Combinator[D,A,A], childR: Combinator[D,B,C], transformF: (A,B) => C, maybeOutput: Option[() => T]) extends Combinator[D,C,T] {
//
//      def this(childL: Combinator[D,A,A], childR: Combinator[D,B,C], transformF: (A,B) => C) = this(childL, childR, transformF, None)
//
//      def next(item: D): Split[D,A,B,C,T] = {
//        val combinatorL = childL.next(item);
//
//        combinatorL.maybeOutput match {
//          case Some(outputL) => {
//            def h(x:B) : C = transformF.curried(outputL())(x);
//            val combinatorR = childR.start(h);
//            Split[D,A,B,C,T](combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
//          }
//          case None =>  {
//            val combinatorR = childR.next(item);
//            combinatorR.maybeOutput match {
//              case Some(output) => Split[D,A,B,C,T](combinatorL, combinatorR, transformF, current, current.get() output)
//            }
//          }
//        }
//      }
//
//      def start(current: C => T): Split[D,A,B,C,T] = {
//        val combinatorL = childL.start(identity);
//
//        combinatorL.maybeOutput match {
//          case Some(outputL) => {
//            val combinatorR = childR.start(identity);
//
//            combinatorR.maybeOutput match {
//              case Some(_) => Split[D,A,B,C,T](combinatorL, combinatorR, transformF, Some(current), combinatorR.maybeOutput)
//              case None => Split[D,A,B,C,T](combinatorL, combinatorR, transformF, Some(current), None)
//            }
//          }
//          case None => Split[D,A,B,C,T](combinatorL, childR, transformF, Some(current), None)
//        }
//      }
//
//      def start(): Split[D,A,B,C,T] = {
//        Split[D,A,B,C,T](childL.start(), childR, transformF, None)
//      }
//    }

  def main(args: Array[String]): Unit = {
    val atomPos = new Atom[Int,Int,Int](x => x, x => x > 0);
    val iterPos = new Iter[Int,Int,Int,String,String](atomPos, 0, (x, y) => x + y, x => x.toString);

    var eval = iterPos.start((x:String) => x.concat("122222"));
    eval = eval.next(5);
    println(eval.maybeOutput.get())
    eval = eval.next(5);
    println(eval.maybeOutput.get())
  }
}