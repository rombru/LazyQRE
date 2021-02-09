package be.bruyere.romain

import be.bruyere.romain.QREUtils.executeOnList
import be.bruyere.romain.eval.EvalExtension.StartEval
import be.bruyere.romain.qre.{AtomQRE, CombineQRE, IterQRE, StreamingCompositionQRE}

import scala.annotation.tailrec

object Main {


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
    executeOnList[String, Int](list, eval, (sc: StartEval[String, Int, () => Int]) => {
      println("Result = " + sc.result())
      println("ResultFn = " + sc.resultFn())
    })
  }
}