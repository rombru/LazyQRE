package be.bruyere.romain

import be.bruyere.romain.QREUtils.executeOnList
import be.bruyere.romain.eval.EvalExtension.StartEval
import be.bruyere.romain.qre.{AtomQRE, CombineQRE, IterQRE, SplitQRE, TumblingWindowQRE, WindowQRE}

import scala.util.Random

object Main {


  def main(args: Array[String]): Unit = {

//    val filterCars = (x:String) => x == "C"
//    val mapToOne = (x:String) => 1
//
//    val vehicles = List("C","M,","T","C","T")
//    val count = vehicles
//      .filter(filterCars)
//      .map(mapToOne)
//      .sum


//    val addition5 = (x:Int) => x + 5
//    val multiplyBy10 = (fn : Int => Int, x: Int) => fn(x * 10)
//    val result = multiplyBy10(addition5, 3)

//    val addition5 = (x:Int) => x + 5
//    val multiplyBy10 = (x: Int) => x * 10
//    val result = (addition5 compose multiplyBy10)(3)

//    val addition1 = (x:Int,y:Int) => x + y
//    val result1 = addition1(2,3)
//
//    val addition2 = (x:Int) => (y:Int) => x + y
//    val result2 = addition2(2)(3)
//
//    val addition3 = (x:Int) => (y:Int) => x + y
//    val partialAddition = addition3(2)
//    val result3 = partialAddition(3)
//
//    val addition4 = addition1.curried
//    val result4 = addition4(2)(3)


//    val number:Option[Double] = Some(2)
//    val none:Option[Double] = None
//
//    def divide(x:Double, y:Double) = if (y == 0) None else Some(x/y)
//
//    val div1 = divide(4,2) // Some(2)
//    val div2 = divide(4,0) // None
//
//    val mult1 = div1.map(x => x * 3) // Some(6)
//    val mult2 = div2.map(x => x * 3) // None



//    val notFlattened:Option[Option[Double]] = mult1.map(x => if (x > 10) Some(x) else None) // Some(None)
//    val flattened:Option[Double] = mult1.flatMap(x => if (x > 10) Some(x) else None) // None

//    case class Vehicle(vehicleBrand: String, vehicleType: String)
//
//    val v1 = Vehicle("Mercedes", "Car")
//    val v2 = Vehicle("Mercedes", "Car")
//
//    println(v1 == v2)

//    abstract class Vehicle(vehicleBrand: String)
//
//    case class Car(brand: String) extends Vehicle(brand)
//    case class Truck(brand: String, load:String) extends Vehicle(brand)
//    case class Moto(brand: String) extends Vehicle(brand)
//
//    def printVehicle(vehicle: Vehicle): Unit = {
//      vehicle match {
//        case Car(brand) => println(s"This is a car from the brand $brand")
//        case Truck(brand, _) => println(s"This is a truck from the brand $brand")
//        case Moto(brand) => println(s"This is a moto from the brand $brand")
//      }
//    }
//
//
//    def printOption(option: Option[Int]): Unit = {
//      option match {
//        case Some(value) => println(s"The contained value is $value")
//        case None => println(s"There is no contained value")
//      }
//    }


//    println(result)

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

//    val atom1 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
//    val iter1 = IterQRE[String, Int, Int, Int](atom1, 0, (x,y) => x + y, x => x)
//    val atom2 = AtomQRE[Int, Int](x => x + 5, _ => true)
//    val atom3 = AtomQRE[Int, Int](x => x + 10, _ => true)
//    val combine = CombineQRE[Int,Int,Int,Int](atom2, atom3, (x,y) => x * y)
//    val iter2 = IterQRE[Int, Int, Int, Int](combine, 0, (x,y) => x + y, x => x)
//    val streamcomp = StreamingCompositionQRE[String, Int, Int](iter1, iter2)

//    val atom1 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
//    val iter1 = IterQRE[String, Int, Int, Int](atom1, 0, (x,y) => x + y, 20, x => x)
//    val list = List("aaaaaa", "aaaaa", "sss")

//    val isVehicle = AtomQRE[String, Int](x => 1, x => x == "C")
//    val iterVehicle = IterQRE[String, Int, Int, Int](isVehicle, 0, (x,y) => x + y, 20, x => x)
//
//    val is15Min = AtomQRE[String, String](x => x, x => x == "M")
//    val split15Min = SplitQRE[String,Int,String,Int,Int](iterVehicle, is15Min, (x,y) => x, x => x)
//    val window1Hour = WindowQRE[String, Int, Int, Int](split15Min, 0, (x, y) => x + y, x => x, 4)
//
//    val iter15Min = IterQRE[String, Int, Int, Int](split15Min, 0, (x,y) => y, 20, x => x)
//
//    val density = CombineQRE[String, Int, Int, Double](window1Hour, iter15Min, (x,y) => x.toDouble / (4 * y.toDouble))


//    val t = Test;
//    t.run

    this.peakHourFactor();
//    this.sum();

//    val iter1 = TumblingWindowQRE[Int, Int, Int, Int](atom1, 0, (x, y) => x + y, x => x, 3)

//    val eval = density.start()
//    val list = List("M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "C", "C", "M",  "C", "C", "C", "C", "M")
//    executeOnList[String, Double](list, eval, (sc: StartEval[String, Double]) => {
//      println("Result = " + sc.result())
//      println("ResultFn = " + sc.resultFn())
//    })
  }

//  def capacityOfClimbingLane() : {
//    val list = List("67", "80", "65", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "C", "C", "M",  "C", "C", "C", "C", "M")
//  }

  def peakHourFactor(): Unit = {
    val list = List("M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "M", "C", "C", "C", "C", "C", "C", "M",  "C", "C", "C", "C", "M")


    val isVehicleToken = AtomQRE[String, Int]( x => x == "C", x => 1)
    val is15MinToken = AtomQRE[String, String](x => x == "M", x => x)

    val sumOfVehicle = IterQRE[String, Int, Int, Int](isVehicleToken, 0, (x,y) => x + y, x => x, 20)
    val sumOfVehiclesDuring15Min = SplitQRE[String,Int, Int,String,Int](sumOfVehicle, is15MinToken, (x,y) => x, x => x)
    val sumOfVehicleDuring1Hour = WindowQRE[String, Int, Int, Int](sumOfVehiclesDuring15Min, 0, (x, y) => x + y, x => x, 4)

    val sumOfVehicleLast15Min = IterQRE[String, Int, Int, Int](sumOfVehiclesDuring15Min, 0, (x,y) => y, x => x, 20)
    val peakCoefficientForLast15Min = CombineQRE[String, Double, Int, Int](sumOfVehicleDuring1Hour, sumOfVehicleLast15Min, (x,y) => x.toDouble / (4 * y.toDouble))

    val eval = peakCoefficientForLast15Min.start()
    executeOnList[String, Double](list, eval, (sc: StartEval[String, Double]) => {
//      println("Result = " + sc.result())
//      println("ResultFn = " + sc.resultFn())
    })
  }

  def sum(): Unit = {
    val list = List("C", "C", "C", "C")

    val isVehicleToken = AtomQRE[String, Int](x => x == "C",x => 1)
    val sumOfVehicle = IterQRE[String, Int, Int, Int](isVehicleToken, 0, (x,y) => x + y, x => x, 1000)

    val eval = sumOfVehicle.start()
    executeOnList[String, Int](list, eval, (sc: StartEval[String, Int]) => {
      println("Result = " + sc.result())
      println("ResultFn = " + sc.resultFn())
    })
  }

  def peakHourFactorInfinite(): Unit = {
    val isVehicleToken = AtomQRE[String, Int](x => x == "C",x => 1)
    val is15MinToken = AtomQRE[String, String](x => x == "M",x => x)

    val sumOfVehicle = IterQRE[String, Int, Int, Int](isVehicleToken, 0, (x,y) => x + y, x => x, 1000)
    val sumOfVehiclesDuring15Min = SplitQRE[String,Int, Int,String,Int](sumOfVehicle, is15MinToken, (x,y) => x, x => x)
    val sumOfVehicleDuring1Hour = WindowQRE[String, Int, Int, Int](sumOfVehiclesDuring15Min, 0, (x, y) => x + y, x => x, 4)

    val sumOfVehicleLast15Min = IterQRE[String, Int, Int, Int](sumOfVehiclesDuring15Min, 0, (x,y) => y, x => x, 1000)
    val peakCoefficientForLast15Min = CombineQRE[String, Double, Int, Int](sumOfVehicleDuring1Hour, sumOfVehicleLast15Min, (x,y) => x.toDouble / (4 * y.toDouble))

    var eval = peakCoefficientForLast15Min.start()

    val list = List("C","M")
    for(i <- 0 until 10000000) {
      eval = eval.next(getRandomElement(list,new Random()))
      checkJvm()
      println("Result = " + eval.result())
//      println("ResultFn = " + eval.resultFn())
    }
  }

  def getRandomElement[A](seq: Seq[A], random: Random): A =
    seq(random.nextInt(seq.length))

  def checkJvm() = {
    val heapSize = Runtime.getRuntime.totalMemory

    // Get maximum size of heap in bytes. The heap cannot grow beyond this size.// Any attempt will result in an OutOfMemoryException.
    val heapMaxSize = Runtime.getRuntime.maxMemory

    // Get amount of free memory within the heap in bytes. This size will increase // after garbage collection and decrease as new objects are created.
    val heapFreeSize = Runtime.getRuntime.freeMemory

//    System.out.println("heapsize" + formatSize(heapSize))
//    System.out.println("heapmaxsize" + formatSize(heapMaxSize))
    System.out.println("heapFreesize" + formatSize(heapFreeSize))
  }

  def formatSize(v: Long): String = {
    if (v < 1024) return v + " B"
    val z = (63 - numberOfLeadingZeros(v)) / 10
    String.format("%.1f %sB", v.toDouble / (1L << (z * 10)), " KMGTPE".charAt(z))
  }

  def numberOfLeadingZeros(l: Long): Int = {
    if (l < 0) 0
    else if (l > 0xffffffffL) Integer.numberOfLeadingZeros((l >>> 32).toInt)
    else Integer.numberOfLeadingZeros((l & 0xffffffffL).toInt) + 32 // is this equivalent to l.toInt?
  }
}