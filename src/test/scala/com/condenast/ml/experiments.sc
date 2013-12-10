package com.condenast.ml

import scala.math.Ordering.Implicits._
import scala.collection.mutable.PriorityQueue


object Experiments  extends Enumeration {

	
    def diff(t: (String, Double)) = t._2          //> diff: (t: (String, Double))Double
    val score: ((String, Double)) => Double = t => t._2
                                                  //> score  : ((String, Double)) => Double = <function1>
    
    val q = new PriorityQueue[(String, Double)]()(Ordering.by(score))
                                                  //> q  : scala.collection.mutable.PriorityQueue[(String, Double)] = PriorityQueu
                                                  //| e()
    q.enqueue("a"->0.3)
    q.enqueue("b"->0.7)
    q.enqueue("c"->0.17)
    q.enqueue("d"->0.8)
 
 		val i = q.iterator                //> i  : Iterator[(String, Double)] = non-empty iterator
 		while (i.hasNext) {
 			println(q.dequeue)
 		}                                 //> (d,0.8)
                                                  //| (b,0.7)
                                                  //| (a,0.3)
                                                  //| (c,0.17)
      val l2: Double => Double = x => scala.math.log(x) / scala.math.log(2)
                                                  //> l2  : Double => Double = <function1>
      l2(1)                                       //> res0: Double = 0.0
    val mi: (Double, Double, Double, Double, Double) => Double = {(n, n11, n10, n01, n00) =>
      val l2: Double => Double = x => scala.math.log(x) / scala.math.log(2)
      (n11 / n) * l2((n * n11) / ((n11 + n10) * (n11 + n01))) +
      	(n01 / n) * l2((n * n01) / ((n01 + n00) * (n01 + n11))) +
      		(n10 / n) * l2((n * n10) / ((n11 + n10) * (n10 + n00))) +
      			(n00 / n) * l2((n * n00) / ((n01 + n00) * (n10 + n00)))
                                                  //> mi  : (Double, Double, Double, Double, Double) => Double = <function5>
    }
    
    val n=39392                                   //> n  : Int = 39392
    val n11=5                                     //> n11  : Int = 5
    val n10=3                                     //> n10  : Int = 3
    val n01=0                                     //> n01  : Int = 0
    val n00=39384                                 //> n00  : Int = 39384
    mi(n, n11, n10, n01, n00)                     //> res1: Double = NaN
    val m = 0.0001105                             //> m  : Double = 1.105E-4
    n11+n10+n01+n00                               //> res2: Int = 39392

		var k = Set[Int]()                //> k  : scala.collection.immutable.Set[Int] = Set()
		k = k + 1
		val r = k + 2                     //> r  : scala.collection.immutable.Set[Int] = Set(1, 2)
		
  def excludedList(l1: List[String], l2: List[String]) =
    l1 filter { t => l2.dropWhile(s => s != t) isEmpty }
                                                  //> excludedList: (l1: List[String], l2: List[String])List[String]
	
	var ex = excludedList(List("1","2","3","4", "7"), List("1","4"))
                                                  //> ex  : List[String] = List(2, 3, 7)

  def excluded2(l1: List[String], l2: List[String]) =
    l1 filter { t => !(l2.dropWhile(s => s != t) isEmpty) }
                                                  //> excluded2: (l1: List[String], l2: List[String])List[String]
	var ex2 = excluded2(List("1","2","3","4", "7"), List("1","4","2"))
                                                  //> ex2  : List[String] = List(1, 2, 4)
	List("1","2","3") dropWhile(s=> s != "2") //> res3: List[java.lang.String] = List(2, 3)

	val f3 = new Function1[List[Int],Collection[Int]]  {
		def apply(l: List[Int]): Collection[Int] = Set(l.size)
	}                                         //> f3  : java.lang.Object with List[Int] => Collection[Int] = <function1>
	f3.isInstanceOf[Iterable[Int]=>Collection[Int]]
                                                  //> res4: Boolean = true

  def sort(input: String): String = {
    quicksort(input.toCharArray, 0, input.length - 1)
  }                                               //> sort: (input: String)String
  
  def quicksort(input: Array[Char], start: Int, end: Int): String = {
    if (start < end) {
      var pivot: Int = start + (end - start + 1) / 2
      var index = partition(input, start, end, pivot)
      quicksort(input, start, index - 1)
      quicksort(input, index + 1, end)
    }
    input.mkString
  }                                               //> quicksort: (input: Array[Char], start: Int, end: Int)String
  
  def partition(input: Array[Char], start: Int, end: Int, pivot: Int): Int = {
    val v = input(pivot)
    val swap: (Int,Int) => Unit = (p,r) => { val tmp = input(p); input(p) = input(r); input(r) = tmp }
    
    swap(pivot, end)
    var index = start
    val range = Range(start, end)
    
    for (i <- range) {
     if (input(i) < v) {
       swap(i, index)
       index = index + 1
     }
    }
    swap(index, end)
    index
  }                                               //> partition: (input: Array[Char], start: Int, end: Int, pivot: Int)Int

  val input = "3chg3".toCharArray()               //> input  : Array[Char] = Array(3, c, h, g, 3)
	partition(input, 0, 4, 2)                 //> res5: Int = 4
	input                                     //> res6: Array[Char] = Array(3, c, 3, g, h)
	partition(input, 0, 2, 1)                 //> res7: Int = 2
	input                                     //> res8: Array[Char] = Array(3, 3, c, g, h)
	partition(input, 3, 4, 3)                 //> res9: Int = 3
	input                                     //> res10: Array[Char] = Array(3, 3, c, g, h)
  sort("QUICKSORT")                               //> res11: String = CIKOQRSTU
  sort("3chg3")                                   //> res12: String = 33cgh
  sort("M646739670257310hdgjg69350q3tudfhgh-2203fsdkghkg584671-38932579673046")
                                                  //> res13: String = --000001122223333333344455556666666777777889999Mdddffgggggh
                                                  //| hhhjkkqstu
}