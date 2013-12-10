package com.condenast.ml

class Sort {
  
  def main(args: Array[String]): Unit = {
      sort("5736576259")
  }
  
  def sort(input: String): String = {
    quicksort(input.toCharArray, 0, input.length)
  }
  
  def quicksort(input: Array[Char], start: Int, end: Int): String = {
    if (start < end) {
      var pivot: Int = start + (end - start + 1) / 2
      var index = partition(input, start, end, pivot)
      quicksort(input, start, index - 1)
      quicksort(input, index + 1, end)
    }
    input.mkString
  }
  
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
  }
}