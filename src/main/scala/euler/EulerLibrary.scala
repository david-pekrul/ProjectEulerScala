package euler

import spire.math.ULong

import scala.collection.mutable

object Numbers {
  def findPrimesBelowLimit(end: Int): Seq[Int] = {
    val primeIndices = mutable.ArrayBuffer.fill((end + 1) / 2)(1)
    val intSqrt = Math.sqrt(end).toInt
    for (i <- 3 to end by 2 if i <= intSqrt) {
      for (nonPrime <- i * i to end by 2 * i) {
        primeIndices.update(nonPrime / 2, 0)
      }
    }
    Seq(2) ++ (for (i <- primeIndices.indices if primeIndices(i) == 1) yield 2 * i + 1).tail.toList
  }

  def isPrimeFast(number: Int): Boolean = {
    if(number<2) false
    if(number<4) false
    if(number%2 == 0) false
    if(number<9) false
    if(number%3 == 0) false
    for(i <- 5 to Math.sqrt(number).intValue() by 6){
      if(number%i == 0) return false
      if(number%(i+2) == 0) return false
    }
    true
  }



  def distinctPrimeFactors(input: Int, primes: Option[Seq[Int]] = None): Seq[Int] = {
    primes.getOrElse(findPrimesBelowLimit(input)).takeWhile(p => p <= input).filter(prime => input%prime==0)
  }

//  def primeFactors(input: Int, primes: Option[Seq[Int]] = None): Seq[Int] = {
//    primes.getOrElse(findPrimesBelowLimit(input)).takeWhile(p => p <= input).filter(prime => input%prime==0)
//  }


  def fibonacci(terms: Int): Stream[Int] = {
    lazy val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
    fibs.take(terms)
  }

  def fibonacciBigInt(terms: Int): Stream[BigInt] = {
    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
    fibs.take(terms)
  }


//  def sqt(n:ULong) :ULong = {
//    val d = BigDecimal(n)
//    var a = BigDecimal(1.0)
//    var b = d
//    while(b-a >= 0) {
//      val mid = (a+b)/2
//      if (mid*mid-d > 0) b = mid-0.0001  //adjust down
//      else               a = mid+0.0001  //adjust up
//    }
//    ULong(b.toLong)
//  }

}