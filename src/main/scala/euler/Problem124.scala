package euler

object Problem124 {
  def main(args: Array[String]): Unit = {

    val upperLimit = 100000
    val answerIndex = 10000
    val primes = Numbers.findPrimesBelowLimit(upperLimit)
    def x = (1 to upperLimit).map(i => (i -> rad(Numbers.distinctPrimeFactors(i,Some(primes))))).sorted(new Ordering[(Int,Int)]{
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        if(x._2 == y._2){
          x._1 - y._1
        } else {
          x._2 - y._2
        }
      }
    })

    println("Answer: ")
    println(x(answerIndex-1)._1)
  }

  def rad(input: Seq[Int]): Int = {
    input.fold(1)((prev,next) => prev*next)
  }
}
