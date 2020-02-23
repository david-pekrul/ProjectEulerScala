package euler

object Problem187 {
  def main(args: Array[String]): Unit = {

//        val upperLimit = 30
    val upperLimit = Math.pow(10,8).intValue
    val primes = Numbers.findPrimesBelowLimit(upperLimit / 2 + 1)

    def x = primes.zip(primes.tails.toSeq)

    val answer = x.map{case (p,q) => {
      val primesThatWorkWithP = q.takeWhile(r => {
        upperLimit / r >= p
      })
      primesThatWorkWithP.size
    }}.sum

    println(answer)
  }
}
