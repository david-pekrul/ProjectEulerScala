package euler

object Problem2 {
  def main(args: Array[String]): Unit = {
    println("Hello 2")
    println(Numbers.findPrimesBelowLimit(10000000).size)
  }
}