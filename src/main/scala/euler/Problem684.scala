package euler

object Problem684 {
  def main(args: Array[String]): Unit = {

    val upperLimit = 90
    val answerMod = 1000000007
    val fibonacci = Numbers.fibonacciBigInt(upperLimit+1).force.toSeq

    val maxFib = fibonacci.last
    println(s"MaxFib: $maxFib")

    val answerNaturalNumbers =
    //      fibonacci.drop(2) // drop the first 0 and 1 of the fibonacci sequence. This problem starts at index 2
      (1 to 20)
        .map(fib => numberToDigitSumNumber(fib).mod(answerMod))
        .reverse.zipWithIndex.map(x => (x._2+1,x._1)) // pair each fibonacci number with the multiplication factor
        .map{case (scalar,sNum) => sNum}
        .fold(BigInt(0)){case (sum,next) => (sum+next).mod(answerMod)}
        .mod(answerMod)
    println(s"answerNaturalNumbers $answerNaturalNumbers")



    var index = 0;
    val answer =
      fibonacci.drop(2) // drop the first 0 and 1 of the fibonacci sequence. This problem starts at index 2
      .map(fib => numberToDigitSumNumber(fib).mod(answerMod))
      .map(x => {println(s"$index -> $x");index=index+1;x})
      .reverse.zipWithIndex.map{case (sSum,index) => (index+1,sSum)} // pair each S(fib) sum number with the multiplication factor
//      .map(x => {println(x);x})
      .map{case (scalar,sNum) => sNum*scalar}
//      .map(x => {println(x);x})
      .fold(BigInt(0)){case (sum,next) => (sum+next).mod(answerMod)}
      .mod(answerMod)

    println(s"Answer $answer")

/*This is for checking the S(20) = 1074 case*/
//    val naturalNumbers = (1 to 20)
//
//    val x = (BigInt(0) to maxFib by 9).map(maxSum => {
//      val next = (1 to 9).map(i => {
//        if (naturalNumbers.contains(maxSum + i)) {
//          println(s"Found answer for ${maxSum + i} = ${i + start}")
//          Some(BigDigitSum(i + start, maxSum + i))
//        } else {
//          None
//        }
//      })
//      start = start + "9"
//      next
//    })

//    val answer = fibonacci
//
//      .fold(BigInt(0)){case (sum,next) => sum+next}.mod(answerMod)
  }

  def numberToDigitSumNumber(input: BigInt): BigInt = {
    val numberOfNines = (input/9).intValue()
    val string = s"${input%9}" + (BigInt(1) to numberOfNines).map(_ => "9").mkString("")
    println(s"$numberOfNines ==> $string")
    BigInt(string)
  }
}

//case class BigDigitSum(str: String, sum: BigInt) {
//  def bigNum = BigInt(str)
//}
//
//case class BigDigitSum2(firstDigit: Int, numberOfNines: Int, sum: Int) {
//  def bigNum = {
//    BigInt("" + firstDigit + (1 to numberOfNines).map("9").mkString(""))
//  }
//}