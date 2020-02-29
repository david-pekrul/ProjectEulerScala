package euler

object Problem684 {

  val answerMod = 1000000007
  lazy val modMap = quickModLookup(10, answerMod, 60)

  def main(args: Array[String]): Unit = {

    val startTime = System.currentTimeMillis()

    val upperLimit = 90
    val fibonacci = Numbers.fibonacciBigInt(upperLimit + 1).force.toSeq

    val maxFib = fibonacci.last
//    println(s"MaxFib: $maxFib")
//    println(numberToDigitSumMod_FAST(maxFib))

    val truncatedFib = fibonacci.drop(2)

    lazy val answer = (2 to upperLimit)
      .map(outerIndex => {
        def startIndex = 1

        def endIndex = fibonacci(outerIndex)

        def modSum = sumModOfInverseDigitSumsOverRange(startIndex, endIndex)

        val x = (outerIndex, endIndex, modSum)
//        println(x)
        x
      })
      .map(_._3)
      .fold(BigInt(0)) { case (acc, next) => (acc + next).mod(answerMod) }

    val endTime = System.currentTimeMillis()
    println(answer)

    println(s"Calculation time: ${(endTime-startTime)/1000.0}")
  }

  def old_numberToDigitSumNumber(input: BigInt): BigInt = {
    val numberOfNines = (input / 9).intValue()
    val string = s"${input % 9}" + (BigInt(1) to numberOfNines).map(_ => "9").mkString("")
    //    println(s"$numberOfNines ==> $string")
    BigInt(string)
  }

  def numberToDigitSumNumber(input: BigInt): BigInt = {
    val exponent = (input / 9).intValue
    val firstDigit = (input % 9) + 1
    val x = (firstDigit.intValue * BigInt(10).pow(exponent)) - 1
    x
  }

  def numberToDigitSumNumberMod(input: BigInt, modValue: Int): BigInt = {
    val exponent = (input / 9).intValue
    val firstDigit = (input % 9) + 1
    val x = ((firstDigit.intValue * BigInt(10).pow(exponent).mod(modValue)) - 1).mod(modValue)
    println(s"$input, exponent: $exponent")
    x
  }

  def numberToDigitSumMod_FAST(input: BigInt) = {

    /*
    input = fibonacci number
    The number who's digits sum to the fibonacci number = ((firstDigit+1) * 10^exponent) - 1 ==> digitSumNumber

    The output of this function needs to be digitSumNumber % answerMod
    [(firstDigit+1) * (10^exponent mod answerMod) - 1].mod(answerMod)

    The tough part is getting (10^exponent mod answerMod).
    To make it efficient, I use Modular Exponentiation to perform congruency operations.
    10^exponent can be written as a set of 10^(2^x), which I will call a ModComponent
     */

    val exponent = (input / 9)
    val firstDigit = (input % 9) + 1

    val (powTenExponents: Seq[BigInt], powTenModValues: Seq[BigInt], remainder: BigInt) =
      modMap.reverse.fold((Seq(), Seq(), exponent)) { case ((expValues: Seq[BigInt], modValues: Seq[BigInt], remainder: BigInt), nextModComponent: (Int, BigInt, BigInt)) => {
        if (nextModComponent._2 <= remainder) {
          val nextExpSeq = expValues :+ nextModComponent._1
          val nextSeq = modValues :+ nextModComponent._3
          val nextRemainder = remainder - nextModComponent._2
          (nextExpSeq, nextSeq, nextRemainder)
        } else {
          (expValues, modValues, remainder)
        }
      }
      }


    val tenToExponentMod = powTenModValues.fold(BigInt(1)) { case (product, nextPowTenMod) => {
      (product * nextPowTenMod).mod(answerMod)
    }
    }


    val returnValue = ((tenToExponentMod * firstDigit) - 1).mod(answerMod)

    //    println(s"input:$input, firstDigit:$firstDigit*10^$exponent % $answerMod = $returnValue")
    def tenBrokenUp = powTenExponents.map(x => "(10^" + x.bigInteger.toString() + ")")
    //    println(s"$exponent = 2^${powTenExponents.toString}")

    returnValue
  }

  def quickModLookup(baseValue: Int, modValue: Int, maxExponent: Int) = {
    /*
    1 -> 10^1 mod modValue
    2 -> 10^2 mod modValue
    4 -> 10^4 mod modValue
    8 -> 10^8 mod modValue
     */
    var previousMod = BigInt(baseValue % modValue)
    (0 to maxExponent)
      .map(x => x -> BigInt(2).pow(x))
      .map {
        case (0, x) => {
          previousMod = baseValue
          (0, x, previousMod)
        }
        case (exponent, powerOfTwo) => {
          previousMod = previousMod.pow(2).mod(modValue)
          val next = (exponent, powerOfTwo, previousMod)
          next
        }
      }
  }

  //  def modInverse(input: BigInt, modValue: BigInt): BigInt = {
  //    input.modInverse(modValue)
  ////    (modValue+1)/input
  //  }

  def sumModOfInverseDigitSumsOverRange(startingIndex: Int, endingIndex: BigInt): BigInt = {
    /* https://math.stackexchange.com/questions/209591/modulo-of-series-summation */
    /* For this problem, endingIndex should be Fib[n] */
    val startingQIndex = endingIndex - (endingIndex % 9) + 1
    val k = endingIndex / 9


    val (firstTerm, secondTerm) = if (endingIndex < BigInt(10)) {
      (BigInt(0), BigInt(0))
    }
    else {
      val f = (((BigInt(10).modPow(k, answerMod)) -1) *6).mod(answerMod)
      val s = (9 * k.mod(answerMod)) % answerMod
      (f,s)
    }

    val Q = sumModOfInverseDigitSumsRemainingQ(startingQIndex, endingIndex)

    val x = (firstTerm - secondTerm + Q).mod(answerMod)
    x
  }

  /** *
   * This function is for getting the mod-sum of the remaining up-to 8 terms in Q that fall outside
   * the closed formula for the sum.
   *
   * @param startingIndex
   * @param endingIndex
   * @return
   */
  def sumModOfInverseDigitSumsRemainingQ(startingIndex: BigInt, endingIndex: BigInt): BigInt = {
    if (endingIndex - startingIndex >= 9) {
      throw new RuntimeException("The range is toooooo big for the remainder!")
    }

    (startingIndex to endingIndex)
      .map(n => numberToDigitSumMod_FAST(n))
      .fold(BigInt(0)) { case (acc, next) => (acc + next).mod(answerMod) }
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