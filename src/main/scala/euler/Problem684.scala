package euler

object Problem684 {

  val answerMod = 1000000007
  lazy val modMap = quickModLookup(10, answerMod, 60)

  def main(args: Array[String]): Unit = {

    val upperLimit = 90
    val fibonacci = Numbers.fibonacciBigInt(upperLimit + 1).force.toSeq
    val naturalNumbers = (1 to 20)

    val maxFib = fibonacci.last
    println(s"MaxFib: $maxFib")
    println(numberToDigitSumMod_FAST(maxFib))

    val x = (BigInt(1) to BigInt(10).pow(8)).map{n =>
      n -> numberToDigitSumNumber(n)
    }.takeWhile(p => p._2 <= answerMod)


    println(x.size)



/*    var index = 1;
    val answerNaturalNumbers =
      naturalNumbers
        .map(fib => fib -> numberToDigitSumMod_FAST(fib))
        .map(x => {
          index = index + 1;
          x // (index, (fibonacci, digitSumMod))
        })
        .map(x => x._2)
        .reverse.zipWithIndex
        .map { case (sSum, index) => (index + 1, sSum) } // pair each S(fib) sum number with the multiplication factor
        .map { case (scalar, sNum) => {
          sNum * 1
        }
        }
        .fold(BigInt(0)) { case (sum, next) => (sum + next).mod(answerMod) }
        .mod(answerMod)
    println(s"answerNaturalNumbers: $answerNaturalNumbers")*/

    //    val answerNaturalNumbers =
    //    //      fibonacci.drop(2) // drop the first 0 and 1 of the fibonacci sequence. This problem starts at index 2
    //      (1 to 20)
    //        .map(fib => numberToDigitSumNumber(fib).mod(answerMod))
    //        .reverse.zipWithIndex.map(x => (x._2+1,x._1)) // pair each fibonacci number with the multiplication factor
    //        .map{case (scalar,sNum) => sNum}
    //        .fold(BigInt(0)){case (sum,next) => (sum+next).mod(answerMod)}
    //        .mod(answerMod)
    //    println(s"answerNaturalNumbers $answerNaturalNumbers")

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
    println(s"input:$input, firstDigit:$firstDigit*10^$exponent % $answerMod = $returnValue")
    def tenBrokenUp = powTenExponents.map(x => "(10^"+x.bigInteger.toString()+")")
    println(s"$exponent = 2^${powTenExponents.toString}")

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