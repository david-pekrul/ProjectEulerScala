package euler

object Problem684 {
  def main(args: Array[String]): Unit = {

    val upperLimit = 20
    val answerMod = 1000000007
    val fibonacci = Numbers.fibonacci(upperLimit).force.toSeq

    val maxFib = fibonacci.last

    println(s"MaxFib: $maxFib")

    var start = ""
    val x = (0 to maxFib by 9).map(maxSum =>{
      val next = (1 to 9).map(i =>  BigDigitSum(i+start,maxSum+i))
      start = start + "9"
      next
    })

    x.flatten.filter(bd =>  fibonacci.contains(bd.sum))
      .map(bd => (bd -> bd.modValue(answerMod)))
      .foreach(println)


  }

//  def sumOfDigits(input: Int): Int = {
//    var i = input
//    var sum = 0
//    while(i != 0){
//      sum += i%10
//      i = (i/10)
//    }
//    sum
//  }
}

case class BigDigitSum(str: String, sum: Int) {
  def modValue(mod: Int):Int = {
    if(str.length < mod.toString.length){
      return sum
    }
    val lastDigits = str.reverse.take(mod.toString.length).reverse
    val i = Integer.parseInt(lastDigits)
    i%mod
  }
}