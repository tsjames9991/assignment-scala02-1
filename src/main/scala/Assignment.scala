import com.sun.istack.internal.logging.Logger

class Assignment {

  def rotate(number: Int, list: List[Int]): List[Int] = {
    (number, list) match {
      case (0, _) => list
      case (_, head :: tail) => rotate(number - 1, tail ::: List(head))
      case (_, Nil) => Nil
    }
  }

  def sumOfSquares(digits: List[Int]): Int = digits.map(d => d * d).sum

  def getDigits(number: Int): List[Int] = {
    if (number < 10) {
      List(number)
    }
    else {
      (number % 10) :: getDigits(number / 10)
    }
  }

  def happyNumbers(number: Int, visited: List[Int] = Nil): Boolean = {
    sumOfSquares(getDigits(number)) match {
      case 1 => true
      case n => if (visited contains (n)) false else happyNumbers(n, n :: visited)
    }
  }

  def removeDuplicates(number: List[Int]): List[Int] = {
    number match {
      case Nil => Nil
      case head :: tail => head :: removeDuplicates(tail.dropWhile(_ == head))
    }
  }
}

object Assignment extends App {
  val obj = new Assignment
  val one: Int = 1
  val rotateBy: Int = 2
  val five: Int = 5
  val testList1: List[Int] = Range(one,five).toList
  val testList2: List[Int] = List(one, one, one, one, rotateBy, rotateBy, rotateBy, five, five, five)
  val log = Logger.getLogger(this.getClass)
  log.info("\n1. Rotate A List\n")
  obj.rotate(rotateBy, testList1)
  testList1.foreach((elem: Int) => log.info(s"$elem\t"))
  log.info("\n2. Remove Duplicates\n")
  log.info(s"${obj.removeDuplicates(testList2)}")
  log.info("\n3. Happy Numbers\n")
  for (a <- 1 to 10) {
    if (obj.happyNumbers(a)) {
      log.info(s"\n${a} is Happy")
    }
    else {
      log.info(s"\n${a} is Sad")
    }
  }
}
