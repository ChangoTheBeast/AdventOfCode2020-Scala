package days

import start.Advent

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Day13 extends Day {

  val lines: List[String] = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))

  def main(args: Array[String]): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }


  override def run(): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def part1(): Unit = {
    var time = 0L
    time = lines.head.toLong
    var listOfBuses = lines(1).split(",") to ListBuffer
    while (listOfBuses.contains("x")) {
      listOfBuses = listOfBuses.subtractOne("x")
    }
    var smallestWait = 10000000000L
    var smallestWithId = (0, 0L)
    for (bus <- listOfBuses) {
      val wait = bus.toInt - time % bus.toInt
      if (wait < smallestWait) {
        smallestWait = wait
        smallestWithId = (bus.toInt, wait)
      }
    }
    println(smallestWithId)
  }

  def part2(): Unit = {
    var schedule = lines(1).split(",")
    var numbers = ListBuffer[Int]()
    var remainders = ListBuffer[Int]()
    for (idx <- schedule.indices) {
      if (schedule(idx) != "x") {
        numbers.addOne(schedule(idx).toInt)
        if (idx != 0) {
          remainders.addOne(schedule(idx).toInt - idx)
        } else {
          remainders.addOne(0)
        }

      }
    }
    var t = chineseRemainder(numbers.toList, remainders.toList)
    println(t)
  }

  def chineseRemainder(numbers: List[Int], remainders: List[Int]): Long = {
    var product = 1L
    var partials = ListBuffer[Long]()
    var inverses = ListBuffer[Long]()
    for (idx <- numbers.indices) {
      product*=numbers(idx)
    }
    var sum = 0L
    for (idx <- numbers.indices) {
      partials.addOne(product/numbers(idx))
      inverses.addOne(computeInverse(partials(idx),numbers(idx)))
      sum += partials(idx) * inverses(idx) * remainders(idx)
    }
    return sum % product
  }

  def computeInverse(a: Long, m: Long): Long = {
    var newA = a
    var newM = m
    var m0 = m;
    var y = 0L
    var x = 1L;

    if (m == 1)
      return 0;

    while (newA > 1) {
      // q is quotient
      var q = newA / newM

      var t = newM

      // m is remainder now, process
      // same as Euclid's algo
      newM = newA % newM;
      newA = t;
      t = y;

      // Update x and y
      y = x - q * y;
      x = t;
    }

    // Make x positive
    if (x < 0)
      x += m0;

    return x;
  }
}
