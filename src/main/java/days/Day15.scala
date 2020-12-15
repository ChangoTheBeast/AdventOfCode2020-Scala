package days

import start.Advent

import scala.collection.mutable


object Day15 extends Day {
  val lines: List[String] = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))

  override def run(): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def main(args: Array[String]): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def part1(): Unit = {
    println(getNumberSpokenAt(2020))
  }

  def part2(): Unit = {
    println(getNumberSpokenAt(30000000))
  }

  def getNumberSpokenAt(n: Int): Int = {
    val startTime = System.nanoTime()
    var count = 0
    var prevNumber = 0
    val numberMap = mutable.Map[Int, Int]()
    val numberSet = mutable.Set[Int]()
    for (number <- lines.head.split(",")) {
      numberMap(number.toInt) = count
      numberSet.add(number.toInt)
      prevNumber = number.toInt
      count += 1
    }
    while (count < n) {
      if (numberSet.contains(prevNumber)) {
        val difference = (count - 1) - numberMap(prevNumber)
        numberMap(prevNumber) = count - 1
        prevNumber = difference
      } else {
        numberMap(prevNumber) = count - 1
        numberSet.add(prevNumber)
        prevNumber = 0
      }
      count += 1
    }
    println("Time taken: " + (System.nanoTime() -startTime).toDouble/1000000000.0 + " seconds")
    prevNumber
  }
}
