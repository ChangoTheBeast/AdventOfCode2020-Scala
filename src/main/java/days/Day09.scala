package days

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import start.Advent

object Day09 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    val invalid = findInvalid(lines)
    println(invalid)
  }

  def part2(lines: List[String]): Unit = {
    val invalid = findInvalid(lines)
    var sum = 0.toLong
    var arrayOfNumbers = ListBuffer[Long]()
    var startingIdx = 0
    breakable {
      while (startingIdx < lines.length) {
        arrayOfNumbers = ListBuffer[Long]()
        var i = startingIdx
        sum = 0
        while (!(sum > invalid)) {
          sum += lines(i).toLong
          arrayOfNumbers.addOne(lines(i).toLong)
          if (sum == invalid) {
            break
          }
          i += 1
        }
        startingIdx += 1
      }
    }
    arrayOfNumbers = arrayOfNumbers.sorted
    println(arrayOfNumbers.head + arrayOfNumbers.last)
  }

  def findInvalid(lines: List[String]): Long = {
    var previousNumbers = ListBuffer[Long]()
    for (i <- lines.indices) {
      if (i < 25) {
        previousNumbers.addOne(lines(i).toLong)
      } else {
        val currentVal = lines(i).toLong
        breakable {
          for (j <- previousNumbers.indices) {
            for (k <- j + 1 until previousNumbers.length) {
              if ((previousNumbers(j) + previousNumbers(k)) == currentVal) {
                break
              }
            }
          }
          return currentVal
        }
        val idx = i % 25;
        previousNumbers.update(idx, currentVal)
      }
    }
    0
  }
}
