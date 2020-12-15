package days

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import start.Advent

object Day10 extends Day {
  var checked: mutable.Set[ListBuffer[Int]] = mutable.Set[ListBuffer[Int]]()
  var iterations: Long = 0.toLong
  var startTime: Long = System.nanoTime()

  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    var numbers = ListBuffer[Int]()
    numbers.addOne(0)
    for (line <- lines) {
      numbers.addOne(line.toInt)
    }
    numbers = numbers.sorted
    numbers.addOne(numbers.last + 3)
    var oneDifferences = 0
    var threeDifferences = 0

    for (idx <- numbers.indices) {
      if (idx < numbers.length - 1) {
        if (numbers(idx + 1) - numbers(idx) == 1) {
          oneDifferences += 1
        } else if (numbers(idx + 1) - numbers(idx) == 3) {
          threeDifferences += 1
        }
      }
    }
    println(oneDifferences * threeDifferences)
  }

  def part2(lines: List[String]): Unit = {
    var numbers = ListBuffer[Int]()
    numbers.addOne(0)
    for (line <- lines) {
      numbers.addOne(line.toInt)
    }
    numbers = numbers.sorted
    numbers.addOne(numbers.last + 3)
    numbers = numbers.distinct
    var combinations = checkCombinations(numbers, 0)
    println(combinations)
  }

  def checkCombinations(numbers: ListBuffer[Int], startingIdx: Int): Long = {
    var numberOfCombinations = 1.toLong
    if (startingIdx < numbers.length) {
      var idx = startingIdx + 1
      while (idx < numbers.length - 1) {
        breakable {
          if (numbers(idx) != numbers.last) {
            if (numbers(idx) - numbers(idx - 1) == 3) {
              idx += 1
              break
            } else if (numbers(idx) - numbers(idx - 1) == 1) {
              var count = 1
              while (numbers(idx) - numbers(idx - 1) == 1) {
                count += 1
                idx += 1
              }
              numberOfCombinations *= combinations(count)
            }
          }
        }
      }
    } else {
    }
    numberOfCombinations
  }

  def combinations(n: Long): Long = {
    if (n == 1) {
      return 1
    } else if (n == 2) {
      return 1
    } else if (n == 3) {
      return 2
    } else {
      return combinations(n - 1) + combinations(n - 2) + combinations(n - 3)
    }
  }
}
