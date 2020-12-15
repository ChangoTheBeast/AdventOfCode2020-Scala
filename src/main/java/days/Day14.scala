package days

import start.Advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Day14 extends Day {

  val lines: List[String] = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))

  override def run(): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def part1(): Unit = {
    var mem = mutable.Map[Int, Long]()
    var mask = lines.head.split(" = ")(1)
    for (i <- 1 until lines.length) {
      breakable {
        if (lines(i).contains("mask")) {
          mask = lines(i).split(" = ")(1)
          break
        }
        var parts = (lines(i).split(Array[Char]('[', ']', ' ', '=')) to ListBuffer).filter(a => a != "" && a != "mem")
        var binString = parts(1).toInt.toBinaryString
        binString = binString.padded
        var binArray = binString.toCharArray
        for (idx <- mask.indices) {
          if (mask(idx) != 'X') {
            binArray(idx) = mask(idx)
          }
        }
        binString = binArray.mkString
        var intVal = java.lang.Long.parseUnsignedLong(binString, 2)
        mem(parts.head.toInt) = intVal
      }
    }
    var sum = 0L
    for (value <- mem.values) {
      sum += value
    }
    println(sum)
  }

  def part2(): Unit = {
    var mem = mutable.Map[Long, Long]()
    var mask = lines.head.split(" = ")(1)
    for (i <- 1 until lines.length) {
      breakable {
        if (lines(i).contains("mask")) {
          mask = lines(i).split(" = ")(1)
          break
        }
        var parts = (lines(i).split(Array[Char]('[', ']', ' ', '=')) to ListBuffer).filter(a => a != "" && a != "mem")
        var binString = parts.head.toInt.toBinaryString
        binString = binString.padded
        val binStrings = getBinaryStrings(mask, binString, 0)
        for (binaryString <- binStrings) {
          var intVal = java.lang.Long.parseUnsignedLong(binaryString, 2)
          mem(intVal) = parts.last.toLong
        }
      }
    }
    var sum = 0L
    for (value <- mem.values) {
      sum += value
    }
    println(sum)
  }

  def getBinaryStrings(mask: String, binString: String, startingIdx: Int): mutable.Set[String] = {
    var binArray = binString.toCharArray
    var binaryStringList = mutable.Set[String]()
    for (idx <- startingIdx until mask.length) {
      breakable {
        if (mask(idx) == '1') {
          binArray(idx) = mask(idx)
        } else if (mask(idx) == '0') {
          break
        } else {
          binArray(idx) = '0'
          var newBinString = binArray.mkString
          binaryStringList.addAll(getBinaryStrings(mask, newBinString, idx + 1))
          binArray(idx) = '1'
          newBinString = binArray.mkString
          binaryStringList.addAll(getBinaryStrings(mask, newBinString, idx + 1))
        }
      }
    }
    var newBinString = binArray.mkString
    binaryStringList.addOne(newBinString)
    binaryStringList
  }

  implicit class StringPadder(s: String) {
    def padded: String = {
      var newString = s
      while (newString.length < 36) {
        newString = newString.reverse.appended('0').reverse
      }
      newString
    }
  }
}
