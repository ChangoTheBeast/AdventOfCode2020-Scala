package days

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import start.Advent

object Day08 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    var idx = 0
    var acc = 0
    var visitedIndexes = mutable.Set[Int]()
    visitedIndexes.add(idx)
    var looping = false
    while (!looping) {
      val instruction = lines(idx)
      val instructionParts = instruction.split(" ")
      breakable {
        if (instructionParts(0) == "nop") {
          idx += 1
          break
        } else if (instructionParts(0) == "jmp") {
          if (instructionParts(1)(0) == '+') {
            idx += instructionParts(1).substring(1).toInt
          } else {
            idx -= instructionParts(1).substring(1).toInt
          }
          break
        } else {
          if (instructionParts(1)(0) == '+') {
            acc += instructionParts(1).substring(1).toInt
          } else {
            acc -= instructionParts(1).substring(1).toInt
          }
          idx += 1
          break
        }
      }
      if (visitedIndexes.contains(idx)) {
        looping = true;
      } else {
        visitedIndexes.add(idx)
      }
    }
    println(acc)
  }

  def part2(lines: List[String]): Unit = {
    var acc = 0
    var linesCopy = lines
    breakable {
      for (idx <- lines.indices) {
        val instruction = lines(idx).split(" ")
        if (instruction(0) == "nop") {
          linesCopy = linesCopy.updated(idx, linesCopy(idx).replace("nop", "jmp"))
          if (checkLoop(linesCopy)) {
            linesCopy = lines.updated(idx, linesCopy(idx).replace("jmp", "nop"))
          } else {
            acc = run(linesCopy)
            //fixed
            break
          }
        } else if (instruction(0) == "jmp") {
          linesCopy = linesCopy.updated(idx, linesCopy(idx).replace("jmp", "nop"))
          if (checkLoop(linesCopy)) {
            linesCopy = lines.updated(idx, linesCopy(idx).replace("nop", "jmp"))
          } else {
            acc = run(linesCopy)
            break
          }
        }
      }
    }
    println(acc)
  }

  def run(lines: List[String]): Int = {
    var idx = 0
    var acc = 0
    while (idx != lines.size) {
      val instruction = lines(idx)
      val instructionParts = instruction.split(" ")
      breakable {
        if (instructionParts(0) == "nop") {
          idx += 1
          break
        } else if (instructionParts(0) == "jmp") {
          if (instructionParts(1)(0) == '+') {
            idx += instructionParts(1).substring(1).toInt
          } else {
            idx -= instructionParts(1).substring(1).toInt
          }
          break
        } else {
          if (instructionParts(1)(0) == '+') {
            acc += instructionParts(1).substring(1).toInt
          } else {
            acc -= instructionParts(1).substring(1).toInt
          }
          idx += 1
          break
        }
      }
    }
    return acc
  }

  def checkLoop(lines: List[String]): Boolean = {
    var idx = 0
    var acc = 0
    var visitedIndexes = mutable.Set[Int]()
    visitedIndexes.add(idx)
    var looping = false
    while (!looping) {
      val instruction = lines(idx)
      val instructionParts = instruction.split(" ")
      breakable {
        if (instructionParts(0) == "nop") {
          idx += 1
          break
        } else if (instructionParts(0) == "jmp") {
          if (instructionParts(1)(0) == '+') {
            idx += instructionParts(1).substring(1).toInt
          } else {
            idx -= instructionParts(1).substring(1).toInt
          }
          break
        } else {
          if (instructionParts(1)(0) == '+') {
            acc += instructionParts(1).substring(1).toInt
          } else {
            acc -= instructionParts(1).substring(1).toInt
          }
          idx += 1
          break
        }
      }
      if (visitedIndexes.contains(idx)) {
        return true
      } else {
        visitedIndexes.add(idx)
      }
      if (idx == lines.size) {
        return false
      }
    }
    false
  }
}
