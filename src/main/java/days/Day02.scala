package days
import start.Advent

import scala.util.control.Breaks.{break, breakable}

object Day02 extends Day {

  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    var minChar = 0
    var maxChar = 0
    var requiredCharacter = 'a'
    var sum = 0
    for (line <- lines) {
      val splitString = line.split(" ")
      for (part <- splitString) {
        if (part.contains("-")) {
          val charLimits = part.split("-")
          minChar = charLimits(0).toInt
          maxChar = charLimits(1).toInt
        } else if (part.contains(":")) {
          requiredCharacter = part.split(":")(0).toCharArray()(0)
        } else {
          var count = 0
          for (character <- part) {
            if (character == requiredCharacter) {
              count = count + 1
            }
          }
          if (count >= minChar && count <= maxChar) {
            sum = sum + 1
          }
        }
      }
    }
    println(sum)
  }

  def part2(lines: List[String]): Unit = {
    var minChar = 0
    var maxChar = 0
    var requiredCharacter = 'a'
    var sum = 0
    for (line <- lines) {
      val splitString = line.split(" ")
      for (part <- splitString) {
        if (part.contains("-")) {
          val charLimits = part.split("-")
          minChar = charLimits(0).toInt
          maxChar = charLimits(1).toInt
        } else if (part.contains(":")) {
          requiredCharacter = part.split(":")(0).toCharArray()(0)
        } else {
          breakable {
            try {
              if (part(minChar - 1) == requiredCharacter && part(maxChar - 1) == requiredCharacter) {
                break
              } else if (part(minChar - 1) == requiredCharacter || part(maxChar - 1) == requiredCharacter) {
                sum = sum + 1
              }
            } catch {
              case e: StringIndexOutOfBoundsException => println("Oit, I ain't that long mate")
            }
          }
        }
      }
    }
    println(sum)
  }

  def main(args: Array[String]): Unit = {

    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }
}
