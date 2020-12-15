package days
import start.Advent

import scala.util.control.Breaks.{break, breakable}

object Day03 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    println(checkSlopes(lines, 3, 1))
  }

  def checkSlopes(lines: List[String], right: Int, down: Double): Long = {
    var idx = 0
    var treeCount = 0
    var firstLine = true
    for (i <- lines.indices) {
      breakable {
        if ((i + 2) % down == 1 || firstLine) {
          firstLine = false
          break
        } else {
          val line = lines(i)
          idx = idx + right
          if (idx >= line.length) {
            idx = idx - line.length
          }
          if (line(idx) == '#') {
            treeCount = treeCount + 1
          }
        }
      }
    }
    return treeCount
  }

  def part2(lines: List[String]): Unit = {
    val right1down1 = checkSlopes(lines, 1, 1.0)
    val right3down1 = checkSlopes(lines, 3, 1.0)
    val right5down1 = checkSlopes(lines, 5, 1.0)
    val right7down1 = checkSlopes(lines, 7, 1.0)
    val right1down2 = checkSlopes(lines, 1, 2.0)

    val answer = right1down1 * right1down2 * right3down1 * right5down1 * right7down1
    println(answer)
  }
}
