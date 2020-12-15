package days

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import start.Advent

object Day11 extends Day {
  val lines: List[String] = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))

  override def run(): Unit = {

    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def part1(): Unit = {
    var buffer = ListBuffer[Array[Char]]()
    for (line <- lines) {
      buffer.addOne(line.toCharArray)
    }
    breakable {
      while (true) {
        var bufferCopy = buffer.clone.map(_.clone)
        for (i <- buffer.indices) {
          for (j <- buffer(i).indices) {
            var seat = buffer(i)(j)
            if (seat == 'L') {
              if (swapSeats(buffer, i, j)) {
                bufferCopy(i)(j) = '#'
              }
            } else if (seat == '#') {
              if (swapSeats(buffer, i, j)) {
                bufferCopy(i)(j) = 'L'
              }
            }
          }
        }
        var equal = true
        breakable {
          for (idx <- buffer.indices) {
            if (!buffer(idx).sameElements(bufferCopy(idx))) {
              equal = false
              break
            }
          }
        }
        if (equal) {
          break
        } else {
          buffer.empty
          buffer = bufferCopy.clone.map(_.clone)
        }
      }
    }
    var count = 0
    for (array <- buffer) {
      for (char <- array) {
        if (char == '#') {
          count += 1
        }
      }
    }
    println(count)
  }

  def part2(): Unit = {

    var iterations = 0

    var buffer = ListBuffer[Array[Char]]()
    for (line <- lines) {
      buffer.addOne(line.toCharArray)
    }
    breakable {
      while (true) {
        iterations += 1
        var bufferCopy = buffer.clone.map(_.clone)
        for (i <- buffer.indices) {
          for (j <- buffer(i).indices) {
            var seat = buffer(i)(j)
            if (seat == 'L') {
              if (checkDirections(buffer, i, j)) {
                bufferCopy(i)(j) = '#'
              }
            } else if (seat == '#') {
              if (checkDirections(buffer, i, j)) {
                bufferCopy(i)(j) = 'L'
              }
            }
          }
        }
        var equal = true
        breakable {
          for (idx <- buffer.indices) {
            if (!buffer(idx).sameElements(bufferCopy(idx))) {
              equal = false
              break
            }
          }
        }
        if (equal) {
          break
        } else {
          buffer.empty
          buffer = bufferCopy.clone.map(_.clone)
        }
      }
    }
    var count = 0
    for (array <- buffer) {
      for (char <- array) {
        if (char == '#') {
          count += 1
        }
      }
    }
    println(count, iterations)
  }

  def checkDirection(buffer: ListBuffer[Array[Char]], i: Int, j: Int, x: Int, y: Int): Int = {
    var posY = i + y
    var posX = j + x
    while (posY >= 0 && posY < buffer.size && posX >= 0 && posX < buffer(posY).length) {
      if (buffer(posY)(posX) == 'L') {
        return 0
      } else if (buffer(posY)(posX) == '#') {
        return 1
      }
      posX += x
      posY += y
    }
    0
  }

  def checkDirections(buffer: ListBuffer[Array[Char]], i: Int, j: Int): Boolean = {
    var count = 0
    count += checkDirection(buffer, i, j, 1, 1)
    count += checkDirection(buffer, i, j, 1, 0)
    count += checkDirection(buffer, i, j, 1, -1)
    count += checkDirection(buffer, i, j, 0, 1)
    count += checkDirection(buffer, i, j, 0, -1)
    count += checkDirection(buffer, i, j, -1, 1)
    count += checkDirection(buffer, i, j, -1, 0)
    count += checkDirection(buffer, i, j, -1, -1)

    if (buffer(i)(j) == 'L') {
      count == 0
    } else {
      count >= 5
    }
  }

  def swapSeats(buffer: ListBuffer[Array[Char]], i: Int, j: Int): Boolean = {
    var count = 0
    if (i - 1 > -1) {
      if (j - 1 > -1) {
        if (buffer(i - 1)(j - 1) == '#') {
          count += 1
        }
        if (buffer(i)(j - 1) == '#') {
          count += 1
        }
      }
      if (buffer(i - 1)(j) == '#') {
        count += 1
      }
      if (j + 1 < buffer(i).length) {
        if (buffer(i - 1)(j + 1) == '#') {
          count += 1
        }
        if (buffer(i)(j + 1) == '#') {
          count += 1
        }
      }
    } else {
      if (j - 1 > -1) {
        if (buffer(i)(j - 1) == '#') {
          count += 1
        }
      }
      if (j + 1 < buffer(i).length) {
        if (buffer(i)(j + 1) == '#') {
          count += 1
        }
      }
    }

    if (i + 1 < buffer.size) {
      if (j - 1 > -1) {
        if (buffer(i + 1)(j - 1) == '#') {
          count += 1
        }
      }
      if (buffer(i + 1)(j) == '#') {
        if (i == 0) {
        }
        count += 1
      }
      if (j + 1 < buffer(i).length) {
        if (buffer(i + 1)(j + 1) == '#') {
          count += 1
        }
      }
    }
    if (buffer(i)(j) == 'L') {
      count == 0
    } else {
      count >= 4
    }
  }


}
