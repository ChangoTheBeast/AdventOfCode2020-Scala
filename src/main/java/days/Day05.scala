package days
import start.Advent

object Day05 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    var largestSeatId = -100
    for (line <- lines) {
      val seatLocation = getLocation(line)
      val seatId = seatLocation(0) * 8 + seatLocation(1)
      if (seatId > largestSeatId) {
        largestSeatId = seatId
      }
    }
    println(largestSeatId)
  }

  def part2(lines: List[String]): Unit = {
    var seatIds = List[Int]()
    for (line <- lines) {
      val seatLocation = getLocation(line)
      val seatId = seatLocation(0) * 8 + seatLocation(1)
      seatIds = seatId :: seatIds
    }
    seatIds = seatIds.sorted
    var mySeatId = 0
    for (i <- seatIds) {
      try {
        if (seatIds(i + 1) - seatIds(i) == 2) {
          mySeatId = seatIds(i) + 1
          println(mySeatId)
        }
      } catch {
        case e: Exception => "shut it"
      }
    }
  }

  def getLocation(code: String): List[Int] = {
    var topRow = 127
    var botRow = 0
    var row = 0
    var topCol = 7
    var botCol = 0
    var column = 0
    for (i <- code.indices) {
      if (code(i) == 'F') {
        topRow = topRow - ((topRow - botRow) / 2) - 1
      } else if (code(i) == 'B') {
        botRow = botRow + ((topRow - botRow) / 2) + 1
      } else if (code(i) == 'L') {
        topCol = topCol - ((topCol - botCol) / 2) - 1
      } else {
        botCol = botCol + ((topCol - botCol) / 2) + 1
      }
    }
    row = botRow
    column = botCol
    return List[Int](row, column)
  }
}
