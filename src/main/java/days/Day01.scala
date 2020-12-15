package days
import start.Advent

object Day01 extends Day {

  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    var numbers = List[Int]()
    for (line <- lines) {
      numbers = line.toInt :: numbers
    }
    for (i <- numbers.indices) {
      for (j <- i + 1 until (numbers.length)) {
        if (numbers(i) + numbers(j) == 2020) {
          println(numbers(i) * numbers(j))
        }
      }
    }
  }

  def part2(lines: List[String]): Unit = {
    var numbers = List[Int]()
    for (line <- lines) {
      numbers = line.toInt :: numbers
    }
    for (i <- numbers.indices) {
      for (j <- i + 1 until (numbers.length)) {
        for (k <- j + 1 until (numbers.length))
          if (numbers(i) + numbers(j) + numbers(k) == 2020) {
            println(numbers(i) * numbers(j) * numbers(k))
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }
}
