package days

import start.Advent
import scala.collection.mutable

object Day06 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def part1(lines: List[String]): Unit = {
    val groups = getGroups(lines)
    var sum = 0
    for (group <- groups) {
      val numAnswered = checkAnswersPart1(group)
      sum += numAnswered
    }
    println(sum)
  }

  def part2(lines: List[String]): Unit = {
    val groups = getGroups(lines)
    var sum = 0
    for (group <- groups) {
      val numAnswered = checkAnswersPart2(group)
      sum += numAnswered
    }
    println(sum)
  }

  def getGroups(lines: List[String]): List[List[String]] = {
    var groups = List[List[String]]()
    val iterator = lines.iterator
    while (iterator.hasNext) {
      var line = iterator.next()
      var group = List[String]()
      var done = false
      while (line != "" && line != "\n" && !done) {
        group = line :: group
        if (iterator.hasNext) {
          line = iterator.next()
        } else {
          done = true
        }
      }
      if (group.nonEmpty) {
        groups = group :: groups
      }
    }
    groups
  }

  def checkAnswersPart1(group: List[String]): Int = {

    var uniqueChars = mutable.Set[Char]()
    for (line <- group) {
      var array = line.toCharArray
      for (char <- array) {
        uniqueChars += char
      }
    }
    return uniqueChars.size
  }

  def checkAnswersPart2(group: List[String]): Int = {
    var allAnswered = mutable.Set[Char]()
    var first = true
    for (person <- group) {
      var personsAnswers = mutable.Set[Char]()
      var answers = person.toCharArray
      for (answer <- answers) {
        personsAnswers += answer
      }
      if (first) {
        allAnswered = personsAnswers
        first = false
      } else {
        allAnswered = allAnswered.intersect(personsAnswers)
      }
    }
    allAnswered.size
  }
}
