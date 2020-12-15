package days

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import start.Advent

object Day07 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    //    val lines = Advent.readFile("input")
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def createBagMap(lines: List[String]): mutable.Map[String, List[String]] = {
    var bags = mutable.Map[String, List[String]]()
    for (line <- lines) {
      val rule = line.split(" contain ")
      val contents = rule(1).replace(".", "");
      val bagList = contents.split(", ").toList
      bags(rule(0).trim) = bagList
    }
    bags
  }

  def part1(lines: List[String]): Unit = {
    val bags = createBagMap(lines)
    println(checkCombinations(bags, "shiny gold bags"))
  }

  def part2(lines: List[String]): Unit = {
    val bags = createBagMap(lines)
    println(numberOfBagsContained(bags, "shiny gold bags") - 1)
  }

  def numberOfBagsContained(bags: mutable.Map[String, List[String]], bagToBeChecked: String): Int = {
    val containedBags = bags(bagToBeChecked)
    var numberOfBags = 1
    for (bag <- containedBags) {
      if (bag.trim != "no other bags") {
        val bagNum = bag.split(" ")(0).toInt
        var newBag = bag.replaceAll("^\\d+", "").trim
        if (newBag.takeRight(1) == "g") {
          newBag = newBag.appended('s')
        }
        numberOfBags += (numberOfBagsContained(bags, newBag)) * bagNum
      }
    }
    numberOfBags
  }

  def checkCombinations(bags: mutable.Map[String, List[String]], bagToBeContained: String): Int = {
    var numberOfCombinations = 0
    for (bag <- bags) {
      if (checkBag(bag, bags, bagToBeContained)) {
        numberOfCombinations += 1
      }
    }
    numberOfCombinations
  }

  def checkBag(bag: (String, List[String]), bags: mutable.Map[String, List[String]], bagToBeContained: String): Boolean = {
    for (item <- bag._2) {
      breakable {
        if (item.trim.equals("no other bags")) {
          break
        }
        var container = item.replaceAll("^\\d+", "").trim
        if (container.takeRight(1) == "g") {
          container = container.appended('s')
        }
        if (container.equals(bagToBeContained)) {
          return true
        } else {
          val newBag = (container, bags(container))
          if (checkBag(newBag, bags, bagToBeContained)) {
            return true
          }
        }
      }
    }
    false
  }
}
