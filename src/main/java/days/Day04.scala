package days

import scala.annotation.switch
import scala.collection.mutable
import start.Advent

object Day04 extends Day {
  override def run(): Unit = {
    val lines = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))
    println("-----------Part 1-----------")
    part1(lines)
    println("-----------Part 2-----------")
    part2(lines)
  }

  def readPassports(lines: List[String]): List[List[String]] = {
    val iterator = lines.iterator
    var passports = List[List[String]]()
    while (iterator.hasNext) {
      var passport = List[String]()
      var line = iterator.next()
      var done = false
      while (line != "" && line != "\n" && !done) {
        passport = line :: passport
        if (iterator.hasNext) {
          line = iterator.next()
        } else {
          done = true
        }
      }
      if (passport.nonEmpty) {
        passports = passport :: passports
      }
    }
    passports
  }

  def part1(lines: List[String]): Unit = {
    val passports = readPassports(lines)
    var valid = 0
    for (passport <- passports) {
      if (passportChecker(passport, first = true)) {
        valid = valid + 1
      }
    }
    println(valid)
  }

  def part2(lines: List[String]): Unit = {
    val passports = readPassports(lines)
    var valid = 0
    for (passport <- passports) {
      if (passportChecker(passport, first = false)) {
        valid = valid + 1
      }
    }
    println(valid)
  }

  def checkYear(str: String, start: Int, end: Int): Boolean = {
    if (str.length == 4) {
      val year = str.toInt
      if (year <= end && year >= start) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def checkHeight(str: String): Boolean = {
    var height = 0
    if (str.contains("cm")) {
      height = str.split("cm")(0).toInt
      if (height <= 193 && height >= 150) {
        return true
      } else {
        return false
      }
    } else if (str.contains("in")) {
      height = str.split("in")(0).toInt
      if (height <= 76 && height >= 59) {
        return true
      } else {
        return false
      }
    }
    false
  }

  def checkHairColour(str: String): Boolean = {
    if (str.matches("#([a-f0-9]){6,}")) {
      true
    } else {
      false
    }
  }


  def checkEyeColour(str: String): Boolean = {
    (str: @switch) match {
      case "amb" => return true
      case "blu" => return true
      case "brn" => return true
      case "gry" => return true
      case "grn" => return true
      case "hzl" => return true
      case "oth" => return true
      case _ => return false
    }
    false
  }

  def checkPassportID(str: String): Boolean = {
    if (str.length == 9) {
      try {
        str.toInt
        return true
      } catch {
        case _: NumberFormatException => return false
      }
    }
    false
  }

  def checkPart(field: Array[String]): Boolean = {
    (field(0): @switch) match {
      case "byr" => return checkYear(field(1), 1920, 2002)
      case "iyr" => return checkYear(field(1), 2010, 2020)
      case "eyr" => return checkYear(field(1), 2020, 2030)
      case "hgt" => return checkHeight(field(1))
      case "hcl" => return checkHairColour(field(1))
      case "ecl" => return checkEyeColour(field(1))
      case "pid" => return checkPassportID(field(1))
      case "cid" => return true
    }
    false
  }

  def passportChecker(passport: List[String], first: Boolean): Boolean = {
    val flags = mutable.Map[String, Boolean]()
    flags("byr") = false
    flags("iyr") = false
    flags("eyr") = false
    flags("hgt") = false
    flags("hcl") = false
    flags("ecl") = false
    flags("pid") = false
    flags("cid") = false
    for (line <- passport) {
      val parts = line.split("[ \n]")
      for (part <- parts) {
        val flag = part.split(":")
        if (first) {
          flags(flag(0)) = true
        } else {
          flags(flag(0)) = checkPart(flag)
        }
      }
    }
    for (key <- flags.keySet) {
      if (!flags(key)) {
        if (key != "cid") {
          return false
        }
      }
    }
    true
  }
}
