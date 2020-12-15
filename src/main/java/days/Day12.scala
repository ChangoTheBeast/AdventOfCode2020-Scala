package days

import start.Advent

object Day12 extends Day {
  val lines: List[String] = Advent.readFile(this.getClass.getSimpleName.replace("$", ""))

  override def run(): Unit = {
    println("-----------Part 1-----------")
    part1()
    println("-----------Part 2-----------")
    part2()
  }

  def part1(): Unit = {
    var east = 0
    var north = 0
    var direction = Direction.EAST
    for (line <- lines) {
      val instruction = (line(0), line.substring(1).toInt)
      val results = calculateInstruction(instruction, direction)
      east += results._1
      north += results._2
      direction = results._3
    }
    println(math.abs(east) + math.abs(north))
  }

  def part2(): Unit = {
    var waypoint = (1, 10)
    var east = 0
    var north = 0
    for (line <- lines) {
      val instruction = (line(0), line.substring(1).toInt)
      if (!(instruction._1 equals 'F')) {
        waypoint = calculateInstruction(instruction, waypoint)
      } else {
        north += waypoint._1*instruction._2
        east += waypoint._2*instruction._2
      }
    }
    println(math.abs(east) + math.abs(north))
  }

  def rotate(instruction: (Char, Int), direction: Direction.Value): Direction.Value = {
    var newDirection = direction
    var degreeRotation = instruction._2 % 360
    degreeRotation = degreeRotation / 90
    if (instruction._1 == 'L') {
      if (degreeRotation equals 1) {
        degreeRotation = 3
      } else if (degreeRotation equals 3) {
        degreeRotation = 1
      }
    }
    if (degreeRotation equals 0) {
      newDirection
    } else if (degreeRotation equals 1) {
      direction match {
        case Direction.EAST => newDirection = Direction.SOUTH
          newDirection
        case Direction.SOUTH => newDirection = Direction.WEST
          newDirection
        case Direction.WEST => newDirection = Direction.NORTH
          newDirection
        case Direction.NORTH => newDirection = Direction.EAST
          newDirection
      }
    } else if (degreeRotation equals 2) {
      direction match {
        case Direction.EAST => newDirection = Direction.WEST
          newDirection
        case Direction.SOUTH => newDirection = Direction.NORTH
          newDirection
        case Direction.WEST => newDirection = Direction.EAST
          newDirection
        case Direction.NORTH => newDirection = Direction.SOUTH
          newDirection
      }
    } else {
      direction match {
        case Direction.EAST => newDirection = Direction.NORTH
          newDirection
        case Direction.SOUTH => newDirection = Direction.EAST
          newDirection
        case Direction.WEST => newDirection = Direction.SOUTH
          newDirection
        case Direction.NORTH => newDirection = Direction.WEST
          newDirection
      }
    }
  }

  def rotate(instruction: (Char, Int), waypoint: (Int, Int)): (Int, Int) = {
    var degreeRotation = instruction._2 % 360
    degreeRotation = degreeRotation / 90
    if (instruction._1 == 'L') {
      if (degreeRotation equals 1) {
        degreeRotation = 3
      } else if (degreeRotation equals 3) {
        degreeRotation = 1
      }
    }
    if (degreeRotation equals 0) {
      waypoint
    } else if (degreeRotation equals 1) {
      var newWaypoint = (-waypoint._2, waypoint._1)
      newWaypoint
    } else if (degreeRotation equals 2) {
      var newWaypoint = (-waypoint._1, -waypoint._2)
      newWaypoint
    } else {
      var newWaypoint = (waypoint._2, -waypoint._1)
      newWaypoint
    }
  }

  def forward(instruction: (Char, Int), direction: Direction.Value): (Int, Int) = {
    var north = 0
    var east = 0
    direction match {
      case Direction.NORTH => north = instruction._2
      case Direction.EAST => east = instruction._2
      case Direction.SOUTH => north -= instruction._2
      case Direction.WEST => east -= instruction._2
    }
    (north, east)
  }

  def calculateInstruction(instruction: (Char, Int), direction: Direction.Value): (Int, Int, Direction.Value) = {
    var north = 0
    var east = 0
    var newDirection = direction

    instruction._1 match {
      case 'R' | 'L' => newDirection = rotate(instruction, direction)
      case 'F' => var (n, e) = forward(instruction, direction)
        north = n
        east = e
      case 'N' => north = instruction._2
      case 'S' => north -= instruction._2
      case 'E' => east = instruction._2
      case 'W' => east -= instruction._2
    }
    return (east, north, newDirection)
  }

  def calculateInstruction(instruction: (Char, Int), waypoint: (Int, Int)): (Int, Int) = {
    var north = waypoint._1
    var east = waypoint._2
    var newWaypoint = waypoint

    instruction._1 match {
      case 'R' | 'L' => newWaypoint = rotate(instruction, waypoint)
        north = newWaypoint._1
        east = newWaypoint._2
      case 'N' => north += instruction._2
      case 'S' => north -= instruction._2
      case 'E' => east += instruction._2
      case 'W' => east -= instruction._2
    }
    newWaypoint = (north, east)
    newWaypoint
  }


}

object Direction extends Enumeration {
  type Direction = Value
  val NORTH, EAST, SOUTH, WEST = Value
}