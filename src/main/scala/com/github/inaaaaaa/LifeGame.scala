package com.github.inaaaaaa

object Cell {
  import scala.util.Random
  private val rand = new Random();

  def newByRand(): Cell = {
    Cell(rand.nextBoolean)
  }
}

case class Cell(state: Boolean) {
  def format(): String = {
    this match {
      case Cell(true) => "@"
      case Cell(false) => " "
    }
  }
}

class Field(field: Array[Array[Cell]], size: Int, sleepTime: Int) {
  def printField() {
    Console.clear()
    this.field.foreach(row => {
      row.foreach(cell => print(cell.format()))
      println("")
    })
    Thread.sleep(sleepTime)
  }

  def update(): Field = {
    val newField = Array.tabulate(size, size)((_, _) => Cell(false))

    for (i <- 0 until size; j <- 0 until size) {
      var liveNeighbors = 0

      for (k <- -1 to 1; l <- -1 to 1) {
        val y = i + k
        val x = j + l
        val out = y < 0 || this.size <= y || x < 0 || this.size <= x
        val myself = k == 0 && l == 0
        if (!(out || myself)) {
          if (this.field(y)(x) == Cell(true)) {
            liveNeighbors += 1
          }
        }
      }

      if (this.field(i)(j) == Cell(true)) {
        if (2 <= liveNeighbors && liveNeighbors <= 3) {
          newField(i)(j) = Cell(true)
        }
      } else {
        if (liveNeighbors == 3) {
          newField(i)(j) = Cell(true)
        }
      }
    }

    new Field(newField, this.size, this.sleepTime)
  }
}

object Field {
  def newByRand(size: Int, sleepTime: Int): Field = {
    new Field(
      Array.tabulate(size, size)((_, _) => Cell.newByRand()),
      size,
      sleepTime
    )
  }
}

object Console {
  def clear() {
    print("\033[2J")
  }
}

object LifeGame extends App {
  val size = 30
  val times = 100
  val sleepTime = 200

  val res = (1 to times)
    .foldLeft(Field.newByRand(size, sleepTime))((field, _) => {
      field.printField()
      field.update()
    })
}
