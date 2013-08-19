package net.ikenna

import org.scalatest.FunSuite
import scala.io.Source
import scala.util.Sorting
import scala.collection.SortedSet

class DataMungingTest extends FunSuite {

  test("Read file into memory") {
    val lines: Array[String] = new Parser("weather.dat").read()
    assert(lines(4) === " MMU June 2002")
  }

  test("Find start and end of days") {
    val lines: Array[String] = new Parser("weather.dat").read()
    assert(lines(8) === "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5")
    assert(lines(37) === "  30  90    45    68          63.6       0.00 H       240  6.0 220  17  4.8 200 41 1022.7")
  }

  test("Create daily weather data from file - read day 1") {
    val days = new Parser("weather.dat").readDays()
    assert(days.get(1).maxTemperature === 88)
  }

  test("Create daily weather data from file - read day 30") {
    val days  = new Parser("weather.dat").readDays()
    assert(days.get(30).maxTemperature === 90)
  }

  test("temperature data for day 1") {
    val aDay:Day =  Day("   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5")
    assert(aDay.maxTemperature === 88)
    assert(aDay.minTemperature === 59)
    assert(aDay.spread === 29)
  }

  test("temperature data for day 30") {
    val aDay =  Day("  30  90    45    68          63.6       0.00 H       240  6.0 220  17  4.8 200 41 1022.7")
    assert(aDay.maxTemperature === 90)
    assert(aDay.minTemperature === 45)
    assert(aDay.spread === 45)
  }

  test("Find least spread") {
    val days = new Parser("weather.dat").readDays()
    assert( 14 === days.getSmallestSpread().date)

  }



}


class Parser(fileName: String) {
  def read(): Array[String] = Source.fromFile(fileName).getLines().toArray
  def readDays(): Days ={
    val lines = read()
    val dayLines = for (i <- 8 to 37) yield {lines(i)}
    new Days(dayLines.map(Day(_)))
  }
}
 class Day(val date:Int, val maxTemperature:Double, val minTemperature:Double){
  def spread : Double = maxTemperature - minTemperature
  override def toString : String = "Day(%s , %s, %s, %s)".format(date, maxTemperature, minTemperature, spread)
}

object Day {
   def apply(lineData:String):Day = {
     assert(lineData != null && !lineData.isEmpty)
     val dailyData = lineData.split(" ").filter(!_.isEmpty).map(_.replace("*", ""))
     new Day(dailyData(0).toInt, dailyData(1).toDouble, dailyData(2).toDouble)
  }
}

class Days(days:Seq[Day]){
  def get(index:Int) = days(index - 1)

  def getSmallestSpread() = {
    days.toList.sortWith((d1, d2) => d1.spread > d2.spread).last
  }
}

class SpreadOrdering extends Ordering[Day] {
  def compare(x: Day, y: Day): Int = {println(" " + x.date  + " " + y.date); x.spread.compare(y.spread)}

}