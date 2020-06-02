package observatory

import java.time.LocalDate

import scala.collection.parallel.ParIterable
import scala.io.{BufferedSource, Source}
import scala.language.reflectiveCalls

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location,
    Temperature)] = {
    val stationsSource = Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8")
    val tempSource = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8")
    val stationLocation = mapStationsLocations(stationsSource)
    val r = for {
      line <- tempSource.getLines()
      splitted = line.split(",")
      location = stationLocation.get(Identifier(splitted(0), splitted(1)))
      month = splitted(2).toInt
      day = splitted(3).toInt
      if location.nonEmpty
    } yield (LocalDate.of(year, month, day), location.get, fahrenheitToCelcius(splitted(4).toDouble))
    r.toIterable
  }
  
  private def mapStationsLocations(stationsSource: BufferedSource): Map[Identifier, Location] = {
    stationsSource.getLines.map(line => line.split(',')).filter(_.length > 3).map(splitted => {
      (Identifier(splitted(0), splitted(1)), Location(splitted(2).toDouble, splitted(3).toDouble))
    }).toMap
  }
  
  private def fahrenheitToCelcius(fahrenheit: Double) = {
    (fahrenheit - 32.0) * 5 / 9
  }
  
  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location,
    Temperature)] = {
    records.par.groupBy(_._2).values.map(groupedRecords => avgAtLocation(groupedRecords)).seq
  }
  
  private def avgAtLocation(records: ParIterable[(LocalDate, Location, Temperature)]) = {
    val summed = records.reduce((a, b) => (a._1, a._2, a._3 + b._3))
    (summed._2, summed._3 / records.size)
  }
  
}
