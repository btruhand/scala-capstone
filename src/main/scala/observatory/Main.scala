package observatory

import java.nio.file.{Files, Paths}

import observatory.Extraction._

object Main extends App {
  
  val colors = Array((60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))
  
  val year = 1980
  val fileName = "temperatures_" + year + ".png"
  
  //  val writer = PngWriter.NoCompression
  val temperaturess = for {
    year <- Seq(1975, 1976)
  } yield locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", "/" + year + ".csv"))
  //  val myImage = Visualization.visualize(temperatures, colors)
  //  myImage.output(new File(fileName))
  
  //  Interaction.generateTiles(Iterable((year, temperatures)), generateImage _)
  //  Manipulation.makeGrid(temperatures)
  //  Manipulation.average(temperaturess)(GridLocation(57, 66))
  
  //  (1 to 100000000).par.aggregate(0.0)((a, b) => {
  //    println("Test", "Current thread", Thread.currentThread().getId)
  //    a + b
  //  })
  val avg = Manipulation.average(temperaturess)
  
  time {avgGrids(avg)}
  time {avgGrids(avg)}
  
  def avgGrids(avg: GridLocation => Temperature) = {
    for {
      lat <- -89 to 90
      lon <- -180 to 179
    } avg(GridLocation(lat, lon))
  }
  
  def generateImage(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]) = {
    val image = Interaction.tile(data, colors, tile)
    val path = Paths.get("target", "temperatures", year.toString, tile.zoom.toString, s"${tile.x}-${tile.y}.png")
    Files.createDirectories(path.getParent)
    image.output(path.toFile)
  }
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
  
}
