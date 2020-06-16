package observatory

import java.io.File
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.nio.PngWriter
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
  
  val year = 2021
  val fileName = "temperatures_" + year + ".png"
  
  val writer = PngWriter.NoCompression
  val temperatures = locationYearlyAverageRecords(locateTemperatures(year, "/week3.csv", "/" + year + ".csv"))
  val myImage = Visualization.visualize(temperatures, colors)
  myImage.output(new File(fileName))
  
  Interaction.generateTiles(Iterable((year, temperatures)), generateImage _)
  
  def generateImage(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]) = {
    val image = Interaction.tile(data, colors, tile)
    val path = Paths.get("target", "temperatures", year.toString, tile.zoom.toString, s"${tile.x}-${tile.y}.png")
    Files.createDirectories(path.getParent)
    image.output(path.toFile)
  }
  
}
