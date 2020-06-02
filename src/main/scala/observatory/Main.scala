package observatory

import com.sksamuel.scrimage.nio.PngWriter
import observatory.Extraction._
import observatory.Visualization._

object Main extends App {
  
  override def main(args: Array[String]): Unit = {
    val year = 1975
    val fileName = "temperatures_" + year + ".png"
    val colors = Array((60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0)))
    
    val writer = PngWriter.NoCompression
    val temperatures = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", "/" + year + ".csv"))
    val myImage = visualize(temperatures, colors)
    myImage.output(new java.io.File(fileName))
  }
  
}
