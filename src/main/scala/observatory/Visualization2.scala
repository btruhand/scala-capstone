package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
 * 5th milestone: value-added information visualization
 */
object Visualization2 extends Visualization2Interface {
  
  /**
   * @param grid   Grid to visualize
   * @param colors Color scale to use
   * @param tile   Tile coordinates to visualize
   * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
   */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val interpolated = for {
      t <- tile
      loc = Interaction.tileLocation(t)
    } yield {
      val d00 = grid(GridLocation((loc.lat - 1).toInt, (loc.lon - 1).toInt))
      val d01 = grid(GridLocation((loc.lat - 1).toInt, (loc.lon + 1).toInt))
      val d10 = grid(GridLocation((loc.lat + 1).toInt, (loc.lon - 1).toInt))
      val d11 = grid(GridLocation((loc.lat + 1).toInt, (loc.lon + 1).toInt))
      
      val pointX = loc.lon - (loc.lon - 1)
      val pointY = loc.lat - (loc.lat - 1)
      val interpolatedTemp = bilinearInterpolation(CellPoint(pointX, pointY), d00, d01, d10, d11)
      Visualization.interpolateColor(colors, interpolatedTemp)
    }
    
    val pixels = interpolated.toParArray.map(c => Pixel(c.red, c.green, c.blue, 127)).toArray
    Image(256, 256, pixels)
  }
  
  /**
   * @param point (x, y) coordinates of a point in the grid cell
   * @param d00   Top-left value
   * @param d01   Bottom-left value
   * @param d10   Top-right value
   * @param d11   Bottom-right value
   * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
   *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
   */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val first = (1 - point.x) * d00 + point.x * d10 // f(x, y1)
    val second = (1 - point.x) * d01 + point.x * d11 // f(x, y2)
    (1 - point.y) * first + point.y * second
  }
  
}
