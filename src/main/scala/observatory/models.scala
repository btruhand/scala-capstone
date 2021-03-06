package observatory

import scala.math.round

/**
 * Introduced in Week 1. Represents a location on the globe.
 *
 * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
 * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
 */
case class Location(lat: Double, lon: Double) {
  lazy val sinLatRad = math.sin(math.toRadians(lat))
  lazy val lonRad = math.toRadians(lon)
  
  def isAntipode(other: Location): Boolean = {
    val antipode = Location(-lat, 180 - math.abs(lon))
    antipode == other
  }
}

/**
 * Introduced in Week 3. Represents a tiled web map tile.
 * See https://en.wikipedia.org/wiki/Tiled_web_map
 * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
 *
 * @param x    X coordinate of the tile
 * @param y    Y coordinate of the tile
 * @param zoom Zoom level, 0 ≤ zoom ≤ 19
 */
case class Tile(x: Int, y: Int, zoom: Int) {
  val N = 1 << zoom
  
  def map[B](f: Tile => B): Seq[B] = {
    val dim = 256
    for {
      b <- 0 until dim
      a <- 0 until dim
      tile = Tile(x * dim + a, y * dim + b, zoom + 8)
    } yield f(tile)
  }
  
}

/**
 * Introduced in Week 4. Represents a point on a grid composed of
 * circles of latitudes and lines of longitude.
 *
 * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
 * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
 */
case class GridLocation(lat: Int, lon: Int)

/**
 * Introduced in Week 5. Represents a point inside of a grid cell.
 *
 * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
 * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
 */
case class CellPoint(x: Double, y: Double)

/**
 * Introduced in Week 2. Represents an RGB color.
 *
 * @param red   Level of red, 0 ≤ red ≤ 255
 * @param green Level of green, 0 ≤ green ≤ 255
 * @param blue  Level of blue, 0 ≤ blue ≤ 255
 */
case class Color(red: Int, green: Int, blue: Int) {
  def +(other: Color): Color = {
    Color(red + other.red, green + other.green, blue + other.blue)
  }
  
  def *(multiplicator: Double): Color = {
    Color(round(red * multiplicator).toInt, round(green * multiplicator).toInt, round(blue * multiplicator).toInt)
  }
}

/**
 * Station identifier
 *
 * @param STN
 * @param WBAN
 */
case class Identifier(STN: String, WBAN: String)

case class InverseDistanceWeight(distance: Distance, temperature: Temperature, tooClose: Boolean) {
  def +(other: InverseDistanceWeight) = {
    if (tooClose) this
    else other match {
      case InverseDistanceWeight(_, _, true) => other
      case _                                 => InverseDistanceWeight(distance + other.distance,
        temperature + other.temperature, false)
    }
  }
  
  def +(other: (Distance, Temperature)) = {
    if (tooClose) this
    else InverseDistanceWeight(distance + other._1, temperature + other._2, false)
  }
  
  def weight = {
    if (tooClose) temperature
    else temperature / distance
  }
}