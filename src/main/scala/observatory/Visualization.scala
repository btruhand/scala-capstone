package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.parallel.ParIterable
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {
  
  val p = 2
  val r = 6371
  // (kilometer distance)
  val thresholdDistance = 1.0
  
  val greatCircleDistance: (Location, Location) => Distance = (loc1: Location, loc2: Location) => {
    if (loc1 == loc2) 0
    else if (loc1.isAntipode(loc2)) r * Pi
    else {
      val loc1Lat = toRadians(loc1.lat)
      val loc1Lon = toRadians(loc1.lon)
      val loc2Lat = toRadians(loc2.lat)
      val loc2Lon = toRadians(loc2.lon)
      val a = sin(loc1Lat) * sin(loc2Lat)
      val b = cos(loc1Lat) * cos(loc2Lat) * cos(abs(loc1Lon - loc2Lon))
      val delta = acos(a + b)
      r * delta
    }
  }
  
  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val locations = Iterable.range(0, 360 * 180).map(n => Location(90 - n / 360, n % 360 - 180)).par
    val locationTemperatures = locations.map(predictTemperature(temperatures, _))
    val interpolatedColors: ParIterable[Color] = locationTemperatures.map(interpolateColor(colors, _))
    val interpolatedPixels = interpolatedColors.map(c => Pixel.apply(c.red, c.green, c.blue, 255)).toArray
    Image.apply(360, 180, interpolatedPixels)
  }
  
  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val lowerBound = points.reduce((v1, v2) => {
      if (v1._1 <= value && v2._1 <= value) {
        if (v1._1 > v2._1) v1
        else v2
      }
      else if (v1._1 <= value) v1
      else v2
    })
    
    val upperBound = points.reduce((v1, v2) => {
      if (v1._1 > value && v2._1 > value) {
        if (v1._1 < v2._1) v1
        else v2
      }
      else if (v1._1 > value) v1
      else v2
    })
    
    if (value == lowerBound._1) lowerBound._2
    else if (value == upperBound._1) upperBound._2
    else if (value < lowerBound._1 || value > upperBound._1) upperBound._2
    else {
      val t = (value - lowerBound._1) / (upperBound._1 - lowerBound._1)
      lowerBound._2 * (1 - t) + upperBound._2 * t
    }
  }
  
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distancesAndTemps = temperatures.par.map(v => (greatCircleDistance(v._1, location), v._2))
    val sameLocation = distancesAndTemps.find(_._1 < thresholdDistance)
    if (sameLocation.isDefined) sameLocation.get._2
    else {
      val inverseDistanceWeighting: (Distance, Temperature) = distancesAndTemps.foldLeft((0.0, 0.0))(
        (acc, distanceAndTemp) => {
          //          println("distanceAndTemp", distanceAndTemp)
          val inverseDistance = 1 / pow(distanceAndTemp._1, p)
          (acc._1 + inverseDistance, acc._2 + inverseDistance * distanceAndTemp._2)
        })
      inverseDistanceWeighting._2 / inverseDistanceWeighting._1
    }
  }
  
}

