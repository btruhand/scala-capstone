package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
 * 2nd milestone: basic visualization
 */
object Visualization extends VisualizationInterface {
  
  val p = 2
  val r = 6371
  // (kilometer distance)
  val thresholdDistance = 1.0
  
  /**
   * @param temperatures Known temperatures
   * @param colors       Color scale
   * @return A 360×180 image where each pixel shows the predicted temperature at its location
   */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180
    val locations = Iterable.range(0, width * height).map(n => Location(90 - n / 360, n % 360 - 180))
    visualizeLocTemps(locations, temperatures, colors, width, height, 255)
  }
  
  def visualizeLocTemps(locations: Iterable[Location],
    temperatures: Iterable[(Location, Temperature)],
    colors: Iterable[(Temperature, Color)],
    width: Int, height: Int, alpha: Int) = {
    val locationTemperatures = locations.par.map(predictTemperature(temperatures, _))
    val interpolatedColors = locationTemperatures.map(interpolateColor(colors, _))
    val interpolatedPixels = interpolatedColors.map(c => Pixel(c.red, c.green, c.blue, alpha)).toArray
    Image(width, height, interpolatedPixels)
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
    val inverseDistanceWeighting = temperatures.toParArray.aggregate(InverseDistanceWeight(0, 0, false))(
      (w, v) => {
        val distance = greatCircleDistance(v._1, location)
        if (distance < thresholdDistance) InverseDistanceWeight(0, v._2, true)
        else {
          val inverseDistance = 1 / math.pow(distance, p)
          w + ((inverseDistance, inverseDistance * v._2))
        }
      }, _ + _)
    
    inverseDistanceWeighting.weight
  }
  
  def greatCircleDistance(loc1: Location, loc2: Location): Distance = {
    if (loc1 == loc2) 0
    else if (loc1.isAntipode(loc2)) r * math.Pi
    else {
      val cosLoc1Lat = math.sqrt(1 - loc1.sinLatRad * loc1.sinLatRad)
      val cosLoc2Lat = math.sqrt(1 - loc2.sinLatRad * loc2.sinLatRad)
      val a = loc1.sinLatRad * loc2.sinLatRad
      val b = cosLoc1Lat * cosLoc2Lat * math.cos(math.abs(loc1.lonRad - loc2.lonRad))
      val delta = math.acos(a + b)
      r * delta
    }
  }
  
}

