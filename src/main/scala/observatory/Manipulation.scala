package observatory

/**
 * 4th milestone: value-added information
 */
object Manipulation extends ManipulationInterface {
  
  /**
   * @param temperaturess Sequence of known temperatures over the years (each element of the collection
   *                      is a collection of pairs of location and temperature)
   * @return A function that, given a latitude and a longitude, returns the average temperature at this location
   */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    //    val yearlyGrid = for {
    //      yearlyAverage <- temperaturess
    //    } yield makeGrid(yearlyAverage)
    //
    //    val size = temperaturess.size
    //    gridLoc => {
    //      val temperatureAtGridLoc = yearlyGrid.aggregate(0.0)(_ + _ (gridLoc), _ + _)
    //      temperatureAtGridLoc / size
    //    }
    ???
  }
  
  /**
   * @param temperatures Known temperatures
   * @param normals      A grid containing the “normal” temperatures
   * @return A grid containing the deviations compared to the normal temperatures
   */
  def deviation(temperatures: Iterable[(Location, Temperature)],
    normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)
    
    def _inner(gridLoc: GridLocation): Temperature = {
      val normal = normals(gridLoc)
      grid(gridLoc) - normal
    }
    
    _inner
  }
  
  /**
   * @param temperatures Known temperatures
   * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
   *         returns the predicted temperature at this location
   */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    def _inner(gridLoc: GridLocation): Temperature = {
      val loc = Location(gridLoc.lat.toDouble, gridLoc.lon.toDouble)
      Visualization.predictTemperature(temperatures, loc)
    }
    
    memoize(_inner _)
  }
  
  def memoize[I, O](f: I => O): I => O = {
    val map = collection.concurrent.TrieMap[I, O]()
    key => map.getOrElseUpdate(key, f(key))
  }
  
}

