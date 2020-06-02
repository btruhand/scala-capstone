package observatory

import org.junit.Assert._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _
  
  // Implement tests for the methods of the `Visualization` object
  
  @Test
  def test_interpolateColor_valueExceededGivenScale_shouldReturnHighestValuesColor(): Unit = {
    milestoneTest {
      val interpolated = Visualization.interpolateColor(
        List((0.0, Color(255, 0, 0)), (1.937284405353246E-5, Color(0, 0, 255))), value = -10.0)
      
      assertEquals(interpolated, Color(255, 0, 0))
    }
  }
  
}
