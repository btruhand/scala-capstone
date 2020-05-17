package observatory

import java.time.LocalDate

import org.hamcrest.Matchers._
import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _
  
  // Implement tests for the methods of the `Extraction` object
  
  @Test
  def test_locateTemperatures_pairingIdentification_shouldPairByBothSTNAndWBAN(): Unit = {
    milestoneTest {
      val result = Extraction.locateTemperatures(2020, "/stations-pairing.csv", "/temperature-pairing.csv")
      val expected1 = (LocalDate.of(2020, 1, 1), Location(37.231, -38.455), 89.01666666666667)
      val expected2 = (LocalDate.of(2020, 3, 1), Location(-32.34, -65.34), 279.01666666666665)
      val expected3 = (LocalDate.of(2020, 2, 3), Location(44.44, 44.44), 54.57222222222222)
      val expectedNot = (LocalDate.of(2020, 2, 2), Location(88.23, 12.34), 12.33333)
      
      assertEquals(3, result.size)
      result.foreach(res => assertThat(res, oneOf(expected1, expected2, expected3)))
      result.foreach(res => assertThat(res, not(equalTo(expectedNot))))
    }
  }
  
  @Test
  def test_locateTemperatures_stationHasNoLocation_shouldIgnoreStation(): Unit = {
    milestoneTest {
      val result = Extraction.locateTemperatures(2020, "/stations-no-location.csv", "/temperature-no-location.csv")
      assertTrue(result.isEmpty)
    }
  }
  
}
