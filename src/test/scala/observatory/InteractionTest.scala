package observatory

import org.junit.Assert._
import org.junit.Test
import org.scalacheck.Prop.{all, forAll}
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Test => SchkChk}

import scala.math.Numeric.IntIsIntegral

trait InteractionTest extends MilestoneSuite {
  val zeroXZeroY = forAll(Gen.posNum) {
    (zoom: Int) => Location(85.05112877980659, -180) == Interaction.tileLocation(Tile(0, 0, zoom))
  }
  val maxCoordinate = forAll(Gen.choose(1, 10) suchThat (_ > 0)) {
    (zoom: Int) => {
      val limit = (math.pow(2.0, zoom) - 1).toInt
      val tile = Tile(limit, limit, zoom)
      val loc = Interaction.tileLocation(tile)
      all(loc.lat <= 0, loc.lon >= 0) :| ("limit = " + limit + " tile = " + tile + " loc = " + loc)
    }
  }
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _
  
  @Test
  def test_tileLocation_zeroXZeroYAllZoomLevels_shouldTopLeftLocation() {
    milestoneTest {
      assertTrue(SchkChk.check(Parameters.defaultVerbose, zeroXZeroY).passed)
    }
  }
  
  @Test
  def test_tileLocation_maxCoordinateAllZoomLevelsY_shouldBottomRightAreaLocation() {
    milestoneTest {
      assertTrue(SchkChk.check(Parameters.defaultVerbose, maxCoordinate).passed)
    }
  }
  
}
