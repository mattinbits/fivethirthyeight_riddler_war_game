import GenerateSpec.TestRandomSource
import Model.{Deployment, Tower}
import org.scalatest.{Matchers, WordSpec}

object GenerateSpec {

  class TestRandomSource(towers: List[Int], troopDeps: List[Int]) extends RandomSource {

    var towers_ = towers
    var troopDeps_ = troopDeps

    override def pickATower: Tower = {
      val tower = towers_.head
      towers_ = towers_.tail
      Tower(tower)
    }

    override def assignTroops(remainingTroops: Int): Deployment = {
      val dep = troopDeps_.head
      troopDeps_ = troopDeps_.tail
      Deployment(dep)
    }
  }
}

class GenerateSpec extends WordSpec with Matchers {

  "RandomSource" should {

    "Always generate a valid tower index" in {

      val towers = (1 to 100).map(_ => PseudoRandomSource.pickATower)

      towers.foreach { t =>
        t.score should (be >= 1 and be <=10)
      }
    }

    "Always generate a valid troop number" in {
      var remaining = 100
      while(remaining > 0) {
        val dep = PseudoRandomSource.assignTroops(remaining)
        dep.regiment should be <= remaining
        remaining = remaining - dep.regiment
      }
      remaining should be (0)
    }
  }

  "DeploymentGenerator" should {

    "Generate a deployment with one assignment to each tower" in {
      val generator = new DeploymentGenerator(
        new TestRandomSource(
          List(
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10
          ),
          List(
            10, 11, 9, 8, 12, 7, 13, 6, 14, 10
          )
      ))
      val deps = generator.generate
      deps should be(Array(
        Tower(1) -> Deployment(10),
        Tower(2) -> Deployment(11),
        Tower(3) -> Deployment(9),
        Tower(4) -> Deployment(8),
        Tower(5) -> Deployment(12),
        Tower(6) -> Deployment(7),
        Tower(7) -> Deployment(13),
        Tower(8) -> Deployment(6),
        Tower(9) -> Deployment(14),
        Tower(10) -> Deployment(10)
      ))
    }

    "Generate a deployment with multiple assignments to each tower" in {
      val generator = new DeploymentGenerator(
        new TestRandomSource(
          ((1 to 10) ++ (1 to 10).reverse).toList,
          List(
            5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
            4, 6, 4, 6, 4, 6, 4, 6, 4, 6
          )
        ))
      val deps = generator.generate
      deps should be(Array(
        Tower(1) -> Deployment(11),
        Tower(2) -> Deployment(9),
        Tower(3) -> Deployment(11),
        Tower(4) -> Deployment(9),
        Tower(5) -> Deployment(11),
        Tower(6) -> Deployment(9),
        Tower(7) -> Deployment(11),
        Tower(8) -> Deployment(9),
        Tower(9) -> Deployment(11),
        Tower(10) -> Deployment(9)
      ))
    }
  }
}
