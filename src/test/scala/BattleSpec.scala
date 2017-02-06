import Model.{Deployment, Deployments, Tower}
import org.scalatest.{Matchers, WordSpec}

class BattleSpec extends WordSpec with Matchers {

  "computeTower" should {

    "Increment score when army A is superior" in {
      Battle.computeTower(-10, (Tower(7) -> Deployment(15), Tower(7) -> Deployment(14))) should be (-3)
      Battle.computeTower(0, (Tower(7) -> Deployment(1), Tower(7) -> Deployment(0))) should be (7)
      Battle.computeTower(10, (Tower(8) -> Deployment(20), Tower(8) -> Deployment(1))) should be (18)
    }

    "Decrement score when Army B is superior" in {
      Battle.computeTower(-10, (Tower(7) -> Deployment(15), Tower(7) -> Deployment(16))) should be (-17)
      Battle.computeTower(0, (Tower(7) -> Deployment(0), Tower(7) -> Deployment(1))) should be (-7)
      Battle.computeTower(10, (Tower(8) -> Deployment(1), Tower(8) -> Deployment(20))) should be (2)
    }

    "Leave score unchanged when Armies are equal" in {
      Battle.computeTower(-10, (Tower(7) -> Deployment(15), Tower(7) -> Deployment(15))) should be (-10)
      Battle.computeTower(0, (Tower(7) -> Deployment(0), Tower(7) -> Deployment(0))) should be (0)
      Battle.computeTower(10, (Tower(8) -> Deployment(1), Tower(8) -> Deployment(1))) should be (10)
    }
  }

  "Battle" should {

    "Calculate an overwhelming winning score for armyA" in {
      val armyA = Deployments(
        Deployment(1),
        Deployment(2),
        Deployment(3),
        Deployment(4),
        Deployment(5),
        Deployment(6),
        Deployment(7),
        Deployment(8),
        Deployment(9),
        Deployment(55)
      )

      val armyB = Deployments(
        Deployment(55),
        Deployment(1),
        Deployment(2),
        Deployment(3),
        Deployment(4),
        Deployment(5),
        Deployment(6),
        Deployment(7),
        Deployment(8),
        Deployment(9)
      )

      Battle(armyA, armyB) should be (53)
    }

    "Calculate an close-run winning score for armyA" in {
      val armyA = Deployments(
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(45),
        Deployment(47)
      )

      val armyB = Deployments(
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(47),
        Deployment(45)
      )

      Battle(armyA, armyB) should be (1)
    }

    "Calculate an overwhelming winning score for armyB" in {
      val armyA = Deployments(
        Deployment(18),
        Deployment(2),
        Deployment(4),
        Deployment(6),
        Deployment(8),
        Deployment(10),
        Deployment(12),
        Deployment(14),
        Deployment(16),
        Deployment(10)
      )

      val armyB = Deployments(
        Deployment(2),
        Deployment(4),
        Deployment(6),
        Deployment(8),
        Deployment(10),
        Deployment(12),
        Deployment(14),
        Deployment(16),
        Deployment(18),
        Deployment(10)
      )

      Battle(armyA, armyB) should be (-43)
    }

    "Calculate an close-run winning score for armyB" in {
      val armyA = Deployments(
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(47),
        Deployment(45)
      )

      val armyB = Deployments(
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(1),
        Deployment(45),
        Deployment(47)
      )

      Battle(armyA, armyB) should be (-1)
    }

    "Calculate a homogenous draw" in {
      val armyA = Deployments(
        Deployment(18),
        Deployment(2),
        Deployment(4),
        Deployment(6),
        Deployment(8),
        Deployment(10),
        Deployment(12),
        Deployment(14),
        Deployment(16),
        Deployment(10)
      )

      val armyB = armyA

      Battle(armyA, armyB) should be (0)
    }

    "Calculate a heterogenous draw" in {
      val armyA = Deployments(
        Deployment(18),
        Deployment(2),
        Deployment(4),
        Deployment(6),
        Deployment(8),
        Deployment(10),
        Deployment(12),
        Deployment(14),
        Deployment(16),
        Deployment(10)
      )

      val armyB = Deployments(
        Deployment(19),
        Deployment(3),
        Deployment(5),
        Deployment(6),
        Deployment(6),
        Deployment(10),
        Deployment(13),
        Deployment(12),
        Deployment(16),
        Deployment(10)
      )

      Battle(armyA, armyB) should be (0)
    }

  }
}
