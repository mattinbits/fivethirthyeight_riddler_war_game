import Model._

object Battle {

  def computeTower(currentScore: Int, spec: ((Tower, Deployment), (Tower, Deployment))): Int = spec match {
    case ((t1, d1), (t2, d2)) if d1.regiment > d2.regiment =>
      currentScore + t1.score

    case ((t1, d1), (t2, d2)) if d1.regiment < d2.regiment =>
      currentScore - t1.score

    case (score, _) => currentScore
  }

  def apply(armyA: Deployments, armyB: Deployments): Int = {
    armyA.zip(armyB).foldLeft(0)(computeTower)
  }
}
