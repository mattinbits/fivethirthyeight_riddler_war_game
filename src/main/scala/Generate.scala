import Model._

import scala.annotation.tailrec
import scala.util.Random

trait RandomSource {

  /**
    *
    * @return A random choice of Tower (between 1 and 10)
    */
  def pickATower: Tower

  /**
    *
    * @param remainingTroops how many troops the army has remaining
    * @return a deployment between 0 and remainingTroops
    */
  def assignTroops(remainingTroops: Int): Deployment
}

object PseudoRandomSource extends RandomSource {

  def pickATower: Tower = {
    Tower(Random.nextInt(10) + 1)
  }

  def assignTroops(remainingTroops: Int): Deployment = {
    Deployment(Random.nextInt(remainingTroops + 1))
  }
}

class DeploymentGenerator(randomSource: RandomSource) {

  def generate: Deployments = {

    @tailrec
    def generateInner(currentDeployments: Deployments, remainingTroops: Int): Deployments = {
      remainingTroops match {
        case 0 =>
          currentDeployments
        case r =>
          val tower = randomSource.pickATower
          val additionalTroops = randomSource.assignTroops(remainingTroops)
          val (_, dep) = currentDeployments(tower.index)
          currentDeployments(tower.index) = (tower, dep.withAdditional(additionalTroops))
          generateInner(currentDeployments, remainingTroops - additionalTroops.regiment)
      }
    }

    generateInner(Deployments.empty, 100)
  }
}

object DeploymentGenerator {

  def default = new DeploymentGenerator(PseudoRandomSource)
}
