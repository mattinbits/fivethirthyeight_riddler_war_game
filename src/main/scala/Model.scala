object Model {

  case class Tower(score: Int) {

    //The spec shows towers as 1 to 10, but
    //arrays are indexed 0 to 9
    val index = score - 1
  }

  type Towers = Array[Tower]

  object Towers {

    def apply(): Array[Tower] =  (1 to 10).map(Tower.apply).toArray
  }

  case class Deployment(regiment: Int) {

    def withAdditional(troops: Deployment) = Deployment(regiment + troops.regiment)
  }

  type Deployments = Array[(Tower, Deployment)]

  object Deployments {

    def apply(deps: Deployment*): Deployments = {
      require(deps.map(_.regiment).sum == 100)
      Towers().zip(deps)
    }

    def empty : Deployments = {
      Towers().map(_ -> Deployment(0))
    }
  }

  type Candidates = Array[Deployments]

  case class Record(win: Int, draw: Int, loss: Int) {

    def withWin = this.copy(win = win + 1)
    def withDraw = this.copy(draw = draw + 1)
    def withLoss = this.copy(loss = loss + 1)
  }

  object Record {

    def empty = Record(0, 0, 0)
  }

  type Results = Array[(Deployments, Record)]
}




