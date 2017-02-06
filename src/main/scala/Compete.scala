import Model.{Candidates, Record, Results}

object Compete {

  def executeRound(candidates: Candidates): Results = {
    val results = candidates.map(d => (d, Record.empty))
    val indices = results.length -1
    for {
      i <- 0 to indices
      j <- 0 to indices
    } yield {
      val a = candidates(i)
      val b = candidates(j)
      val (d, rec) = results(i)
      val newRecord = Battle(a, b) match {
        case i if i > 0 =>
          rec.withWin
        case i if i < 0 =>
          rec.withLoss
        case _ => rec.withDraw
      }
      results(i) = (d, newRecord)
    }
    results
  }

}
