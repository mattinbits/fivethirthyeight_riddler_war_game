import Model.Candidates

object Main extends App {

  val generator = DeploymentGenerator.default
  val rounds = for {
    i <- 1 to 1000
  } yield {
    val candidates = (1 to 1000).map(_ => generator.generate).toArray
    val results = Compete.executeRound(candidates).sortBy(_._2.win)(Ordering[Int].reverse)
    val topTen = results.take(10)
    topTen.foreach {
      case (d, r) =>
        d.foreach {
          case (t, c) =>
            print(s"$t = ${c.regiment}  ")
        }
        println()
        println(r)
    }
    topTen
  }

  val finalCandidates = rounds.flatten.map(_._1).toArray
  val results = Compete.executeRound(finalCandidates)

  results.foreach {
    case (d, r) =>
      d.foreach {
        case (t, c) =>
          print(s"$t = ${c.regiment}  ")
      }
      println()
      println(r)
  }
}
