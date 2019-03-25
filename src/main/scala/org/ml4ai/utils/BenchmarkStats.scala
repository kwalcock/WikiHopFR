package org.ml4ai.utils

import java.io.PrintWriter

import org.ml4ai.agents.StatsObserver
import org.ml4ai.inference.VerboseRelation
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

import scala.collection.mutable

case class StatsDatum(instanceId:String, paths:Iterable[Seq[VerboseRelation]], observer:StatsObserver)

/**
  * Main analysis source file for the results of a benchmark run
  */
class BenchmarkStats(data:Iterable[StatsDatum]) {

  lazy val size: Int = data.size

  private lazy val (successes, failures) = data partition {
    case StatsDatum(_, p, _) =>
      p.nonEmpty
  }

  // Queries
  lazy val numSuccesses: Int = successes.size
  lazy val numFailures: Int = failures.size

  lazy val successRate: Double = numSuccesses.toDouble / size

  private def crunchNumbers(observers:Iterable[StatsObserver]) = {
    val iterationNumDistribution = observers.map(_.iterations.get).groupBy(identity).mapValues(_.size)
    val papersDistribution = observers.map(_.papersRead.get).groupBy(identity).mapValues(_.size)
    val acc = new mutable.HashMap[String, Int].withDefaultValue(0)

    for(curr <- observers.map(_.actionDistribution)){
      acc("EXPLORATION") += curr(StatsObserver.EXPLORATION)
      acc("EXPLORATION_DOUBLE") += curr(StatsObserver.EXPLORATION_DOUBLE)
      acc("EXPLOITATION") += curr(StatsObserver.EXPLOITATION)
      acc("RANDOM") += curr(StatsObserver.RANDOM)
    }

    (iterationNumDistribution, papersDistribution, acc.toMap)
  }

  lazy val (iterationNumDistribution, papersDistribution, actionDistribution) =
    crunchNumbers(successes map (_.observer))

  def toJson(path:String) {

    implicit val formats = Serialization.formats(NoTypeHints)

    using(new PrintWriter(path)){
      pw =>
        pw.print(
          write(
            Map(
              "size" -> size,
              "numSuccesses" -> numSuccesses,
              "numFailures" -> numFailures,
              "stats" -> Map(
                "iterationsDist" -> iterationNumDistribution,
                "papersDist" -> papersDistribution,
                "actionDist" -> actionDistribution
              ),
              "data" -> data.map{
                d =>
                  Map(
                    "id" -> d.instanceId,
                    "success" -> (if(d.paths.nonEmpty) true else false),
                    "paths" -> d.paths,
                    "iterations" -> d.observer.iterations.get,
                    "papersRead" -> d.observer.papersRead.get,
                    "actions" -> d.observer.actionDistribution.toMap
                  )
              }
            )
          )
        )
    }
  }

}

object BenchmarkStats {
  def prettyPrintMap[K](m:Map[K, Int]):String = {
    val buf = new mutable.StringBuilder("\n")
    val entries = m.toSeq.sortBy{case (k, v) => v}.reverse
    entries foreach {
      case (k, v) =>
        buf.append(s"$k: $v\n")
    }
    buf.toString()
  }
}