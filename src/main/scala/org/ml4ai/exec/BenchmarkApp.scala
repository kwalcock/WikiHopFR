package org.ml4ai.exec

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.agents.StatsObserver
import org.ml4ai.agents.baseline.RandomActionAgent
import org.ml4ai.utils.WikiHopParser

import concurrent.{Await, ExecutionContext, Future}
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
  * Runs an agent over all the training instances of WikiHop and reports results
  */
object BenchmarkApp extends App with LazyLogging{

  // Read the instances
  val config = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances//.take(100)
  val totalInstances = instances.size
  logger.info(s"About to run FocusedReading on $totalInstances instances")

  // TODO: Make this configurable
  val agent = new RandomActionAgent
  logger.info(s"Agent: $agent")

  import ExecutionContext.Implicits.global

  // Async programming =)
  val runs =
    for(instance <- instances) yield Future{
      logger.info(s"Starting ${instance.id}")
      val observer = new StatsObserver
      // Return the instance id along with the outcomes
      val outcome = Try(agent.runEpisode(instance, monitor=Some(observer))) match {
        case Success(paths) =>
          paths
        case Failure(exception) =>
          logger.error(s"Error in ${instance.id} - ${exception.toString}: ${exception.getMessage}")
          Iterable.empty
        }

      val ret = (instance.id, outcome, observer)
      logger.info(s"Finished ${instance.id}")
      ret
    }

  val outcomes = Future.sequence(runs)

  // Crunch the numbers with the results. This is a side-effect deferred function
  val stats =
    outcomes andThen {
      case Success(os) =>

        val successes = os count {case (_, paths, _) => paths.nonEmpty}

        val successRate = successes / totalInstances.toDouble

        val (iterationDistribution, paperDistribution, actionDistribution) = crunchNumbers(os.map(_._3))

        logger.info(s"Success rate of $successRate. Found a path on $successes out of $totalInstances instances")
        logger.info(s"Iteration distribution: ${prettyPrintMap(iterationDistribution)}")
        logger.info(s"Papers distribution: ${prettyPrintMap(paperDistribution)}")
        logger.info(s"Action distribution: ${prettyPrintMap(actionDistribution)}")

      case Failure(exception) =>
        logger.error(exception.toString)
    }

  Await.ready(stats, Duration.Inf)

  def crunchNumbers(observers:Iterable[StatsObserver]) = {
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
