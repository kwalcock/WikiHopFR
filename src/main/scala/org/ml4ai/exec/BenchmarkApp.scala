package org.ml4ai.exec

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WHConfig
import org.ml4ai.agents.{BaseAgent, StatsObserver}
import org.ml4ai.agents.baseline.{CascadeAgent, RandomActionAgent}
import org.ml4ai.utils.{BenchmarkStats, StatsDatum, WikiHopParser}

import concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import org.ml4ai.utils.BenchmarkStats.prettyPrintMap

/**
  * Runs an agent over all the training instances of WikiHop and reports results
  */
object BenchmarkApp extends App with LazyLogging{

  // Read the instances
  //val config = ConfigFactory.load()

  val jsonOutputPath = WHConfig.Files.benchmarkOutput

  val instances = WikiHopParser.trainingInstances
  val totalInstances = instances.size
  logger.info(s"About to run FocusedReading on $totalInstances instances")

  // TODO: Make this configurable
  val agent = makeAgentFromConfig
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

      val ret = StatsDatum(instance.id, outcome, observer)
      logger.info(s"Finished ${instance.id}")
      ret
    }

  val outcomes = Future.sequence(runs)

  // Crunch the numbers with the results. This is a side-effect deferred function
  val stats =
    outcomes andThen {
      case Success(os) =>
        val stats = new BenchmarkStats(os)

        logger.info(s"Success rate of ${stats.successRate}. Found a path on ${stats.numSuccesses} out of $totalInstances instances")
        logger.info(s"Iteration distribution: ${prettyPrintMap(stats.iterationNumDistribution)}")
        logger.info(s"Papers distribution: ${prettyPrintMap(stats.papersDistribution)}")
        logger.info(s"Action distribution: ${prettyPrintMap(stats.actionDistribution)}")

        stats.toJson(jsonOutputPath)

      case Failure(exception) =>
        logger.error(exception.toString)
    }

  Await.ready(stats, Duration.Inf)

  def makeAgentFromConfig:BaseAgent = {
    WHConfig.Benchmark.agentType.toLowerCase match {
      case "random" => new RandomActionAgent
      case "cascade" => new CascadeAgent
    }
  }
}
