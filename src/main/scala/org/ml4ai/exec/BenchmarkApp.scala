package org.ml4ai.exec

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.agents.baseline.RandomActionAgent
import org.ml4ai.utils.WikiHopParser

import concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
  * Runs an agent over all the training instances of WikiHop and reports results
  */
object BenchmarkApp extends App with LazyLogging{

  // Read the instances
  val config = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances.take(1000)
  logger.info(s"About to run FocusedReading on ${instances.size} instances")

  // TODO: Make this configurable
  val agent = new RandomActionAgent
  logger.info(s"Agent: $agent")

  import ExecutionContext.Implicits.global

  // Async programming =)
  val runs =
    for(instance <- instances) yield Future{
      logger.info(s"Starting ${instance.id}")
      // TODO implement an observer pattern to keep track of metrics in an uncluttered fashion
      // Return the instance id along with the outcomes
      val outcome = Try(agent.runEpisode(instance)) match {
        case Success(paths) =>
          paths
        case Failure(exception) =>
          logger.error(s"Error in ${instance.id} - ${exception.toString}: ${exception.getMessage}")
          Iterable.empty
        }

      val ret = (instance.id, outcome)
      logger.info(s"Finished ${instance.id}")
      ret
    }

  val outcomes = Future.sequence(runs)
  Await.result(outcomes, Duration.Inf)

  // Crunch the numbers with the results
//  val numPaths =
//    outcomes.value.get.collect(o => o)
}
