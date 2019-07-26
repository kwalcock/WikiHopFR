package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WikiHopInstance
import org.ml4ai.agents.{AgentObserver, PolicyAgent}
import org.ml4ai.mdp.WikiHopEnvironment
import org.ml4ai.utils.WikiHopParser
import org.sarsamora.actions.Action

object TrainFR extends App with LazyLogging{

  def selectSmall(instances: Seq[WikiHopInstance]) = instances.head // TODO: implement this correctly

  // Load the config
  // Load the data
  val instances = WikiHopParser.trainingInstances

  val episode = 1000
  val policy = None

  val instance = selectSmall(instances)

  val trainingObserver = new AgentObserver {

    override def startedEpisode(env: WikiHopEnvironment): Unit = {}

    override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
      // TODO fill here
      val state = env.observeState
    }

    override def actionTaken(action: Action, reward: Double, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
      // TODO fill here
      val newState = env.observeState

    }


    override def concreteActionTaken(action: Action, reward: Double, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {}

    override def endedEpisode(env: WikiHopEnvironment): Unit = {}

    override def registerError(throwable: Throwable): Unit = {
      logger.error(throwable.getMessage)
    }
  }

  var successes = 0

  for(ep <- 1 to episode){
    logger.info(s"Epoch $ep")
    val agent = new PolicyAgent(policy)
    val outcome = agent.runEpisode(instance, Some(trainingObserver))

    val successful = outcome.nonEmpty
    if(successful)
      successes += 1

    if(ep % 100 == 0){
      val successRate = successes / 100.0
      logger.info(s"Success rate of $successRate for the last 100 episodes")
      successes = 0
    }

    // TODO store observations
    // TODO Update policy
  }

  // TODO: Do dataset split
  // Define the batch size
  // Collect the observations
  // Perform the update
  // Test for convergence
}
