package org.ml4ai.mdp

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WikiHopInstance
import org.ml4ai.inference.KnowledgeGraph
import org.ml4ai.utils.WikiHopParser
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State

import scala.util.{Failure, Success, Try}

class WikiHopEnvironment(start:String, end:String) extends Environment {

  def this(wikiHopKey:String) {
    this(WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }


  private var knowledgeGraph:Option[KnowledgeGraph] = None


  override def possibleActions: Seq[Action] = {
    // Generate the possible actions to be taken given the current state of the environment
    val actions =
      knowledgeGraph match {
        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
        case None =>
          List(
            // TODO: Figure out how to deal with the hashes correctly
            Exploration(start.##),
            Exploration(end.##),
            ExplorationDouble(start.##, end.##),
            Exploitation(start.##, end.##),
          )
        // Otherwise, procedurally generate the list of actions
        case Some(kg) => ???
      }

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  override def execute(action: Action, persist: Boolean): Double = {
    // Convert the action into a lucene query
    // Fetch documents from lucene query
    // Generate new KG from the documents
    // Compute the reward function

    throw new NotImplementedError
  }

  override def observeState: State = ???

  override def finishedEpisode: Boolean = ???

}

object WikiHopEnvironment extends LazyLogging {

  private def getInstance(data:Iterable[WikiHopInstance], key:String):WikiHopInstance =
    Try(data.filter(_.id == key)) match {
      case Success(values) if values.size == 1 => values.head
      case Success(_) =>
        val msg = "More than one instance with tha same key"
        logger.error(msg)
        throw new UnsupportedOperationException(msg)
      case Failure(exception) =>
        logger.error(exception.getMessage)
        throw exception

    }

  def getTrainingInstance(key:String):WikiHopInstance = getInstance(WikiHopParser.trainingInstances, key)

  def getTestingInstance(key:String):WikiHopInstance = getInstance(WikiHopParser.testingInstances, key)
}
