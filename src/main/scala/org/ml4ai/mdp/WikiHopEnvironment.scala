package org.ml4ai.mdp

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WikiHopInstance
import org.ml4ai.inference.{CoocurrenceKnowledgeGraph, KnowledgeGraph}
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class WikiHopEnvironment(start:String, end:String) extends Environment {

  private implicit val loader:AnnotationsLoader = WikiHopEnvironment.annotationsLoader

  // TODO add a factory pattern to change this without recompiling
  type KG = CoocurrenceKnowledgeGraph

  def this(wikiHopKey:String) {
    this(WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }


  // Control values
  val maxIterations = 10

  // State variables
  private var knowledgeGraph:Option[KnowledgeGraph] = None
  private var iterationNum:Int = 0
  private val exploredEntities = new mutable.HashSet[Int]
  private val exploitedEntities = new mutable.HashSet[Int]



  private def exploitationEligible(e: Int) = !(exploitedEntities contains e)
  private def explorationEligible(e: Int) = !(exploredEntities contains e)



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
        case Some(kg) =>
          val currentEntities = kg.entities

          val ret = new mutable.ListBuffer[Action]

          currentEntities foreach {
            e =>

              if(explorationEligible(e))
                ret += Exploration(e)

              if(exploitationEligible(e))
                ret += Exploitation(e, end.##)

              for(i <- currentEntities if i != e && explorationEligible(i)){
                ret += ExplorationDouble(e, i)
              }
          }

          ret.toList
      }

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  /**
    * Computes the scalar reward of taking the current action
    * @param action taken
    * @return
    */
  private def rewardSignal(action: Action):Double = ???

  override def execute(action: Action, persist: Boolean): Double = {
    // Increment the iteration counter
    iterationNum += 1
    // TODO Convert the action into a lucene query
    // TODO Fetch documents from lucene query
    val fetchedDocs = Seq.empty[String]
    // Generate new KG from the documents
    val kg = new KG(fetchedDocs)
    // TODO Compute the reward function
    val reward = rewardSignal(action)

    // Update the knowledge graph
    knowledgeGraph = Some(kg)

    reward
  }

  override def observeState: State = WikiHopState(iterationNum)

  override def finishedEpisode: Boolean = {
    //TODO: Consider quiting if the state didn't change after an action.
    if(iterationNum >= maxIterations)
      true
    else{
      knowledgeGraph match {
        case Some(kg) =>
          val paths = kg.findPath(start, end)
          if(paths.nonEmpty)
            true
          else
            false
        case None => false
      }
    }
  }

}

object WikiHopEnvironment extends LazyLogging {

  private val config = ConfigFactory.load()

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

  lazy val annotationsLoader = new AnnotationsLoader(config.getString("files.annotationsFile"), cache = false)
}
