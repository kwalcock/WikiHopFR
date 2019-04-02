package org.ml4ai.mdp

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.inference._
import org.ml4ai.ir.LuceneHelper
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State
import org.ml4ai.utils.rng

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import WikiHopEnvironment.buildKnowledgeGraph

class WikiHopEnvironment(start:String, end:String, documentUniverse:Option[Set[String]] = None) extends Environment with LazyLogging {

  private implicit val loader:AnnotationsLoader = WikiHopEnvironment.annotationsLoader


  def this(wikiHopKey:String) {
    this(WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }


  // Control values
  val maxIterations = 10

  // State variables
  private var knowledgeGraph:Option[KnowledgeGraph] = None
  private var iterationNum:Int = 0
  private val exploredEntities = new mutable.HashSet[Set[String]]
  private val exploitedEntities = new mutable.HashSet[Set[String]]
  private val papersRead = new mutable.HashSet[String]



  private def exploitationEligible(e: Set[String]) = !(exploitedEntities contains e)
  private def explorationEligible(e: Set[String]) = !(exploredEntities contains e)



  override def possibleActions: Seq[Action] = {
    // Generate the possible actions to be taken given the current state of the environment
    val actions =
      knowledgeGraph match {
        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
        case None =>
          List(
            Exploration(start.split(" ").toSet),
            Exploration(end.split(" ").toSet),
            ExplorationDouble(start.split(" ").toSet, end.split(" ").toSet),
            Exploitation(start.split(" ").toSet, end.split(" ").toSet),
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
                ret += Exploitation(e, end.split(" ").toSet)

              for(i <- currentEntities if i != e && explorationEligible(i)){
                ret += ExplorationDouble(e, i)
              }
          }

          ret.toList
      }


    if(actions.isEmpty)
      throw new ActionStarvationException

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  /**
    * Computes the scalar reward of taking the current action
    * @param action taken
    * @return
    */
  private def rewardSignal(action: Action, newState:KnowledgeGraph, fetchedPapers:Set[String]):Double = {
    // TODO make the reward function more nuanced
    val newPapers:Int = (fetchedPapers diff papersRead).size
    val newRelations:Int =
      (newState.edges diff
        (knowledgeGraph match {
          case Some(kg) => kg.edges
          case None => Set.empty
        })
      ).size

    // TODO: make this parametrizable
    val livingReward = 0.5

    val informationRatio = if(newPapers > 0) newRelations / newPapers else 0

    val standardReward = informationRatio - livingReward

    val outcomeReward: Double =
      if(finishedEpisode) {
        if(outcome.nonEmpty)
          WHConfig.Environment.successReward
        else
          WHConfig.Environment.failureReward
      }
      else
        0

    standardReward + outcomeReward
  }

  override def execute(action: Action, persist: Boolean): Double = {
    // Increment the iteration counter
    iterationNum += 1
    // If the random action is selected, transform it to a concrete action randomly
    val finalAction = action match {
      case RandomAction => selectActionRandomly
      case a:Action => a
    }
    val fetchedDocs = LuceneHelper.retrieveDocumentNames(finalAction, instanceToFilter = documentUniverse)
    // Generate new KG from the documents
    val kg = buildKnowledgeGraph(fetchedDocs union papersRead)
    val reward = rewardSignal(action, kg, fetchedDocs)

    // Update the knowledge graph and keep track of the new papers
    knowledgeGraph = Some(kg)
    papersRead ++= fetchedDocs

    reward
  }

  private def selectActionRandomly:Action = {
    // Select an index randomly
    // Subtract one because we won't consider the element corresponding to the random action
    val ix = rng.nextInt(possibleActions.size - 1)
    // The first element is the random action, therefore we will operate on the tail
    possibleActions.tail(ix)
  }


  override def observeState: State = WikiHopState(iterationNum)

  override def finishedEpisode: Boolean = {
    if(iterationNum >= maxIterations)
      true
      else {
        Try(possibleActions) match {
          case Failure(s:ActionStarvationException) =>
            logger.debug("Action Starvation")
            true
          case _ =>
            knowledgeGraph match {
              case Some(_) =>
                val paths = outcome
                if (paths.nonEmpty)
                  true
                else
                  false
              case None => false
            }
        }
      }
  }

  def outcome:Iterable[Seq[VerboseRelation]] = knowledgeGraph match {
    case Some(kg) =>
      Try(kg.findPath(start, end)) match {
        case Success(v) => v
        case Failure(e) =>
          logger.error(s"$e - ${e.getMessage}")
          Seq.empty
      }
    case None =>
      Seq.empty
  }

  def iterations:Int = iterationNum
  def consultedPapers:Set[String] = papersRead.toSet

  def entityDegrees: Map[Set[String], Int] = knowledgeGraph match {
    case Some(kg) => kg.degrees
    case None => Map.empty
  }

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

  lazy val annotationsLoader = new AnnotationsLoader(WHConfig.Files.annotationsFile, cache = false)

  def buildKnowledgeGraph(docs:Iterable[String])(implicit loader:AnnotationsLoader):KnowledgeGraph = WHConfig.Environment.knowledgeGraphType match {

    case "Coocurrence" => new CoocurrenceKnowledgeGraph(docs)
    case "OpenIE" => new OpenIEKnowledgeGraph(docs)
    case "NamedEntityLink" => new NamedEntityLinkKnowledgeGraph(docs)
    case t =>
      throw new UnsupportedOperationException(s"Type $t is not a recognized KnowledgeGraph implementation")

  }
}
