import akka.actor.Actor
import akka.actor.ActorRef
import Solution._
import akka.actor.Props
//import akka.event.Logging

package BranchAndBoundActors {

case class Start ()
case class Complete (s : CompleteSolution)

case class NewBranch (ps : PartialSolution, currCost : Int)
case class BetterComplete (s : CompleteSolution)
case class LivePartial (s : PartialSolution)
case class PartialCompleted (ps : PartialSolution)

class BranchExpander extends Actor {
  def receive = {
    case NewBranch(ps, currCost) => {
      val solList = ps.deeperList
      for (sol <- solList) {
         if (sol.isComplete) {
            if (sol.cost < currCost) {
               sender ! BetterComplete (sol.asInstanceOf[CompleteSolution])
            }
         } 
         else {
            if (sol.cost < currCost) {
               sender ! LivePartial (sol.asInstanceOf[PartialSolution])
            }
         }
       }
     }
    case _ => throw new Exception("Unknown message!");
  }
}

class BranchAndBound (val startBestSol : CompleteSolution) extends Actor {
   
   var started = false;
   var originator : ActorRef = null;
   var currBestSol = startBestSol;
   var createdActors = 0
   
   private def DepthFirstOrder = new Ordering[PartialSolution] {
      def compare(a : PartialSolution, b : PartialSolution): Boolean = { 
         (a.numSetPoints > b.numSetPoints) || 
         (a.numSetPoints == b.numSetPoints && a.cost < b.cost)
      }
   }
   
   val pq = new scala.collection.mutable.PriorityQueue[PartialSolution]()(DepthFirstOrder)
   var waitingActors = List[ActorRef]()
   
   def receive = {
      case Start => {
         if (!started) {
            originator = sender
            started = true;
            val startingProbs = new PartialSolution(startBestSol.prob).deeperList
            createdActors = startingProbs.length
            // I can't gaurnetee this won't throw, but it would take pathological
            // data.
            for (psol <- startingProbs.map(_.asInstanceOf[PartialSolution])) {
               context.actorOf(Props[BranchExpander]) ! NewBranch (psol, currBestSol.cost)
            }
         }
      }
      // case class BetterComplete (s : CompleteSolution);
      // case class LivePartial (s : PartialSolution);
      // case class PartialCompleted (ps : PartialSolution);
      case BetterComplete (s) => if (s.cost < currBestSol.cost) currBestSol = s
      case LivePartial (s) => {
         if (waitingActors.isEmpty) pq += s
         else {
            waitingActors.head ! NewBranch (s, currBestSol.cost)
            waitingActors = waitingActors.tail
         }
      }
      case PartialCompleted (_) => {
         if (pq.isEmpty) {
            waitingActors = sender :: waitingActors;
            // We check to see if they're all in, but one might have got lost and we'll
            // run forever. We should check if all the Partial Solutions we sent out got
            // returned, and if not resend them out.
            if (waitingActors == createdActors) {
               originator ! Complete (currBestSol);
            }            
         }
         else sender ! NewBranch (pq.dequeue, currBestSol.cost)
      }
      case _ => throw new Exception ("Unknown message!")
   }
}

}
