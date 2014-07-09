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
      if (ps.cost >= currCost) {
         sender ! PartialCompleted(ps) 
         println ("Skipping work on:" + ps.toString + " value " + ps.cost)
      }
      else {
         println ("Starting work on:" + ps.toString + " value " + ps.cost)
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
          sender ! PartialCompleted (ps)
        }
     }
    case x => throw new Exception("Unknown message: " + x.toString);
  }
}

class BranchAndBound (val startBestSol : CompleteSolution) extends Actor {
   
   var started = false;
   var originator : ActorRef = null;
   var currBestSol = startBestSol;
   var createdActors = 0
   var count = 0
   
   val prob = startBestSol.prob
   private def printPartial (partial: PartialSolution) : Unit = {
      val time = (System.currentTimeMillis - prob.startTime) / 1000
      println ("bnb " + count.toString + " (" + time.toString + "): " + 
         partial.toString + " / " + partial.cost.toString)
      count = count + 1
   }
   
   private def DepthFirstOrder = new Ordering[PartialSolution] {
      def compare(a : PartialSolution, b : PartialSolution) = {
         if (a.numSetPoints != b.numSetPoints) a.numSetPoints - b.numSetPoints 
         else b.cost - a.cost 
      }
   }
   
   var pq = new scala.collection.mutable.PriorityQueue[PartialSolution]()(DepthFirstOrder)
   var waitingActors = List[ActorRef]()
   val maxActors = 30
   
   def receive = {
      case Start() => {
         println ("Recieved start message")
         if (!started) {
            originator = sender
            started = true;
            val startingProbs = new PartialSolution(startBestSol.prob).deeperList.sortWith(_.cost < _.cost)
            // It is conceivable this will throw, but it would take pathological
            // data.
            for (psol <- startingProbs.map(_.asInstanceOf[PartialSolution])) {
               if (psol.cost < currBestSol.cost) {
                  if (createdActors >= maxActors) {
                     pq += psol
                  }
                  else {
                     createdActors = createdActors + 1
                     context.actorOf(Props[BranchExpander]) ! NewBranch (psol, currBestSol.cost)
                  }
               }
            }
            // If we have the best already, just return
            if (createdActors == 0) originator ! Complete (currBestSol)
         }
      }
      case BetterComplete (s) => {
         println ("Received BetterComplete cost " + s.cost)
         println ("*** " + s.toString)
         if (s.cost < currBestSol.cost) {
            currBestSol = s
            pq = pq.filter (_.cost < currBestSol.cost)
         }
      }
      case LivePartial (s) => {
         if (s.cost < currBestSol.cost) {
            printPartial (s) 
            if (waitingActors.isEmpty) pq += s
            else {
               waitingActors.head ! NewBranch (s, currBestSol.cost)
               waitingActors = waitingActors.tail
            }
         }
      }
      case PartialCompleted (_) => {
         //println ("Received PartialCompleted")
         if (pq.isEmpty) {
            waitingActors = sender :: waitingActors;
            println ("Waiting actors now " + waitingActors.length)
            // We check to see if they're all in, but one might have got lost and we'll
            // run forever. We should check if all the Partial Solutions we sent out got
            // returned, and if not resend them out.
            if (waitingActors.length == createdActors) {
               originator ! Complete (currBestSol)
            }            
         }
         else {
            sender ! NewBranch (pq.dequeue, currBestSol.cost)
         }
      }
      case x => throw new Exception ("Unknown message:" + x.toString)
   }
}

}
