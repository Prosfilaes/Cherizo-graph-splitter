import Solution._

object Cherizo {
   def main(args: Array[String]) : Unit = {
      if (args.size !=  3) {
         println ("This program takes the filename, page size and output basename as mandatory arguments.")
         return
      }

      val dot = dotParser.dotParse.dotFileToGraph (args(0))
      if (dot.isLeft) {
         println (dot.left.get);
         return;
      }
      val ourDotGraph = dot.right.get
      // XXX: Handle args(1) not being an int
      val prob = new Problem (ourDotGraph.names.size, ourDotGraph.edges, args(1).toInt)
      val bestSol = Solution.quickSol (prob)
      
      //println ("Problem: " + prob.toString)
      println ("*Start Value: " + bestSol.cost.toString)
      dotPrinter.dotPrinter.printDot (ourDotGraph, bestSol, args(2) + "_start")
      
      var newBestSol = bestSol
      val startSwap = newBestSol.tryAllSwaps
      println ("*Swapped Value: " + startSwap.cost.toString)      
      if (startSwap.cost < newBestSol.cost) {
         newBestSol = startSwap
         dotPrinter.dotPrinter.printDot (ourDotGraph, newBestSol, args(2) + "_swap")
      }         
      // should probably be dependent on problem size; maybe ceil (1.6 * sqrt (ps))?
      for (i <- Range (1, 12)) { 
         println ("#" + i)
         // Perhaps each anneal should get a different solution; completely random?
         val simulatedAnneal = Solution.simulatedAnnealing (prob, bestSol.cost, i)
         println ("*Annealed Value: " + simulatedAnneal.cost.toString)
         if (simulatedAnneal.cost < newBestSol.cost) {
            newBestSol = simulatedAnneal
            dotPrinter.dotPrinter.printDot (ourDotGraph, newBestSol, args(2) + "_anneal" + i)
         }    
         val swap2 = simulatedAnneal.tryAllSwaps
         if (swap2.cost < simulatedAnneal.cost) {
            println ("*Swap Value: " + swap2.cost.toString)
         }
         if (swap2.cost < newBestSol.cost) {
            newBestSol = swap2
            dotPrinter.dotPrinter.printDot (ourDotGraph, newBestSol, args(2) + "_anneal_swap" + i)
         }
      }
      val optimalSol = Solution.branchAndBound (newBestSol)
      println ("Optimal: " + optimalSol.toString)
      println ("*Optimal value: " + optimalSol.cost.toString)
      dotPrinter.dotPrinter.printDot (ourDotGraph, optimalSol, args(2) + "_optimal")
   }
   
}
