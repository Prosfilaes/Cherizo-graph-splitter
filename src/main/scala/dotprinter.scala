import Solution._
import java.io._
import dotParser._

package dotPrinter {

object dotPrinter {
   private def escapeChars (s : String) : String = {
      if (s.isEmpty) ""
      else if (s.head == '"') "\\\"" + escapeChars (s.tail)
      else if (s.head == '\\') "\\\\" + escapeChars (s.tail)
      else s.head + escapeChars (s.tail)
   }
   
   private def printOnePage (dg : dotGraph, sol : CompleteSolution, fullname : String, pageNum : Int) = {
      val outFile = new File (fullname)
      val output = new PrintWriter (outFile)
      val pageSize = sol.prob.pageSize
      val page = sol.pointList.slice (pageSize * pageNum, pageSize * pageNum + pageSize)
      
      assert (page.length == pageSize)
      output.println ("graph G {")
      for (i <- page) {
         output.println ("n" + i + "[label=\"" + escapeChars (dg.names(i)) + "\"];")
      }
      for (i <- page; j <- page if i < j && sol.prob.edges (i).contains(j)) {
         output.println ("n" + i + " -- " + "n" + j + ";")
      }
      output.println ("}")
      output.close ()
   }
   
   def printDot (dg : dotGraph, sol: CompleteSolution, basename : String) = {
      for (i <- Range (0, sol.prob.countPoints / sol.prob.pageSize)) {
         printOnePage (dg, sol, basename + "-" + i + ".dot", i)
      }
   }
}
}
