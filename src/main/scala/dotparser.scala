import java.io.FileNotFoundException
import java.io.IOException

package dotParser {

case class dotGraph (names : Map [Int, String], edges : List [(Int, Int)]);

object dotParse {

def trim (s : String) : String = {
   def trimHead (s : String) : String = {
      if (s.isEmpty) s
      else if (s.head == ' ') trimHead (s.tail)
      else s
   }
   def trimTail (s : String) : String = {
      trimHead (s.reverse).reverse
   }
   trimHead (trimTail (s))
}

def dotFileToGraph (filename : String) : Either [String, dotGraph] = {
   try {
      val fileContents = io.Source.fromFile(filename, "utf8").getLines.toList.
         map (x => trim (x.stripLineEnd))
      if (fileContents.head != "graph G {") return Left ("bad header: " + fileContents.head)
      if (fileContents.last != "}") return Left ("bad footer: " + fileContents.last)
      val realContents = fileContents.tail.init // remove header & footer
   
      val labelsRegex = new scala.util.matching.Regex ("""n(\d+)\[label="(.*)"];""", "node", "name")
      val edgesRegex = new scala.util.matching.Regex ("""n(\d+) -- n(\d+);""", "edge1", "edge2")
      def readLabel (s: String) = labelsRegex findFirstIn s match 
         {case Some(labelsRegex(node, name)) => (node.toInt, name)}
      def readEdge (s: String) = edgesRegex findFirstIn s match 
         {case Some(edgesRegex(edge1, edge2)) => (edge1.toInt, edge2.toInt)}
      def stripEscaping (s: String): String = {
         if (s.isEmpty) ""
         else if (s.head == '\\') {
            if (s.tail.isEmpty) ""
            else s.tail.head + stripEscaping(s.tail.tail)
         }
         else s.head + stripEscaping (s.tail)
      }
   
      val (names, edges) = realContents.partition (x => (labelsRegex findFirstIn x).nonEmpty)
      val namesMap = names.map (readLabel).map (t => (t._1, stripEscaping (t._2))).toMap
      if (!edges.forall (x => (edgesRegex findFirstIn x).nonEmpty)) {
         val invalidEdges = edges.filterNot (x => (edgesRegex findFirstIn x).nonEmpty).reduce (_ + "|" + _)
         return Left ("at least one non-edge, non-label found:" + invalidEdges)
      }
      val edgesParsed = edges.map (readEdge) 
      val newNames = (edgesParsed.map (_._1) ++ edgesParsed.map (_._2)).distinct.
         filterNot (namesMap.contains).map (x => (x, x.toString))
      val fullNamesMap = namesMap ++ newNames
      val newIdMap = fullNamesMap.map (_._1).zipWithIndex.toMap
      val newNameMap = fullNamesMap.toList.map (x => (newIdMap (x._1), x._2)).toMap
      val newEdges = edgesParsed.map (x => (newIdMap (x._1), newIdMap (x._2)))
      
      Right (dotGraph (newNameMap, newEdges))
   } 
   catch {
      case ex: FileNotFoundException => {
         return Left ("File not found: " + filename)
      }
      case ex: IOException => {
          return Left("Unknown IO exception:"+ ex)
      }
   }
}
}

}

