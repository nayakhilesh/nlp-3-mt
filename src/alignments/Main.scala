package alignments;

import scala.io.Source

object Main {

  def main(args: Array[String]) {

    val source = Source.fromFile("corpus.en", "utf-8").getLines
    val target = Source.fromFile("corpus.es", "utf-8").getLines
    val map = collection.mutable.Map()

    val n = collection.mutable.Map[String, collection.mutable.Set[String]]()

    for ((s, t) <- source.zip(target)) {
      //println(s + " =source| target= " + t);
      s.split(" ").foreach(e =>
        n(e) = n.getOrElse(e, collection.mutable.Set()) ++= t.split(" ").distinct)
    }

    val target2 = Source.fromFile("corpus.es", "utf-8").getLines
    target2.foreach(f =>
      n("NULL") = n.getOrElse("NULL", collection.mutable.Set()) ++= f.split(" ").distinct)

    print(n("NULL"))
    //println("hey")
    //print(n.toString())

  }

}