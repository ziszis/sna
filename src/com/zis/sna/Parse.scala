import io.Source
import java.io.File
import java.util.Date

case class Card(name: String, edition: String)
case class Point(date: Date, price: Int)

object Parser {
  def file(filename: String) : Iterator[(Card, Int, Int, Int)] = {
    val content = Source.fromFile(filename, "ISO-8859-1")
      .getLines()
      .dropWhile(!_.startsWith("==="))
      .filterNot(s => s.isEmpty || s.startsWith("==="))

    def parseCard(s: String) = {
      val p = """(.*) \[(.*)\]""".r
      s.trim match { case p(name, edition) => Card(name, edition) }
    }
    def parsePrice(s: String) = (s.trim.toFloat * 1000.0).toInt
    def parseVolume(s: String) = {
      val p = """.*\[(\d+)\]""".r
      s.trim.split(" ").map(_ match { case p(x) => x.toInt }).sum
    }

    content.map(_.split("   +").toList match {
      case List(a, b, c, d) => (parseCard(a), parsePrice(b), parsePrice(c), parseVolume(d))
      case List(a, c, d) => (parseCard(a), 0, parsePrice(c), parseVolume(d))
      case List(a, b) => (parseCard(a), parsePrice(b), 0, 0)
      case List(a) => (parseCard(a), 0, 0, 0)
      case s => throw new RuntimeException("Failed to parse " + s)
    })
  }
}

object Parse extends App {
  val root = new File("/Users/zis/Desktop/supernova")
  for (f <- root.list) {
    val ps = Parser.file(root.getAbsolutePath + "/" + f)
    val vind = ps.filter(p => p._1.name == "Vindicate" && p._1.edition == "AP" && p._3 != 0)
    if (!vind.isEmpty) println(f + "\t" + vind.map(_._3).min)
  }
}
