import io.Source
import java.io.File

case class Card(name: String, edition: String)
case class Point(card: Card, buy: Int, sell: Int, volume: Int)

object Parser {
  private val volumeRegexp = """.*\[(\d+)\]""".r
  private val cardRegexp = """(.*) \[(.*)\]""".r

  def file(filename: String) : Iterator[Point] = {
    def parseCard(s: String) = s.trim match { case cardRegexp(name, edition) => Card(name, edition) }
    def parsePrice(s: String) = (s.trim.toFloat * 1000.0).toInt
    def parseVolume(s: String) = s.trim.split(" ").map { case volumeRegexp(x) => x.toInt }.sum

    Source.fromFile(filename, "ISO-8859-1")
      .getLines()
      .dropWhile(!_.startsWith("==="))
      .filterNot(s => s.isEmpty || s.startsWith("==="))
      .map(_.split("   +").toList match {
        case List(a, b, c, d) => Point(parseCard(a), parsePrice(b), parsePrice(c), parseVolume(d))
        case List(a, c, d) => Point(parseCard(a), 0, parsePrice(c), parseVolume(d))
        case List(a, b) => Point(parseCard(a), parsePrice(b), 0, 0)
        case List(a) => Point(parseCard(a), 0, 0, 0)
        case s => throw new RuntimeException("Failed to parse " + s)
    })
  }
}

object Parse extends App {
  val root = new File("/Users/zis/Desktop/supernova")
  def selectCard(name: String, edition: String) =
    root.list.flatMap(filename => {
      Parser.file(root.getAbsolutePath + "/" + filename).filter(p => p.card.name == name && p.card.edition == edition)
    })
  def avg(pts:Seq[Int]) = {
    val x = pts.map((_, 1)).reduce((a, b) => (a._1+b._1, a._2+b._2))
    x._1 / x._2
  }
  def avgSell(cards: Seq[Point]) = cards.filter(_.volume != 0).map(_.sell)
  val start = System.currentTimeMillis()
  println(avg(selectCard("Vindicate", "AP").filter(_.volume != 0).map(_.sell)))
  val end = System.currentTimeMillis()
  println(end - start)
}
