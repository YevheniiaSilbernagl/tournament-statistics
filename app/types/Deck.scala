package types

import org.htmlcleaner.{HtmlCleaner, TagNode}

case class Deck(
                 link: String,
                 description: String,
                 name: String,
                 mainDeck: List[(Card, Int)],
                 sideBoard: List[(Card, Int)],
                 market: List[(Card, Int)],
                 notTournamentDeck: Boolean,
                 userDefinedArchetype: Option[String]
               ) {
  def eternalFormat: List[String] = {
    List("Main Deck:") ++ this.mainDeck.map(p => s"${p._2} ${p._1.eternalFormat}") ++
      List(" ", "--------------MARKET---------------") ++ this.market.map(p => s"${p._2} ${p._1.eternalFormat}")
  }

  def validate: List[String] = {
    var messages = List[String]()
    if (this.notTournamentDeck) messages = messages :+ "Is not a tournament deck"
    this.market.filterNot(p => p._1.name.contains("Sigil")).foreach(p => {
      val mainDeckCopies = mainDeck.find(e => e._1.equals(p._1))
      val numberOfCopies: Int = p._2 + mainDeckCopies.map(_._2).getOrElse(0)
      if (numberOfCopies > 4) {
        messages = messages :+ s"$numberOfCopies copies of ${p._1.eternalFormat}"
      }
    })
    if (this.market.map(_._2).sum > 5) messages = messages :+ s"Market has ${this.market.size} cards"
    if (market.exists(p => p._2 > 1)) messages = messages :+ "Market has duplicates"
    val mainDeckSize = this.mainDeck.map(_._2).sum
    if (mainDeckSize < 75 || mainDeckSize > 150) messages = messages :+ s"Size of the main deck is ${this.mainDeck.size}"
    val sideBoardSize = this.sideBoard.map(_._2).sum
    if (sideBoardSize > 0) messages = messages :+ s"Size of the sideboard is $sideBoardSize"
    messages
  }

  def cards: List[(Card, Int)] = mainDeck ++ market ++ sideBoard

  def eternalWarCryId: String = "details\\/(.+)\\/".r.findAllIn(link).matchData.toList.head.group(1)

  def faction: Option[String] = cards.flatMap(_._1.influences.toList).distinct.sortBy(f => f.toString.charAt(0)) match {
    case inf if inf.length == 1 => Some(inf.head.toString.toLowerCase.capitalize)
    case List(Faction.SHADOW, Faction.TIME) | List(Faction.TIME, Faction.SHADOW) => Some("Xenan")
    case List(Faction.SHADOW, Faction.PRIMAL) | List(Faction.PRIMAL, Faction.SHADOW) => Some("Feln")
    case List(Faction.SHADOW, Faction.JUSTICE) | List(Faction.JUSTICE, Faction.SHADOW) => Some("Argenport")
    case List(Faction.SHADOW, Faction.FIRE) | List(Faction.FIRE, Faction.SHADOW) => Some("Stonescar")

    case List(Faction.JUSTICE, Faction.FIRE) | List(Faction.FIRE, Faction.JUSTICE) => Some("Rakano")
    case List(Faction.JUSTICE, Faction.TIME) | List(Faction.TIME, Faction.JUSTICE) => Some("Combrei")
    case List(Faction.JUSTICE, Faction.PRIMAL) | List(Faction.PRIMAL, Faction.JUSTICE) => Some("Hooru")

    case List(Faction.FIRE, Faction.TIME) | List(Faction.TIME, Faction.FIRE) => Some("Praxis")
    case List(Faction.FIRE, Faction.PRIMAL) | List(Faction.PRIMAL, Faction.FIRE) => Some("Skycrag")

    case List(Faction.PRIMAL, Faction.TIME) | List(Faction.TIME, Faction.PRIMAL) => Some("Elysian")
    case List(Faction.JUSTICE, Faction.PRIMAL, Faction.TIME) => Some("TJP")

    case inf if inf.length == 3 => Some(inf.map(f => f.toString.charAt(0).toUpper).mkString)
    case _ => None
  }

  def archetype: Option[String] = faction.map(f => s"$f ${userDefinedArchetype.getOrElse(name.split("-").map(_.capitalize).filterNot(word => faction.contains(word)).mkString(" "))}")
}

object Deck {
  private val TOURNEY_IDENTIFIER = "<i class=\"fa fa-trophy\">"

  def parse(url: String, htmlSource: String): Deck = {
    val htmlCleaner = new HtmlCleaner
    val node = htmlCleaner.clean(htmlSource)
    val additionalCards = node.evaluateXPath("//*[@id='div-list']/*").map(_.asInstanceOf[TagNode]).filter(t => t.getAttributeByName("id") != "maindeck-wrapper").toList
    val archetypeValue = node.evaluateXPath("//tr[td[text()='Archetype']]/td[2]").headOption.map(_.asInstanceOf[TagNode]).map(_.getText.toString).filterNot(_ == "Unknown")
    val M = "Market"
    val S = "Sideboard"
    val markers = additionalCards.map(p => p.getText.toString).map {
      case t if t.contains(M) => M
      case t if t.contains(S) => S
      case _ => ""

    }
    val (sideboard, market): (List[TagNode], List[TagNode]) = (markers.indexOf("Sideboard"), markers.indexOf("Market")) match {
      case (-1, -1) => (List(), List())
      case (-1, 0) => (List(), additionalCards)
      case (0, -1) => (additionalCards, List())
      case (0, n) => (additionalCards.take(n), additionalCards.slice(n, additionalCards.length))
      case (n, 0) => (additionalCards.slice(n, additionalCards.length), additionalCards.take(n))
    }
    Deck(
      link = url,
      description = "Description?",
      name = url.split("/").last,
      mainDeck = cardsList(node.evaluateXPath("//*[@id='maindeck-wrapper']/a").toList.map(_.asInstanceOf[TagNode])),
      sideBoard = cardsList(sideboard.filterNot(t => t.getText.toString.contains(S) || t.getText.toString.contains(M))),
      market = cardsList(market.filterNot(t => t.getText.toString.contains(S) || t.getText.toString.contains(M))),
      notTournamentDeck = !htmlSource.contains(TOURNEY_IDENTIFIER),
      archetypeValue
    )
  }

  private def cardsList(nodes: List[TagNode]) = nodes.map(tag => (Card.parse(tag), tag.getAttributeByName("data-count").toInt))

  val empty: Deck = Deck("empty", "empty", "empty", List(), List(), List(), notTournamentDeck = false, Some("empty"))

}
