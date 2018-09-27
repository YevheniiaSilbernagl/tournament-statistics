package types

import org.htmlcleaner.{HtmlCleaner, TagNode}

case class Deck(
                 link: String,
                 description: String,
                 name: String,
                 mainDeck: List[(Card, Int)],
                 sideBoard: List[(Card, Int)],
                 market: List[(Card, Int)],
                 notTournamentDeck: Boolean
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
    if (this.market.map(_._2).sum > 5) messages = messages :+  s"Market has ${this.market.size} cards"
    if (market.exists(p => p._2 > 1)) messages = messages :+  "Market has duplicates"
    val mainDeckSize = this.mainDeck.map(_._2).sum
    if (mainDeckSize < 75 || mainDeckSize > 150) messages = messages :+  s"Size of the main deck is ${this.mainDeck.size}"
    val sideBoardSize = this.sideBoard.map(_._2).sum
    if (sideBoardSize > 0) messages = messages :+  s"Size of the sideboard is $sideBoardSize"
    messages
  }
}

object Deck {
  private val TOURNEY_IDENTIFIER = "<i class=\"fa fa-trophy\">"

  def parse(url: String, htmlSource: String): Deck = {
    val htmlCleaner = new HtmlCleaner
    val node = htmlCleaner.clean(htmlSource)
    val additionalCards = node.evaluateXPath("//*[@id='div-list']/*").map(_.asInstanceOf[TagNode]).filter(t => t.getAttributeByName("id") != "maindeck-wrapper").toList
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
      sideBoard = cardsList(sideboard.filterNot(t => t.getText.toString.contains(S)||t.getText.toString.contains(M))),
      market = cardsList(market.filterNot(t => t.getText.toString.contains(S)||t.getText.toString.contains(M))),
      notTournamentDeck = !htmlSource.contains(TOURNEY_IDENTIFIER)
    )
  }

  private def cardsList(nodes: List[TagNode]) = nodes.map(tag => (Card.parse(tag), tag.getAttributeByName("data-count").toInt))
}
