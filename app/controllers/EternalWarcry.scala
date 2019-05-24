package controllers

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.TimeUnit

import javax.imageio.ImageIO
import javax.inject.Inject
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import play.libs.Json
import types._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class EternalWarcry @Inject()(ws: WSClient, cache: Cache) extends Controller {

  val cacheDuration: FiniteDuration = 30.days

  def changeDeckName(url: String, customName: String): Unit = {
    cache.put(url, getDeck(url).copy(name = customName), cacheDuration)
  }

  def card_icon: String => String = (name: String) => s"https://cards.eternalwarcry.com/cards/icon/${name.replaceAll("\\s", "_")}.jpg"

  def card_full_image: String => String = (name: String) => s"https://cards.eternalwarcry.com/cards/full/${name.replaceAll("\\s", "_")}.png"

  def getDeck(url: String): Deck = {
    val requestedDeck: Deck = cache.get[Deck](url).getOrElse({
      val deck = Await.result(ws.url(url.substring(0, Option(url.indexOf("?")).filterNot(_ < 0).getOrElse(url.length)))
        .get().map(response => Deck.parse(url, response.body)), Duration.apply(30, TimeUnit.SECONDS))
      if (deck.isTournamentDeck)
        cache.put(url, deck, cacheDuration)
      deck
    })
    cache.get[String](url + "_deck_name").map(custom_name => requestedDeck.copy(name = custom_name, userDefinedArchetype = Some(custom_name))).getOrElse(requestedDeck)
  }

  def getDeck(mainDeck: List[String], marketDeck: List[String]): Deck = {
    val jsonCardIterator = Json.parse(Await.result(ws.url("https://eternalwarcry.com/deck-builder/load-deck")
      .withQueryString(
        "cards" -> mainDeck.map(Card.parseDetails).map(d => s"${d._3}-${d._4}:${d._1}").mkString(";"),
        "marketCards" -> marketDeck.map(Card.parseDetails).map(d => s"${d._3}-${d._4}:${d._1}").mkString(";")
      ).post("").map(response => response.body), Duration.apply(30, TimeUnit.SECONDS))).elements()

    val cards: mutable.MutableList[(Card, String, Int)] = new mutable.MutableList[(Card, String, Int)]()

    while (jsonCardIterator.hasNext) {
      val jsonNode = jsonCardIterator.next()
      val card = Card(
        jsonNode.findValue("name").asText(),
        Faction.values.map(faction => (faction, jsonNode.findValue(faction.toString.toLowerCase).asInt())).toMap,
        jsonNode.findValue("cost").asInt(),
        jsonNode.findValue("setName").asText(),
        jsonNode.findValue("eternalID").asText(),
        jsonNode.findValue("isSigil").asBoolean()
      )
      val location = jsonNode.findValue("location").asText()
      val number = jsonNode.findValue("count").asInt()
      cards.+=((card, location, number))
    }
    val main: List[(Card, Int)] = cards.toList.filter(c => c._2 == "main").map(c => (c._1, c._3))
    val market: List[(Card, Int)] = cards.toList.filter(c => c._2 == "market").map(c => (c._1, c._3))
    Deck("", "", "", main, List(), market, false, None)
  }

  def cardIcon(name: String): BufferedImage = {
    val response = Await.result(ws.url(card_icon(name)).get(), Duration.apply(30, TimeUnit.SECONDS))
    ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
  }

  def cardFullImage(name: String): BufferedImage = {
    val response = Await.result(ws.url(card_full_image(name)).get(), Duration.apply(30, TimeUnit.SECONDS))
    ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
  }
}
