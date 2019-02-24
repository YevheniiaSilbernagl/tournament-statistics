package controllers

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.TimeUnit

import javax.imageio.ImageIO
import javax.inject.Inject
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import types.Deck

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

  def cardIcon(name: String): BufferedImage = {
    val response = Await.result(ws.url(card_icon(name)).get(), Duration.apply(30, TimeUnit.SECONDS))
    ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
  }

  def cardFullImage(name: String): BufferedImage = {
    val response = Await.result(ws.url(card_full_image(name)).get(), Duration.apply(30, TimeUnit.SECONDS))
    ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
  }
}
