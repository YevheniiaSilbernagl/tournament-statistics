package controllers

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.TimeUnit

import javax.imageio.ImageIO
import javax.inject.Inject
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import types.Deck

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class EternalWarcry @Inject()(ws: WSClient) extends Controller {
  def changeDeckName(url: String, customName: String): Unit = {
    decksCache.put(url, getDeck(url).copy(name = customName))
  }

  val decksCache: mutable.HashMap[String, Deck] = scala.collection.mutable.HashMap()

  def card_icon: String => String = (name: String) => s"https://cards.eternalwarcry.com/cards/icon/${name.replaceAll("\\s", "_")}.jpg"

  def getDeck(url: String): Deck = decksCache.getOrElse(url,
    {
      val deck = Await.result(ws.url(url.substring(0, Option(url.indexOf("?")).filterNot(_ < 0).getOrElse(url.length)))
        .get().map(response => Deck.parse(url, response.body)), Duration.apply(30, TimeUnit.SECONDS))
      decksCache.put(url, deck)
      deck
    })

  def cardIcon(name: String): BufferedImage = {
    val response = Await.result(ws.url(card_icon(name)).get(), Duration.apply(30, TimeUnit.SECONDS))
    ImageIO.read(new ByteArrayInputStream(response.bodyAsBytes))
  }
}
