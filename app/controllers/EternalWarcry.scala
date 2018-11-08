package controllers

import java.util.concurrent.TimeUnit

import javax.inject.Inject
import play.api.libs.ws.WSClient
import play.api.mvc.Controller
import types.Deck

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class EternalWarcry @Inject()(ws: WSClient) extends Controller {
  val decksCache: mutable.HashMap[String, Deck] = scala.collection.mutable.HashMap()

  def getDeck(url: String): Deck = decksCache.getOrElse(url,
    {
      val deck = Await.result(ws.url(url.substring(0, Option(url.indexOf("?")).filterNot(_ < 0).getOrElse(url.length)))
        .get().map(response => Deck.parse(url, response.body)), Duration.apply(30, TimeUnit.SECONDS))
      decksCache.put(url, deck)
      deck
    })
}
