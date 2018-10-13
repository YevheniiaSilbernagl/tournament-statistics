package controllers

import java.awt.Font
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files
import java.util.concurrent.TimeUnit

import javax.imageio.ImageIO
import javax.inject.Inject
import org.joda.time.DateTime
import play.api.libs.json.{JsValue, _}
import play.api.libs.ws.WSClient
import play.api.mvc._
import types.Deck

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal

class Application @Inject()(ws: WSClient) extends Controller {

  type EternalLink = String
  type EternalName = String
  type DiscordName = String

  val decksCache: mutable.HashMap[String, Deck] = scala.collection.mutable.HashMap()

  lazy val sourceDir: String = Option(s"${new File("").getAbsolutePath}/public").filter(path => new File(path).exists())
    .getOrElse(s"${new File("").getAbsolutePath}/assets")

  lazy val FONT: Font = {
    import java.awt.{Font, GraphicsEnvironment}
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val font = Font.createFont(Font.TRUETYPE_FONT, new File(s"$sourceDir/fonts/Galdeano-Regular.ttf"))
    ge.registerFont(font)
    font
  }


  def all_tournaments: () => String = () => "https://dtmwra1jsgyb0.cloudfront.net/organizations/5a0e00fdc4cd48033c0083b7/tournaments"

  def stage_info: String => String = (stage: String) => s"https://dtmwra1jsgyb0.cloudfront.net/stages/$stage/matches"

  def players: String => String = (tournament_id: String) => s"https://dtmwra1jsgyb0.cloudfront.net/tournaments/$tournament_id/teams"

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)


  def index = Action {
    Ok(views.html.index(getCurrentTournament))
  }

  def streamingResources = Action {
    Ok(views.html.streaming(getCurrentPlayersList))
  }

  def validateDeck(url: String) = Action {
    try {
      val result = decksCache.get(url) match {
        case Some(d) => d.validate
        case None =>
          val d = getDeck(url)
          val validation = d.validate
          if (validation.isEmpty) decksCache.put(url, d)
          validation
      }
      Ok(Json.obj("valid" -> result.isEmpty, "messages" -> result))
    } catch {
      case NonFatal(e) => Ok(Json.obj("valid" -> false, "messages" -> Json.arr(e.getMessage)))
    }
  }

  def getDeck(url: String): Deck = decksCache.getOrElse(url,
    Await.result(ws.url(url.substring(0, Option(url.indexOf("?")).filterNot(_ < 0).getOrElse(url.length)))
      .get().map(response => Deck.parse(url, response.body)), Duration.apply(30, TimeUnit.SECONDS)))

  def validateDecks(tournamentId: String) = Action {
    Ok(views.html.validation(currentListOfPlayers(tournamentId)))
  }

  def currentListOfPlayers(tournamentId: String): List[(EternalName, Option[EternalLink], Option[DiscordName])] =
    Await.result(ws.url(players(tournamentId)).get().map(response => {
      val list = Json.parse(response.body).asInstanceOf[JsArray].value.toList
      list
        .map(p => {
          val eternalName = (p \ "name").as[String]
          val customFields = (p \ "customFields" \\ "value").toList.map(_.as[String])
          (eternalName, customFields.find(_.contains("eternalwarcry")), customFields.filterNot(_.contains("eternalwarcry")).headOption)
        }
        )
    }), Duration.apply(30, TimeUnit.SECONDS))

  def generateDeckDoc(tournamentId: String) = Action {
    Ok(views.html.deckdoc(tournamentName(tournamentId), currentListOfPlayers(tournamentId).map { le =>
      val name = le._1.substring(0, Some(le._1.indexOf("+")).filter(_ > 0).getOrElse(le._1.length))
      (le._1, le._2.map(getDeck))
    }))
  }

  def tournamentName(tournamentId: String): String = Await.result(ws.url(all_tournaments()).get().map(response => {
    Json.parse(response.body).asInstanceOf[JsArray].value.toList
      .map(_.as[JsObject]).find(t => t.value("_id").as[String] == tournamentId)
      .map(t => t.value("name").as[String]).getOrElse("")
  }), Duration.apply(30, TimeUnit.SECONDS))

  def getCurrentPlayersList: (String, List[String]) = {
    Await.result(ws.url(all_tournaments()).get().flatMap(response => {
      val tournament = Json.parse(response.body).asInstanceOf[JsArray].value.toList
        .sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.head
      val currentStage = tournament.\("stages").as[JsArray].value.toList.sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.headOption
      if (currentStage.isEmpty) Future((tournament.\("name").as[String], List()))
      else {
        val stage = currentStage.get.\("_id").as[String]
        ws.url(stage_info(stage)).get().map(resp => {
          def name(node: JsValue): String = node.\("team").\("name").toOption.map(_.as[String]).getOrElse("Undefined name")

          val rounds = Json.parse(resp.body).asInstanceOf[JsArray].value.toList
          if (currentStage.get.\("bracket").get.\("type").get.asInstanceOf[JsString].value == "elimination") {
            (tournament.\("name").as[String], rounds.iterator.toList.map(r => name(r.\("top").get) + " - : - " + name(r.\("bottom").get)))
          } else {
            val maxRound = rounds.map(r => r.\("roundNumber").as[Int]).max
            val round = rounds.filter(o => o.\("roundNumber").as[Int] == maxRound)
            (tournament.\("name").as[String], round.iterator.toList.map(r => name(r.\("top").get) + " - : - " + name(r.\("bottom").get)))
          }
        })
      }
    }), Duration.apply(30, TimeUnit.SECONDS))
  }

  def getCurrentTournament: (String, String) = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

    Await.result(ws.url(all_tournaments()).get().map(response => {
      val tournament = Json.parse(response.body).asInstanceOf[JsArray].value.toList.sortBy(o => DateTime.parse(o.\("startTime").as[String])).reverse.head
      (tournament.\("_id").as[String], tournament.\("name").as[String])
    }), Duration.apply(30, TimeUnit.SECONDS))
  }

  def scale(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val ws = width.doubleValue() /image.getWidth
    val hs = height.doubleValue()/image.getHeight
    val dest = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val transform = AffineTransform.getScaleInstance(ws, hs)
    val renderedGraphics = dest.createGraphics()
    renderedGraphics.drawRenderedImage(image, transform)
    dest
  }

  def generateImage(player: (String, Option[String]), side: String, deckName: Option[String] = None): String = {
    val currentTournament = getCurrentTournament._1
    val currentPlayers = currentListOfPlayers(currentTournament)
    val playersName = Option(player._1).map(s => s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#")))).getOrElse(player._1)
    val targetFile = new File(s"$sourceDir/images/$currentTournament-$playersName-$side.png")
    new File(s"$sourceDir/images/background-$side.png") match {
        case bg if bg.exists() =>
          currentPlayers.filter(p => p._1 == player._1).flatMap(_._2).map(getDeck).headOption match {
            case Some(deck) =>
              val image = ImageIO.read(bg)
              val g = image.getGraphics
              g.setFont(FONT.deriveFont(48f))
              val title = s"$playersName - ${deckName.getOrElse(if (deck.name.length > 30) s"${deck.name.substring(0, 20)}..." else deck.name)}"
              val titleWidth = g.getFontMetrics.stringWidth(title)

              g.drawString(title, (image.getWidth() - titleWidth) / 2, 80)
              g.setFont(FONT.deriveFont(30f))
              val oneColumnWidth = image.getWidth / 3
              var column = 0
              val max_column = 3
              val max_cards = 15
              val cardHeight = 50
              val cardWidth = 250
              var counter = 0

              def drawCard(name: String, quantity: Int) = {
                val cardFile = Option(new File(s"$sourceDir/images/cards/$name.png")).filter(_.exists())
                  .getOrElse(new File(s"$sourceDir/images/MISSING.png"))
                val cardImage = scale(ImageIO.read(cardFile), cardWidth, cardHeight)
                val qImage = ImageIO.read(new File(s"$sourceDir/images/quantity-blank.png"))
                val quantityScale = cardImage.getHeight().doubleValue() / qImage.getHeight().doubleValue()
                val quantityImage = scale(qImage, cardHeight, cardHeight)

                val dest = new BufferedImage(cardImage.getWidth + quantityImage.getWidth, cardImage.getHeight, BufferedImage.TYPE_INT_RGB)
                val renderedGraphics = dest.createGraphics()
                renderedGraphics.setFont(FONT.deriveFont(30f))
                renderedGraphics.drawImage(cardImage, 0, 0, null)
                renderedGraphics.drawImage(quantityImage, cardImage.getWidth, 0, null)

                val qString = quantity.toString
                renderedGraphics.drawString(qString,
                  cardImage.getWidth + (quantityImage.getWidth() - renderedGraphics.getFontMetrics.stringWidth(qString)) / 2,
                  (quantityImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)

                g.drawImage(dest, column * oneColumnWidth + 20, 160 + dest.getHeight * counter, null)

              }

              if (deck.mainDeck.nonEmpty) {
                val md = "Main deck:"
                g.drawString(md, 40, 150)
                for (card <- deck.mainDeck) {
                  if (counter >= max_cards) {
                    counter = 0
                    column = column + 1
                  }
                  drawCard(card._1.name, card._2)
                  counter = counter + 1
                }
              }
              if (deck.market.nonEmpty) {
                if (column < max_column - 1) column = column + 1
                counter = 0
                val md = "Market:"
                g.drawString(md, column * oneColumnWidth + 40, 150)
                for (card <- deck.market) {
                  if (counter >= max_cards) {
                    counter = 0
                    column = column + 1
                  }
                  drawCard(card._1.name, card._2)
                  counter = counter + 1
                }
              }
              g.dispose()

              val resultFile = new File(s"$sourceDir/images/$currentTournament-$playersName-$side.png")

              ImageIO.write(image, "png", resultFile)
              resultFile.getName
            case _ => throw new Exception(s"${player._1}'s deck unidentified")
          }
        case _ =>
          new File("").listFiles().map(_.getName).mkString(",")
      }
  }

  def generateImages(player1: (String, Option[String]), player2: (String, Option[String])): (String, String) = {

    def generateLeft(player: (String, Option[String])): String = generateImage(player, "left")

    def generateRight(player: (String, Option[String])): String = generateImage(player, "right")

    (generateLeft(player1), generateRight(player2))
  }

  def generateResources = Action {
    request =>
      val body: Option[JsValue] = request.body.asJson
      body.flatMap(params => {
        (params \ "players").toOption.map(_.as[String]).map(_.split("[\\s-:]+")).map(players => {
          val scoreP1 = (params \ "p1score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          val scoreP2 = (params \ "p2score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          val images = generateImages((players(0), scoreP1), (players(1), scoreP2))
          Ok(Json.obj("left" -> images._1, "right" -> images._2))
        })
      }).getOrElse(Ok("{}"))

  }

  def side(side: String, link: String, name: String, player: String) = Action {
    val file = new File(sourceDir + "/images/" + generateImage((player, None), side, Some(name)))
    if (file.exists())
      Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png")
    else NotFound
  }

  def left(link: String, name: String, player: String): Action[AnyContent] = side("left", link, name, player)

  def right(link: String, name: String, player: String): Action[AnyContent] = side("right", link, name, player)
}
