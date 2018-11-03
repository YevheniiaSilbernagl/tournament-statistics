package controllers

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font}
import java.io.File
import java.nio.file.Files
import java.util.concurrent.TimeUnit

import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.inject.Inject
import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.{DefaultResizerFactory, Resizer}
import org.docx4j.jaxb.Context
import org.docx4j.openpackaging.packages.WordprocessingMLPackage
import org.docx4j.wml._
import org.joda.time.DateTime
import play.api.Environment
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

class Application @Inject()(ws: WSClient, env: Environment) extends Controller {

  type EternalLink = String
  type EternalName = String
  type DiscordName = String

  val decksCache: mutable.HashMap[String, Deck] = scala.collection.mutable.HashMap()

  def file(path: String): Option[File] = Option(env.getExistingFile("/public" + path)
    .getOrElse(new File("/app/public" + path))).filter(_.exists())

  lazy val FONT: Option[Font] = {
    import java.awt.{Font, GraphicsEnvironment}
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val font_ = file("/fonts/Galdeano-Regular.ttf").map(fontFile => Font.createFont(Font.TRUETYPE_FONT, fontFile))
    font_.foreach(font => ge.registerFont(font))
    font_
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
    Ok(views.html.validation(listOfPlayers(tournamentId)))
  }

  def listOfPlayers(tournamentId: String): List[(EternalName, Option[EternalLink], Option[DiscordName])] =
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
    Ok(views.html.deckdoc(tournamentId, tournamentName(tournamentId), listOfPlayers(tournamentId).map { le =>
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
    val resizer: Resizer = DefaultResizerFactory.getInstance().getResizer(
      new Dimension(image.getWidth(), image.getHeight()),
      new Dimension(width, height))
    new FixedSizeThumbnailMaker(
      width, height, false, true).resizer(resizer).make(image)
  }

  def generateImage(player: (String, Option[String]),
                    side: String,
                    deckLink: Option[String],
                    deckName: Option[String] = None
                   ): Either[Exception, File] = {

    val playersName = Option(player._1).map(s => s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#")))).getOrElse(player._1)
    file(s"/images/background-$side.png") match {
      case Some(bg) => deckLink.map(getDeck) match {
        case Some(deck) =>
          val image = ImageIO.read(bg)
          val g = image.getGraphics
          FONT.foreach(f => g.setFont(f.deriveFont(48f)))
          val title = s"$playersName - ${deckName.getOrElse(if (deck.name.length > 30) s"${deck.name.substring(0, 20)}..." else deck.name)}"
          val titleWidth = g.getFontMetrics.stringWidth(title)

          g.drawString(title, (image.getWidth() - titleWidth) / 2, 80)
          FONT.foreach(f => g.setFont(f.deriveFont(30f)))
          var column = 0
          val max_column = 3
          val max_cards = 15
          val cardHeight = 48
          val cardWidth = image.getWidth / 3 - 20
          var counter = 0

          def block(i: Int) = i * 20 + column * cardWidth

          def drawCard(blockN: Int, name: String, quantity: Int) = {
            //cardWidth = cardImage+quantityImage
            val cardFile = file(s"/images/cards/$name.png").filter(_.exists())
              .getOrElse(file(s"/images/MISSING.png").get)
            val cardImage = scale(ImageIO.read(cardFile), cardWidth - cardHeight, cardHeight)
            val qImage = ImageIO.read(file(s"/images/quantity-blank.png").get)
            val quantityImage = scale(qImage, cardHeight, cardHeight)

            val dest = new BufferedImage(cardImage.getWidth + quantityImage.getWidth, cardImage.getHeight, BufferedImage.TYPE_INT_RGB)
            val renderedGraphics = dest.createGraphics()
            FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(30f)))
            renderedGraphics.drawImage(cardImage, 0, 0, null)
            renderedGraphics.drawImage(quantityImage, cardImage.getWidth, 0, null)

            val qString = quantity.toString
            renderedGraphics.drawString(qString,
              cardImage.getWidth + (quantityImage.getWidth() - renderedGraphics.getFontMetrics.stringWidth(qString)) / 2,
              (quantityImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)

            g.drawImage(dest, block(blockN), 160 + dest.getHeight * counter, null)

          }

          if (deck.mainDeck.nonEmpty) {
            val md = "Main deck:"
            g.drawString(md, block(1), 150)
            for (card <- deck.mainDeck) {
              if (counter >= max_cards) {
                counter = 0
                column = column + 1
              }
              drawCard(1, card._1.name, card._2)
              counter = counter + 1
            }
          }
          if (deck.market.nonEmpty) {
            if (column < max_column - 1) column = column + 1
            counter = 0
            val md = "Market:"
            g.drawString(md, block(2), 150)
            for (card <- deck.market) {
              if (counter >= max_cards) {
                counter = 0
                column = column + 1
              }
              drawCard(2, card._1.name, card._2)
              counter = counter + 1
            }
          }
          g.dispose()

          val resultFile = new File(s"${bg.getParent}/tourney-$side.png")
          val iter = ImageIO.getImageWritersByFormatName("png")
          val writer = iter.next()
          val iwp = writer.getDefaultWriteParam
          if (iwp.canWriteCompressed) {
            iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
            iwp.setCompressionQuality(1.0f)
          }
          writer.setOutput(new FileImageOutputStream(resultFile))
          writer.write(null, new IIOImage(image, null, null), iwp)
          writer.dispose()
          Right(resultFile)
        case _ => Left(new Exception(s"${player._1}'s deck unidentified"))
      }
      case _ =>
        Left(new Exception(s"${side.capitalize} background image not found on ${env.mode}"))
    }
  }

  def generateImages(player1: (String, Option[String]), player2: (String, Option[String])): (Either[Exception, File], Either[Exception, File]) = {
    val currentTournament = getCurrentTournament._1
    val currentPlayers = listOfPlayers(currentTournament)

    def generateLeft(player: (String, Option[String])): Either[Exception, File] = generateImage(player, "left", currentPlayers.filter(p => p._1 == player._1).flatMap(_._2).headOption)

    def generateRight(player: (String, Option[String])): Either[Exception, File] = generateImage(player, "right", currentPlayers.filter(p => p._1 == player._1).flatMap(_._2).headOption)

    (generateLeft(player1), generateRight(player2))
  }

  def generateResources = Action {
    request =>
      val body: Option[JsValue] = request.body.asJson
      body.flatMap(params => {
        (params \ "players").toOption.map(_.as[String]).map(_.split("[\\s-:]+")).map(players => {
          val scoreP1 = (params \ "p1score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          val scoreP2 = (params \ "p2score").toOption.map(_.as[String]).filterNot(_.equals("-"))
          generateImages((players(0), scoreP1), (players(1), scoreP2))
        })
      })
        .filter(images => images._1.isRight && images._2.isRight)
        .map(images => Ok(Json.obj(
          "left" -> images._1.right.get.getName,
          "right" -> images._2.right.get.getName
        ))).getOrElse(Ok("{}"))

  }

  def doc(tournament_id: String): Action[AnyContent] = Action {
    val tName = tournamentName(tournament_id)
    val exportFile = file(s"/$tName.docx").get
    val wordPackage = WordprocessingMLPackage.createPackage
    val mainDocumentPart = wordPackage.getMainDocumentPart
    val factory = Context.getWmlObjectFactory
    val b = new BooleanDefaultTrue

    def pageBreak: Br = {
      val breakObj = new Br
      breakObj.setType(STBrType.PAGE)
      breakObj
    }

    def textLine(text: String): R = {
      val r = factory.createR
      val t = factory.createText
      t.setValue(text)
      r.getContent.add(t)
      r.getContent.add(factory.createBr())
      r
    }

    def title(text: String) = {
      val paragraphProperties = factory.createPPr
      val justification = factory.createJc
      justification.setVal(JcEnumeration.CENTER)
      paragraphProperties.setJc(justification)

      val p = factory.createP
      val rpr = factory.createRPr
      rpr.setB(b)
      rpr.setI(b)
      val r = factory.createR
      val t = factory.createText
      t.setValue(text)
      r.getContent.add(t)
      r.setRPr(rpr)
      p.getContent.add(r)
      p.getContent.add(pageBreak)
      p.setPPr(paragraphProperties)
      mainDocumentPart.getContent.add(p)
    }

    def deck(playerName: String, deck: Deck): Unit = {
      val p = factory.createP
      p.getContent.add(textLine(playerName))
      p.getContent.add(textLine(deck.name))
      p.getContent.add(textLine(""))
      for (line <- deck.eternalFormat) {
        p.getContent.add(textLine(line))
      }
      p.getContent.add(pageBreak)

      mainDocumentPart.getContent.add(p)
    }

    title(tName)
    for ((eternalName, link, _) <- listOfPlayers(tournament_id)) {
      deck(eternalName, getDeck(link.get))
    }

    wordPackage.save(exportFile)
    Ok(Files.readAllBytes(exportFile.toPath))
      .withHeaders("Content-Type" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "content-disposition" -> s"""attachment; filename="${exportFile.getName}"""")
  }

  def side(side: String, link: String, name: String, player: String) = Action {
    generateImage((player, None), side, Some(link), Some(name)) match {
      case Right(file) => Ok(Files.readAllBytes(file.toPath)).withHeaders("Content-Type" -> "image/png")
      case Left(error) => NotFound(error.getMessage)
    }
  }

  def left(link: String, name: String, player: String): Action[AnyContent] = side("left", link, name, player)

  def right(link: String, name: String, player: String): Action[AnyContent] = side("right", link, name, player)
}
