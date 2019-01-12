package controllers

import java.awt._
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.inject.Inject
import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.{DefaultResizerFactory, Resizer}
import org.joda.time.DateTime
import play.api.mvc.Controller
import types.{Card, Deck}

class Graphics @Inject()(fs: FileSystem, eternalWarcry: EternalWarcry, database: DB) extends Controller {

  lazy val FONT: Option[Font] = {
    import java.awt.{Font, GraphicsEnvironment}
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val font_ = fs.file("/fonts/Galdeano-Regular.ttf").map(fontFile => Font.createFont(Font.TRUETYPE_FONT, fontFile))
    font_.foreach(font => ge.registerFont(font))
    font_
  }

  def scale(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val resizer: Resizer = DefaultResizerFactory.getInstance().getResizer(
      new Dimension(image.getWidth(), image.getHeight()),
      new Dimension(width, height))
    new FixedSizeThumbnailMaker(
      width, height, false, true).resizer(resizer).make(image)
  }

  def graphicsSettings(g: Graphics2D): Graphics2D = {
    g.setComposite(AlphaComposite.SrcOver)
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY))
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY))
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY))
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE))
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_DEFAULT))
    g.addRenderingHints(new RenderingHints(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON))
    g
  }

  def adjustFontSize(g: Graphics2D, str: String, maxLength: Int, defaultFontSize: Float, maximumFontSize: Float): Float = {
    FONT.foreach(f => g.setFont(f.deriveFont(defaultFontSize)))
    val preferredFontSize = (for (s <- maximumFontSize to 1 by -1) yield (s, {
      FONT.foreach(f => g.setFont(f.deriveFont(s.toFloat)))
      g.getFontMetrics.stringWidth(str)
    })).find(_._2 <= maxLength).map(_._1.toFloat).getOrElse(defaultFontSize)

    preferredFontSize
  }

  def generateImage(player: (String, Option[String]),
                    side: String,
                    deck: Deck,
                    deckName: Option[String] = None
                   ): Either[Exception, File] = {

    val playersName = Option(player._1.trim).map(s => if (s.contains("+")) s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#"))) else s).getOrElse(player._1)
    fs.file(s"/images/background-$side.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())

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
        var hadToShift = false

        def block(i: Int) = i * 20 + column * cardWidth

        def drawCard(blockN: Int, cq: (Card, Int)) = {
          val (card, quantity) = cq
          val cardBGFile = (card match {
            case c if card.influences.isEmpty => fs.file(s"/images/cards/nofaction${if (c.isPower) "-power" else ""}.png")
            case c if card.influences.size == 1 => fs.file(s"/images/cards/${card.influences.head.toString.toLowerCase}${if (c.isPower) "-power" else ""}.png")
            case c => fs.file(s"/images/cards/multifaction${if (c.isPower) "-power" else ""}.png")
          }).getOrElse(fs.file(s"/images/cards/MISSING.png").get)
          val cardBGImage = scale(ImageIO.read(cardBGFile), cardWidth - cardHeight, cardHeight)
          val qImage = ImageIO.read(fs.file(s"/images/quantity-blank.png").get)
          val quantityImage = scale(qImage, cardHeight, cardHeight)

          val dest = new BufferedImage(cardBGImage.getWidth + quantityImage.getWidth, cardBGImage.getHeight, BufferedImage.TYPE_INT_ARGB)
          val renderedGraphics = graphicsSettings(dest.createGraphics())

          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(30f)))
          renderedGraphics.drawImage(cardBGImage, 0, 0, null)
          renderedGraphics.drawImage(quantityImage, cardBGImage.getWidth, 0, null)

          val icon = scale(eternalWarcry.cardIcon(card.name), 38, 38)
          renderedGraphics.drawImage(icon, 47, 7, null)


          //QUANTITY
          val qString = quantity.toString
          renderedGraphics.drawString(qString,
            cardBGImage.getWidth + (quantityImage.getWidth() - renderedGraphics.getFontMetrics.stringWidth(qString)) / 2,
            (quantityImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)

          //COST
          if (!card.isPower) {
            val cString = card.cost.toString
            renderedGraphics.drawString(cString,
              if (cString.length == 1) {
                FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(30f)))
                14
              } else {
                FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(25f)))
                9
              },
              (cardBGImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)
          }

          //NAME
          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(adjustFontSize(renderedGraphics, card.name, 150, 14f, 20f))))
          val nString = card.name.toString
          renderedGraphics.setColor(new Color(244, 206, 109))
          renderedGraphics.drawString(nString,
            95,
            (cardBGImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 3)
          g.drawImage(dest, block(blockN), 160 + dest.getHeight * counter, null)

        }

        if (deck.mainDeck.nonEmpty) {
          val md = "Main deck:"
          g.drawString(md, block(1) + 15, 150)
          for (card <- deck.mainDeck) {
            if (counter >= max_cards) {
              counter = 0
              column = column + 1
            }
            drawCard(1, card)
            counter = counter + 1
          }
        }
        if (deck.market.nonEmpty) {
          if (column < max_column - 1) {
            column = column + 1
            counter = 0
          }
          val md = if (deck.hasBlackMarket) "Black market:" else "Market:"
          if (counter > 0) {
            g.drawString(md, block(2) + 15, 150 + ((counter * cardHeight) + 50))
            counter += 1
            hadToShift = true
          } else {
            g.drawString(md, block(2) + 15, 150)
          }

          for (card <- deck.market) {
            if (counter >= max_cards) {
              counter = 0
              column = column + 1
            }
            drawCard(2, card)
            counter = counter + 1
          }
        }

        if (!hadToShift) {
          //stats block
          val margin_value = 230
          val margin_name = 40
          val margin_block_name1 = 100
          val margin_block_name2 = 90
          val spacing = 30
          val start = 570
          val underline_spacing = 3
          FONT.foreach(f => g.setFont(f.deriveFont(24f)))

          database.playerId(playersName).map(id => database.playerStats(id)).foreach {
            stats =>
              stats._2.find(_._1 == "Rounds: Win-Loss").foreach(record => {
                g.drawString("Win - Loss", block(2) + margin_name, start + spacing)
                g.drawString(record._3, block(2) + margin_value, start + spacing)
                g.drawString("Win - Loss", block(2) + margin_name, start + 5 * spacing)
                g.drawString(record._4, block(2) + margin_value, start + 5 * spacing)
              })
              stats._2.find(_._1 == "Rounds: Win-Loss %").foreach(record => {
                g.drawString("Win Rate", block(2) + margin_name, start + 2 * spacing)
                g.drawString(record._3.substring(0, record._3.indexOf("-")).trim, block(2) + margin_value, start + 2 * spacing)
                g.drawString("Win Rate", block(2) + margin_name, start + 6 * spacing)
                g.drawString(record._4.substring(0, record._4.indexOf("-")).trim, block(2) + margin_value, start + 6 * spacing)
              })
              val yr_str = s"${new DateTime().getYear} Record"
              g.drawString(yr_str, block(2) + margin_block_name1, start)
              g.drawLine(block(2) + margin_block_name1, start + underline_spacing, block(2) + margin_block_name1 + g.getFontMetrics.stringWidth(yr_str), start + underline_spacing)

              val cr_str = "ETS Career Stats"
              g.drawString(cr_str, block(2) + margin_block_name2, start + 4 * spacing)
              g.drawLine(block(2) + margin_block_name2, start + 4 * spacing + underline_spacing, block(2) + margin_block_name2 + g.getFontMetrics.stringWidth(cr_str), start + 4 * spacing + underline_spacing)
          }
        }

        g.dispose()

        val resultFile = new File(s"${fs.parent}/tourney-$side.png")
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
      case _ =>
        Left(new Exception(s"${side.capitalize} background image not found"))
    }
  }

  def casters(list: String): File = {
    val imageWidth = 278
    val darkerColour = new Color(15, 26, 56)
    val defaultColor = new Color(255, 255, 255)
    val image = new BufferedImage(imageWidth, 220, BufferedImage.TYPE_INT_ARGB)
    val g = graphicsSettings(image.createGraphics())

    //casters
    def center(graphics: Graphics2D, str: String) = (imageWidth - graphics.getFontMetrics.stringWidth(str)) / 2

    g.setColor(darkerColour)
    g.drawRect(0, 0, imageWidth - 2, 50)
    g.fillRect(0, 0, imageWidth - 2, 50)
    g.setColor(defaultColor)
    FONT.foreach(f => g.setFont(f.deriveFont(32f)))
    val castersStr = "Casters:"
    g.drawString(castersStr, center(g, castersStr), 35)
    val casters = list.split("\n")
    for (i <- casters.indices) {
      g.drawString(casters(i), 50, 50 + 40 * (i + 1))
    }
    g.dispose()

    val resultFile = new File(s"${fs.parent}/casters.png")
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
    resultFile
  }

  def sidePanel(
                 tournamentName: String,
                 round: String,
                 player1: (String, Int, String),
                 player2: (String, Int, String)
               ): File = {
    val imageWidth = 278
    val image = new BufferedImage(imageWidth, 910, BufferedImage.TYPE_INT_ARGB)
    val maxTextWidth = 270
    val darkerColour = new Color(15, 26, 56)
    val defaultColor = new Color(255, 255, 255)

    def center(graphics: Graphics2D, str: String) = (imageWidth - graphics.getFontMetrics.stringWidth(str)) / 2

    val g = graphicsSettings(image.createGraphics())
    g.setColor(defaultColor)
    val matchScore = ImageIO.read(fs.file(s"/images/matchscore.png").get)
    g.setColor(darkerColour)
    g.drawRect(0, 98, imageWidth - 2, 77)
    g.fillRect(0, 98, imageWidth - 2, 77)
    g.setColor(defaultColor)
    //tournament name
    FONT.foreach(f => g.setFont(f.deriveFont(adjustFontSize(g, tournamentName, maxTextWidth, 14f, 32f))))
    g.drawString(tournamentName, 10, 130)
    //round
    FONT.foreach(f => g.setFont(f.deriveFont(adjustFontSize(g, round, 230, 14f, 32f))))
    g.drawString(round, center(g, round), 160)
    //logo

    val additionalDistanceFromInfoBox = 50
    //score summary
    val summary = (player1._2, player2._2) match {
      case (s1, s2) if s1 == s2 => "Players tied"
      case (s1, s2) if s1 > s2 => s"${player1._1} leads"
      case (s1, s2) if s1 < s2 => s"${player2._1} leads"
    }
    FONT.foreach(f => g.setFont(f.deriveFont(adjustFontSize(g, summary, maxTextWidth, 14f, 42f))))
    g.drawString(summary, center(g, summary), 330 + additionalDistanceFromInfoBox)

    //match score
    FONT.foreach(f => g.setFont(f.deriveFont(64f)))
    val matchScoreY = 290
    g.drawImage(scale(matchScore, imageWidth, 207), 0, matchScoreY + additionalDistanceFromInfoBox, null)

    //score points
    g.drawString(player1._2.toString, 53, matchScoreY + 162 + additionalDistanceFromInfoBox)
    g.drawString(player2._2.toString, 192, matchScoreY + 162 + additionalDistanceFromInfoBox)


    val startY = 550
    val nameFont = scala.List(adjustFontSize(g, player1._1, maxTextWidth, 14f, 48f), adjustFontSize(g, player2._1, maxTextWidth, 14f, 48f)).min
    val deckNameFont = scala.List(adjustFontSize(g, player1._3, maxTextWidth, 14f, 48f), adjustFontSize(g, player2._3, maxTextWidth, 14f, 48f)).min
    //player1
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(player1._1, center(g, player1._1), startY + additionalDistanceFromInfoBox)
    val p1nHeights = g.getFontMetrics.getHeight
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(player1._3, center(g, player1._3), startY + p1nHeights + 5 + additionalDistanceFromInfoBox)
    val p1dnHeights = g.getFontMetrics.getHeight

    //VS
    FONT.foreach(f => g.setFont(f.deriveFont(128f)))
    val vsYPosition = startY + p1nHeights + p1dnHeights + 70
    val vs = "VS"
    g.drawString(vs, center(g, vs), vsYPosition + additionalDistanceFromInfoBox)

    //player2
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(player2._1, center(g, player2._1), vsYPosition + 60 + additionalDistanceFromInfoBox)
    val p2nHeights = g.getFontMetrics.getHeight
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(player2._3, center(g, player2._3), vsYPosition + p2nHeights + 65 + additionalDistanceFromInfoBox)
    val lineY = vsYPosition + p2nHeights + 85

    g.dispose()

    val resultFile = new File(s"${fs.parent}/left-side-panel.png")
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
    resultFile
  }

}
