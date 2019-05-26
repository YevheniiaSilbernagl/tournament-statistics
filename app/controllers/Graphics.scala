package controllers

import java.awt._
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.inject.Inject
import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.{DefaultResizerFactory, Resizer}
import org.jfree.chart.ChartFactory
import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.plot.PlotOrientation._
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.ui.RectangleInsets
import org.joda.time.DateTime
import play.api.mvc.Controller
import types.{Card, Deck}

class Graphics @Inject()(fs: FileSystem, eternalWarcry: EternalWarcry, database: DB) extends Controller {
  private val defaultColor = new Color(255, 255, 255)
  private val defaultYellow = new Color(244, 206, 109)
  private val upArrow: Option[BufferedImage] = fs.file("/images/up_arrow.png").map(ImageIO.read)

  lazy val FONT: Option[Font] = {
    import java.awt.{Font, GraphicsEnvironment}
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val font_ = fs.file("/fonts/Galdeano-Regular.ttf").map(fontFile => Font.createFont(Font.TRUETYPE_FONT, fontFile))
    font_.foreach(font => ge.registerFont(font))
    font_
  }

  def scale(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    if (image == null) image
    else {
      val resizer: Resizer = DefaultResizerFactory.getInstance().getResizer(
        new Dimension(image.getWidth(), image.getHeight()),
        new Dimension(width, height))
      new FixedSizeThumbnailMaker(
        width, height, false, true).resizer(resizer).make(image)
    }
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

  def adjustFontSize(g: Graphics2D, maxHeight: Int, defaultFontSize: Float, maximumFontSize: Float, additionalPadding: Int): Float = {
    FONT.foreach(f => g.setFont(f.deriveFont(defaultFontSize)))
    val preferredFontSize = (for (s <- maximumFontSize to 1 by -1) yield (s, {
      FONT.foreach(f => g.setFont(f.deriveFont(s.toFloat)))
      g.getFontMetrics.getHeight + additionalPadding
    })).find(_._2 <= maxHeight).map(_._1.toFloat).getOrElse(defaultFontSize)

    preferredFontSize
  }

  def saveFile(g: Graphics2D, image: BufferedImage, fileName: String): File = {
    g.dispose()

    val resultFile = new File(s"${fs.parent}/$fileName")
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

  def statsList(header: String, top10: scala.List[(String, scala.List[(String, String, String, String)], Boolean)]): File = {
    def column(players: scala.List[(String, scala.List[(String, String, String, String)], Boolean)], fontSize: Float): BufferedImage = {
      val dest = new BufferedImage(350, 880, BufferedImage.TYPE_INT_ARGB)
      val renderedGraphics = graphicsSettings(dest.createGraphics())
      FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(fontSize)))
      for (i <- players.indices) {
        val player = players(i)
        renderedGraphics.setColor(defaultYellow)
        renderedGraphics.drawString(s"${player._1.split("\\+")(0)}", 0, (3 * i + 1) * 40 + i * 40)
        renderedGraphics.setColor(defaultColor)
        if (player._2(3)._2 == "-") {
          renderedGraphics.drawString(s"No games [0 - 0]", 0, (3 * i + 2) * 40 + i * 40)
        } else {
          renderedGraphics.drawString(s"${player._2(4)._2.split("-")(0).trim} [${player._2(3)._2}]", 0, (3 * i + 2) * 40 + i * 40)
        }
        if (player._2(4)._4 == "-") {
          renderedGraphics.drawString(s"No games [0 - 0]", 0, (3 * i + 3) * 40 + i * 40)
        } else {
          renderedGraphics.drawString(s"${player._2(4)._4.split("-")(0).trim} [${player._2(3)._4}]", 0, (3 * i + 3) * 40 + i * 40)
        }
      }
      dest
    }

    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(header, 220, 105)

        val longestLine = top10.map(_._1).sortBy(_.length).reverse.head + " - 100.00%[100%]"
        val fontSize = adjustFontSize(g, longestLine, 600, 34f, 60f)
        g.drawImage(column(top10.take(4), fontSize), 250, 200, null)
        g.drawImage(column(top10.slice(4, 8), fontSize), 630, 200, null)
        g.drawImage(column(top10.slice(8, 12), fontSize), 1030, 200, null)
        g.drawImage(column(top10.slice(12, 16), fontSize), 1420, 200, null)

        saveFile(g, image, "top-players.png")
      case _ => throw new Exception(s"background image not found")
    }
  }

  def topPlayers(header: String, top10: scala.List[(String, String)]): File = {

    def column(players: scala.List[(String, String)], fontSize: Float): BufferedImage = {
      val dest = new BufferedImage(600, 480, BufferedImage.TYPE_INT_ARGB)
      val renderedGraphics = graphicsSettings(dest.createGraphics())
      FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(fontSize)))
      for (i <- players.indices)
        renderedGraphics.drawString(s"${players(i)._1.split("\\+")(0)} - ${players(i)._2.split("-")(0).trim}", 0, (i + 1) * 80)
      dest
    }

    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(header, 220, 105)

        val longestLine = top10.map(_._1).sortBy(_.length).reverse.head + " - 100.00%"
        val fontSize = adjustFontSize(g, longestLine, 600, 34f, 60f)
        g.drawImage(column(top10.take(5), fontSize), 350, 300, null)
        g.drawImage(column(top10.drop(5), fontSize), 1100, 300, null)

        saveFile(g, image, "top-players.png")
      case _ => throw new Exception(s"background image not found")
    }
  }

  def invitationalPoints(results: scala.List[(String, Int, Int, scala.List[String])],
                         currentTournamentPlayers: scala.List[String],
                         playersWithPotential: scala.List[(String, Int, Int, scala.List[String])]
                        ): File = {
    val tmp = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB)
    val tmpGraphics = graphicsSettings(tmp.createGraphics())
    val additionalLinePadding = 6
    val arrowSize = 25
    val starSize = 50

    def stringValue(player: (String, Int, Int, scala.List[String])): String = s"${player._1.split("\\+")(0)} - ${(player._2 + player._3).toString}"

    def column(players: scala.List[(String, Int, Int, scala.List[String])], fontSize: Float): BufferedImage = {
      FONT.foreach(f => tmpGraphics.setFont(f.deriveFont(fontSize)))
      val lineHeights = tmpGraphics.getFontMetrics.getHeight + additionalLinePadding
      val lineLength = tmpGraphics.getFontMetrics.stringWidth(results.map(stringValue).maxBy(_.length))
      val dest = new BufferedImage(lineLength + 2 * arrowSize, lineHeights * players.size + additionalLinePadding, BufferedImage.TYPE_INT_ARGB)
      val renderedGraphics = graphicsSettings(dest.createGraphics())
      FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(fontSize)))
      val star = fs.file("/images/winner_star.png").map(ImageIO.read)
      for (i <- players.indices) {
        val player = players(i)
        if (currentTournamentPlayers.contains(player._1)) {
          renderedGraphics.setColor(new Color(187, 231, 157))
        } else {
          renderedGraphics.setColor(defaultColor)
        }
        if (player._4.nonEmpty) {
          star.foreach(s => renderedGraphics.drawImage(scale(s, starSize, starSize), 0, (i + 1) * lineHeights - starSize, null))
          renderedGraphics.drawString(stringValue(player), starSize + 5, (i + 1) * lineHeights)
        } else {
          val xPosition = (if (player._3 > 0) upArrow else None) match {
            case Some(arrow) =>
              renderedGraphics.drawImage(scale(arrow, arrowSize, arrowSize), 0, (i + 1) * lineHeights - arrowSize - 5, null)
              arrowSize + 5
            case None =>
              0
          }
          renderedGraphics.drawString(stringValue(player), xPosition, (i + 1) * lineHeights)
        }
      }
      dest
    }

    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(s"The Desk - Invitational points", 220, 105)

        val col1Height = 770.0
        val longestLine = (results ++ playersWithPotential).map(stringValue).maxBy(tmpGraphics.getFontMetrics.stringWidth)
        val batchSize = (results.size.doubleValue() / 3).round.intValue()
        val fontSize = scala.List(adjustFontSize(g, longestLine, 430, 9f, 60f),
          adjustFontSize(g, (col1Height / batchSize).intValue(), 9f, 60f, additionalLinePadding)).min
        FONT.foreach(f => g.setFont(f.deriveFont(fontSize)))
        val col1 = results.take(batchSize)
        val col3 = results.slice(results.size - (results.size - col1.size) / 2, results.size)
        val col2 = results.slice(col1.size, results.size - col3.size)
        val img1 = column(col1, fontSize)
        val img2 = column(col2, fontSize)
        val img3 = column(col3, fontSize)
        val img4 = column(playersWithPotential.take(col1.size - 1), fontSize)

        val paddingCol1 = {
          val padding = 65
          65 + (image.getHeight - padding - img1.getHeight) / 2 - padding
        }
        val distance = 5
        val firstPosition = 50
        g.drawImage(img1, firstPosition, paddingCol1, null)
        g.drawImage(img2, firstPosition + img1.getWidth + distance, paddingCol1, null)
        g.drawImage(img3, firstPosition + img1.getWidth + img2.getWidth + 2 * distance, paddingCol1, null)
        g.setComposite(AlphaComposite.SrcOver.derive(0.5f))
        g.drawImage(img4, firstPosition + img1.getWidth + img2.getWidth + img3.getWidth + 3 * distance, paddingCol1, null)

        saveFile(g, image, "invitational-points.png")
      case _ => throw new Exception(s"background image not found")
    }
  }


  def communityChampionshipPoints(results: scala.List[(String, Int, Int)],
                                  currentTournamentPlayers: scala.List[String],
                                  havePotential: scala.List[(String, Int, Int)]): File = {
    val tmp = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB)
    val tmpGraphics = graphicsSettings(tmp.createGraphics())
    val additionalLinePadding = 11
    val arrowSize = 35

    def stringValue(player: (String, Int, Int)): String = s"${player._1.split("\\+")(0)} - ${(player._2 + player._3).toString}"

    def column(players: scala.List[(String, Int, Int)], fontSize: Float): BufferedImage = {
      FONT.foreach(f => tmpGraphics.setFont(f.deriveFont(fontSize)))
      val lineHeights = tmpGraphics.getFontMetrics.getHeight + additionalLinePadding
      val lineLength = tmpGraphics.getFontMetrics.stringWidth(results.map(stringValue).maxBy(_.length))
      val dest = new BufferedImage(lineLength + arrowSize, lineHeights * players.size + additionalLinePadding + 50, BufferedImage.TYPE_INT_ARGB)
      val renderedGraphics = graphicsSettings(dest.createGraphics())
      FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(fontSize)))
      for (i <- players.indices) {
        val player = players(i)
        if (currentTournamentPlayers.contains(player._1)) {
          renderedGraphics.setColor(new Color(187, 231, 157))
        } else {
          renderedGraphics.setColor(defaultColor)
        }
        val xPosition = (if (player._3 > 0) upArrow else None) match {
          case Some(arrow) =>
            renderedGraphics.drawImage(scale(arrow, arrowSize, arrowSize), 0, (i + 1) * lineHeights - arrowSize, null)
            arrowSize
          case None =>
            0
        }
        renderedGraphics.drawString(stringValue(player), xPosition, (i + 1) * lineHeights)
      }
      dest
    }

    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(s"The Desk - Community championship", 220, 105)

        val col1Height = 770.0
        val colNextHeight = 920.0
        val longestLine = results.map(stringValue).maxBy(_.length)
        val batchSize = (results.size.doubleValue() / 2).round.intValue()
        val fontSize = scala.List(adjustFontSize(g, longestLine, 600, 16f, 60f), adjustFontSize(g, (col1Height / batchSize).intValue(), 34f, 60f, additionalLinePadding)).min
        FONT.foreach(f => g.setFont(f.deriveFont(fontSize)))
        val col1 = results.take(batchSize)
        val col2 = results.drop(batchSize)

        val img1 = column(col1, fontSize)
        val img2 = column(col2, fontSize)
        val img3 = column(havePotential, fontSize)
        val paddingCol1 = {
          val padding = 65
          65 + (image.getHeight - padding - img1.getHeight) / 2 - padding
        }
        val distance = 15
        val firstPosition = 250

        g.drawImage(img1, firstPosition, paddingCol1, null)
        g.drawImage(img2, firstPosition + img1.getWidth + distance, paddingCol1, null)
        g.setComposite(AlphaComposite.SrcOver.derive(0.5f))
        g.drawImage(img3, firstPosition + img1.getWidth + img2.getWidth + 2 * distance, paddingCol1, null)

        saveFile(g, image, "community-championship-points.png")
      case _ => throw new Exception(s"background image not found")
    }
  }

  def topCards(cards: scala.List[(String, Int)], ecq: Boolean = false): File = {
    if (cards.size > 10) throw new Exception(s"Too many cards to display [max 10]")
    else fs.file(s"/images/${if (ecq) "ecq/" else ""}background${if (ecq) "-ecq" else ""}.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(s"${if (ecq) "" else "The Desk - "}Top ${cards.size} cards this week", 220, 105)
        val numberOfLines = if (cards.size > 6) 2 else 1
        val cardSize = (236, 350)

        def cardsLine(cardList: scala.List[(String, Int)]): BufferedImage = {
          val dest = new BufferedImage(if (cardList.size > 5) 1700 else 1430, cardSize._2 + 30, BufferedImage.TYPE_INT_ARGB)
          val renderedGraphics = graphicsSettings(dest.createGraphics())
          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(30f)))
          val width = dest.getWidth / cardList.size
          for (i <- cardList.indices) {
            val image = scale(eternalWarcry.cardFullImage(cardList(i)._1), cardSize._1, cardSize._2)
            renderedGraphics.setColor(defaultYellow)
            val number = cardList(i)._2.toString
            val wordWidth = renderedGraphics.getFontMetrics.stringWidth(number)
            renderedGraphics.drawString(number, (width * i + 20) + (cardSize._1 / 2) - wordWidth / 2, 18)
            renderedGraphics.drawImage(image, width * i + 20, 10, null)
          }
          dest
        }

        if (numberOfLines == 1) {
          val line = cardsLine(cards)
          g.drawImage(line, (image.getWidth - line.getWidth) / 2, 360, null)
        } else {
          val line1 = cardsLine(cards.take(cards.size / 2))
          val line2 = cardsLine(cards.drop(cards.size / 2))
          g.drawImage(line1, (image.getWidth - line2.getWidth) / 2, 135, null)
          g.drawImage(line2, (image.getWidth - line1.getWidth) / 2, 535, null)
        }

        saveFile(g, image, s"top-${cards.size}-cards.png")
      case _ => throw new Exception(s"background image not found")
    }
  }

  def customCardList(header: String, cards: scala.List[String]): File = {
    if (cards.size > 20) throw new Exception(s"Too many cards to display [max 10]")
    else fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        g.drawString(s"The Desk - $header", 220, 105)
        val numberOfLines = if (cards.size > 6) 2 else 1
        val cardProportions = (236, 350)
        val totalWidth = 1700

        def cardsLine(cardList: scala.List[String]): BufferedImage = {
          val cardSize: (Int, Int) = if (numberOfLines == 2) cardProportions else {
            val width = scala.List((totalWidth / cardList.size * 0.95).intValue(), 400).min
            val height = (width * (cardProportions._2.doubleValue() / cardProportions._1)).intValue()
            (width, height)
          }
          val dest = new BufferedImage(totalWidth, cardSize._2 + 30, BufferedImage.TYPE_INT_ARGB)
          val renderedGraphics = graphicsSettings(dest.createGraphics())
          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(30f)))
          val width = dest.getWidth / cardList.size
          for (i <- cardList.indices) {
            val image = scale(eternalWarcry.cardFullImage(cardList(i)), cardSize._1, cardSize._2)
            renderedGraphics.setColor(new Color(244, 206, 109))
            renderedGraphics.drawImage(image, width * i + (width - cardSize._1) / 2, 10, null)
          }
          dest
        }

        if (numberOfLines == 1) {
          val line = cardsLine(cards)
          g.drawImage(line, (image.getWidth - line.getWidth) / 2, (image.getHeight - line.getHeight) / 2, null)
        } else {
          val line1 = cardsLine(cards.take(cards.size / 2))
          val line2 = cardsLine(cards.drop(cards.size / 2))
          g.drawImage(line1, (image.getWidth - line2.getWidth) / 2, 135, null)
          g.drawImage(line2, (image.getWidth - line1.getWidth) / 2, 535, null)
        }

        saveFile(g, image, s"${header.replaceAll("\\.", "_")}.png")
      case _ => throw new Exception(s"background image not found")
    }
  }

  def deckImage(player: (String, Option[String]),
                side: String,
                deck: Deck,
                deckName: Option[String] = None,
                ecq: Boolean = false
               ): Either[Exception, File] = {

    val playersName = Option(player._1.trim).map(s => if (s.contains("+")) s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#"))) else s).getOrElse(player._1)
    fs.file(s"/images/ecq/background-$side${if (ecq) "-ecq" else ""}.png") match {
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
          column = 2
          val margin_value = 220
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
              val wins = stats._2.find(_._1 == "Winner").map(_._4.split(":")(0)).filterNot(_ == "-\n ")
              val top2 = stats._2.find(_._1 == "Top 2").map(_._4.split(":")(0)).filterNot(_ == "-\n ")
              val top4 = stats._2.find(_._1 == "Top 4").map(_._4.split(":")(0)).filterNot(_ == "-\n ")
              var length = block(2) + margin_name

              FONT.foreach(f => g.setFont(f.deriveFont(30f)))

              def draw(list: Option[String], imageName: String): Unit = {
                list.foreach(win => {
                  val gold: Option[BufferedImage] = fs.file(s"/images/$imageName").map(ImageIO.read)
                  gold.foreach { image =>
                    val str = win match {
                      case "once" => "1"
                      case "twice" => "2"
                      case s => s.split("\\s")(0)
                    }
                    g.drawImage(scale(image, 40, 40), length, start + 7 * spacing, null)
                    length = length + 40
                    g.drawString(s"x $str", length, start + 30 + 7 * spacing)
                    length = length + 50
                  }
                })
              }

              draw(wins, "gold.png")
              draw(top2, "silver.png")
              draw(top4, "bronze.png")
          }
        }

        Right(saveFile(g, image, s"tourney-$side.png"))
      case _ =>
        Left(new Exception(s"${side.capitalize} background image not found"))
    }
  }

  def oneDeckImage(player: (String, Option[String]),
                   deck: Deck,
                   deckName: Option[String] = None,
                   ecq: Boolean = false
                  ): Either[Exception, File] = {

    val playersName = Option(player._1.trim).map(s => if (s.contains("+")) s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#"))) else s).getOrElse(player._1)
    fs.file(s"/images/${if(ecq) "ecq/" else ""}background${if(ecq) "-ecq" else ""}.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())

        FONT.foreach(f => g.setFont(f.deriveFont(48f)))
        val title = s"$playersName - ${deckName.getOrElse(if (deck.name.length > 30) s"${deck.name.substring(0, 20)}..." else deck.name)}"
        val titleWidth = g.getFontMetrics.stringWidth(title)

        g.drawString(title, (image.getWidth() - titleWidth) / 2, 80)
        FONT.foreach(f => g.setFont(f.deriveFont(36f)))
        var column = 0
        val max_column = 4
        val max_cards = 10
        val cardHeight = 60
        val cardWidth = 375
        var counter = 0
        var hadToShift = false
        val yDistanceToHeaders = 200

        def block(i: Int) = 160 + i * 20 + column * cardWidth

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

          val icon = scale(eternalWarcry.cardIcon(card.name), cardHeight - 10, cardHeight - 10)
          renderedGraphics.drawImage(icon, cardHeight - 5, 7, null)


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
                20
              } else {
                FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(25f)))
                16
              },
              (cardBGImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)
          }

          //NAME
          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(adjustFontSize(renderedGraphics, card.name, 150, 14f, 24f))))
          val nString = card.name.toString
          renderedGraphics.setColor(new Color(244, 206, 109))
          renderedGraphics.drawString(nString, cardHeight * 2,
            (cardBGImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 3)
          g.drawImage(dest, block(blockN), yDistanceToHeaders + dest.getHeight * counter, null)

        }

        if (deck.mainDeck.nonEmpty) {
          val md = "Main deck:"
          g.drawString(md, block(1) + 15, yDistanceToHeaders - 5)
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
            g.drawString(md, block(2) + 15, yDistanceToHeaders - 5 + ((counter * cardHeight) + 50))
            counter += 1
            hadToShift = true
          } else {
            g.drawString(md, block(2) + 15, yDistanceToHeaders - 5)
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
          column = 3
          val margin_value = 220
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

        Right(saveFile(g, image, s"deck-image.png"))
      case _ =>
        Left(new Exception(s"Background image not found"))
    }
  }

  def casters(casters: scala.List[(java.lang.String, Option[BufferedImage])]): File = {
    val imageWidth = 278
    val darkerColour = new Color(15, 26, 56)
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
    for (i <- casters.indices) {
      g.drawString(casters(i)._1, 65, 50 + 40 * (i + 1))
      casters(i)._2.foreach(avatar => g.drawImage(scale(avatar, 40, 40), 10, 15 + 40 * (i + 1), null))
    }
    saveFile(g, image, "casters.png")
  }

  def castersEcq(casters: scala.List[(java.lang.String, Option[BufferedImage])]): File = {
    val imageWidth = 278
    val image = new BufferedImage(imageWidth, 220, BufferedImage.TYPE_INT_ARGB)
    val g = graphicsSettings(image.createGraphics())

    //casters
    def center(graphics: Graphics2D, str: String) = (imageWidth - graphics.getFontMetrics.stringWidth(str)) / 2

    g.setColor(defaultColor)
    FONT.foreach(f => g.setFont(f.deriveFont(60f)))
    val castersStr = "Casters"
    g.drawString(castersStr, center(g, castersStr), 35)
    FONT.foreach(f => g.setFont(f.deriveFont(40f)))
    for (i <- casters.indices) {
      g.drawString(casters(i)._1, 47, 40 + 40 * (i + 1))
      casters(i)._2.foreach(avatar => g.drawImage(scale(avatar, 37, 37), 5, 5 + 40 * (i + 1), null))
    }
    saveFile(g, image, "casters.png")
  }

  def ecqSidePannel(mainCam: (String, String, Int), handCam: (String, String, Int)): File = {
    val imageWidth = 278
    val image = new BufferedImage(imageWidth, 910, BufferedImage.TYPE_INT_ARGB)
    val maxTextWidth = 270

    def center(graphics: Graphics2D, str: String) = (imageWidth - graphics.getFontMetrics.stringWidth(str)) / 2

    val g = graphicsSettings(image.createGraphics())
    g.setColor(defaultColor)

    val matchScore = ImageIO.read(fs.file(s"/images/ecq/smallBox.png").get)

    val nameFont = scala.List(adjustFontSize(g, handCam._1, maxTextWidth, 14f, 48f), adjustFontSize(g, mainCam._1, maxTextWidth, 14f, 48f)).min
    val deckNameFont = scala.List(adjustFontSize(g, handCam._2, maxTextWidth, 14f, nameFont - 10f), adjustFontSize(g, mainCam._2, maxTextWidth, 14f, 48f)).min

    //player2name
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(handCam._1, center(g, handCam._1), 165)
    //player2 deckname
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(handCam._2, center(g, handCam._2), 210)
    //player2 score
    g.drawImage(scale(matchScore, 135, 135), 70, 270, null)
    FONT.foreach(f => g.setFont(f.deriveFont(160f)))
    g.drawString(handCam._3.toString, center(g, handCam._3.toString), 380)
    //best of 3
    val bo3 = "Best of Three"
    val font = adjustFontSize(g, bo3, maxTextWidth, 14f, 48f)
    FONT.foreach(f => g.setFont(f.deriveFont(font)))
    g.drawString(bo3, center(g, bo3), 465)
    //player1score
    g.drawImage(scale(matchScore, 135, 135), 70, 500, null)
    FONT.foreach(f => g.setFont(f.deriveFont(160f)))
    g.drawString(mainCam._3.toString, center(g, mainCam._3.toString), 610)

    //player1name
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(mainCam._1, center(g, mainCam._1), 720)
    //player1deckname
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(mainCam._2, center(g, mainCam._2), 765)

    saveFile(g, image, "left-side-panel.png")
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


    val startY = 550 + additionalDistanceFromInfoBox
    val nameFont = scala.List(adjustFontSize(g, player1._1, maxTextWidth, 14f, 48f), adjustFontSize(g, player2._1, maxTextWidth, 14f, 48f)).min
    val deckNameFont = scala.List(adjustFontSize(g, player1._3, maxTextWidth, 14f, 48f), adjustFontSize(g, player2._3, maxTextWidth, 14f, 48f)).min
    //player1
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(player1._1, center(g, player1._1), startY)
    val p1nHeights = g.getFontMetrics.getHeight
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(player1._3, center(g, player1._3), startY + (p1nHeights + g.getFontMetrics.getHeight) / 2 + 5)
    val p1dnHeights = g.getFontMetrics.getHeight

    //VS
    FONT.foreach(f => g.setFont(f.deriveFont(128f)))
    val vsYPosition = startY + p1nHeights + p1dnHeights + 75
    val vs = "VS"
    g.drawString(vs, center(g, vs), vsYPosition)

    //player2
    FONT.foreach(f => g.setFont(f.deriveFont(nameFont)))
    g.drawString(player2._1, center(g, player2._1), vsYPosition + 60)
    val p2nHeights = g.getFontMetrics.getHeight
    FONT.foreach(f => g.setFont(f.deriveFont(deckNameFont)))
    g.drawString(player2._3, center(g, player2._3), vsYPosition + (p2nHeights + g.getFontMetrics.getHeight) / 2 + 65)
    val lineY = vsYPosition + p2nHeights + 85
    saveFile(g, image, "left-side-panel.png")
  }


  def trend(header: String, stats: scala.List[(DateTime, Double, Double)]): File = {
    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        val theDesk = "The Desk - "
        g.drawString(theDesk, 220, 105)
        val theDeskWidth = g.getFontMetrics.stringWidth(theDesk)
        FONT.foreach(f => g.setFont(f.deriveFont(adjustFontSize(g, header, 1650 - theDeskWidth, 80f, 110f))))
        g.drawString(header, 220 + theDeskWidth, 105)

        val dataSet = new DefaultCategoryDataset()
        stats.foreach { dataPoint =>
          val date = dataPoint._1.toString("MMM yyyy")
          dataSet.addValue(dataPoint._2, "rounds win rate", date)
          dataSet.addValue(dataPoint._3, "games win rate", date)
        }
        val trans = new Color(0, 0, 0, 0)
        val white = new Color(255, 255, 255)
        val chart = ChartFactory.createLineChart("", "Tournament date", "Win rate(%)", dataSet, VERTICAL, true, false, false)
        val plot = chart.getPlot
        val categoryPlot = chart.getCategoryPlot
        val legend = chart.getLegend
        plot.setBackgroundPaint(null)
        chart.setBackgroundPaint(trans)
        chart.setPadding(new RectangleInsets(3, 3, 3, 3))
        plot.setBackgroundPaint(trans)
        legend.setBackgroundPaint(trans)
        FONT.map(_.deriveFont(30f)).foreach { font =>
          legend.setItemFont(font)
          categoryPlot.getRangeAxis.setLabelFont(font)
          categoryPlot.getDomainAxis.setLabelFont(font)
          categoryPlot.getRangeAxis.setTickLabelFont(font)
        }
        FONT.map(_.deriveFont(24f)).foreach { font =>
          categoryPlot.getDomainAxis.setTickLabelFont(font)
        }

        legend.setItemPaint(white)
        plot.setOutlinePaint(white)
        categoryPlot.getRangeAxis.setLabelPaint(white)
        categoryPlot.getDomainAxis.setLabelPaint(white)
        categoryPlot.getRangeAxis.setAxisLinePaint(white)
        categoryPlot.getDomainAxis.setAxisLinePaint(white)
        categoryPlot.getDomainAxis.setTickLabelPaint(white)
        categoryPlot.getRangeAxis.setTickLabelPaint(white)

        categoryPlot.getDomainAxis.setMaximumCategoryLabelLines(2)
        categoryPlot.getDomainAxis.setCategoryLabelPositions(CategoryLabelPositions.STANDARD)

        val minY = scala.List(stats.map(_._2).min, stats.map(_._3).min).min - 5.0
        categoryPlot.getRangeAxis.setLowerBound(minY)

        categoryPlot.getRenderer.setSeriesPaint(0, new Color(205, 192, 172))
        categoryPlot.getRenderer.setSeriesStroke(0, new BasicStroke(3f))
        categoryPlot.getRenderer.setSeriesPaint(1, new Color(255, 161, 18))
        categoryPlot.getRenderer.setSeriesStroke(1, new BasicStroke(3f))

        val chartImage = chart.createBufferedImage(1500, 800)
        g.drawImage(chartImage, (image.getWidth - chartImage.getWidth) / 2, (image.getHeight - chartImage.getHeight) / 2, chartImage.getWidth, chartImage.getHeight, null)

        saveFile(g, image, s"${header.replaceAll("\\s", "-")}.png")
      case _ => throw new Exception(s"background image not found")
    }

  }

  def compare(header: String,
              player1: (String, scala.List[(DateTime, Double, Double)]),
              player2: (String, scala.List[(DateTime, Double, Double)])): File = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isAfter _)

    fs.file(s"/images/background.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = graphicsSettings(image.createGraphics())
        FONT.foreach(f => g.setFont(f.deriveFont(110f)))
        val str = s"${player1._1} VS ${player2._1} $header"
        val theDesk = "The Desk - "
        g.drawString(theDesk, 220, 105)
        val theDeskWidth = g.getFontMetrics.stringWidth(theDesk)
        FONT.foreach(f => g.setFont(f.deriveFont(adjustFontSize(g, str, 1650 - theDeskWidth, 80f, 110f))))
        g.drawString(str, 220 + theDeskWidth, 105)

        val dataSet = new DefaultCategoryDataset()
        (player1._2.map(_._1) ++ player2._2.map(_._1)).distinct.sorted.reverse.foreach { date =>
          player1._2.filterNot(_._1.isAfter(date)).sorted.headOption.foreach { dataPoint =>
            dataSet.addValue(dataPoint._2, player1._1, dataPoint._1.toString("MMM yyyy"))
          }
          player2._2.filterNot(_._1.isAfter(date)).sorted.headOption.foreach { dataPoint =>
            dataSet.addValue(dataPoint._2, player2._1, dataPoint._1.toString("MMM yyyy"))
          }
        }
        val trans = new Color(0, 0, 0, 0)
        val white = new Color(255, 255, 255)
        val chart = ChartFactory.createLineChart("", "Tournament date", "Win rate(%)", dataSet, VERTICAL, true, false, false)
        val plot = chart.getPlot
        val categoryPlot = chart.getCategoryPlot
        val legend = chart.getLegend
        plot.setBackgroundPaint(null)
        chart.setBackgroundPaint(trans)
        chart.setPadding(new RectangleInsets(3, 3, 3, 3))
        plot.setBackgroundPaint(trans)
        legend.setBackgroundPaint(trans)
        FONT.map(_.deriveFont(30f)).foreach { font =>
          legend.setItemFont(font)
          categoryPlot.getRangeAxis.setLabelFont(font)
          categoryPlot.getDomainAxis.setLabelFont(font)
          categoryPlot.getRangeAxis.setTickLabelFont(font)
        }
        FONT.map(_.deriveFont(24f)).foreach { font =>
          categoryPlot.getDomainAxis.setTickLabelFont(font)
        }

        legend.setItemPaint(white)
        plot.setOutlinePaint(white)
        categoryPlot.getRangeAxis.setLabelPaint(white)
        categoryPlot.getDomainAxis.setLabelPaint(white)
        categoryPlot.getRangeAxis.setAxisLinePaint(white)
        categoryPlot.getDomainAxis.setAxisLinePaint(white)
        categoryPlot.getDomainAxis.setTickLabelPaint(white)
        categoryPlot.getRangeAxis.setTickLabelPaint(white)
        categoryPlot.getDomainAxis.setMaximumCategoryLabelLines(2)
        categoryPlot.getDomainAxis.setCategoryLabelPositions(CategoryLabelPositions.STANDARD)

        val minY = scala.List(player1._2.map(_._2).min, player1._2.map(_._3).min, player2._2.map(_._2).min, player2._2.map(_._3).min).min - 5.0
        categoryPlot.getRangeAxis.setLowerBound(minY)

        categoryPlot.getRenderer.setSeriesPaint(0, new Color(205, 192, 172))
        categoryPlot.getRenderer.setSeriesStroke(0, new BasicStroke(3f))
        categoryPlot.getRenderer.setSeriesPaint(1, new Color(255, 161, 18))
        categoryPlot.getRenderer.setSeriesStroke(1, new BasicStroke(3f))

        val chartImage = chart.createBufferedImage(1500, 800)
        g.drawImage(chartImage, (image.getWidth - chartImage.getWidth) / 2, (image.getHeight - chartImage.getHeight) / 2, chartImage.getWidth, chartImage.getHeight, null)

        saveFile(g, image, s"${player1._1}-vs-${player2._1}.png")
      case _ => throw new Exception(s"background image not found")
    }

  }
}
