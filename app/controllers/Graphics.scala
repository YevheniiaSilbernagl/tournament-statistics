package controllers

import java.awt._
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.inject.Inject
import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.{DefaultResizerFactory, Resizer}
import play.api.mvc.Controller
import types.{Card, Deck}

class Graphics @Inject()(fs: FileSystem, eternalWarcry: EternalWarcry) extends Controller {

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

  def generateImage(player: (String, Option[String]),
                    side: String,
                    deck: Deck,
                    deckName: Option[String] = None
                   ): Either[Exception, File] = {

    val playersName = Option(player._1).map(s => s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#")))).getOrElse(player._1)
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
              renderedGraphics.getFontMetrics.stringWidth(cString),
              (cardBGImage.getHeight() + renderedGraphics.getFontMetrics.getHeight) / 2 - 7)
          }


          def font(str: String): Float = {
            val defaultFontSize = 14f
            FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(defaultFontSize)))
            val maxLength = 150

            val preferredFontSize = (for (s <- 20 to 1 by -1) yield (s, {
              FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(s.toFloat)))
              renderedGraphics.getFontMetrics.stringWidth(str)
            })).find(_._2 <= maxLength).map(_._1.toFloat).getOrElse(defaultFontSize)

            preferredFontSize
          }
          //NAME
          FONT.foreach(f => renderedGraphics.setFont(f.deriveFont(font(card.name))))
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
          if (column < max_column - 1) column = column + 1
          counter = 0
          val md = "Market:"
          g.drawString(md, block(2) + 15, 150)
          for (card <- deck.market) {
            if (counter >= max_cards) {
              counter = 0
              column = column + 1
            }
            drawCard(2, card)
            counter = counter + 1
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


}
