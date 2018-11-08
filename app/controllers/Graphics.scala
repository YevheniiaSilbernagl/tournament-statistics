package controllers

import java.awt.image.BufferedImage
import java.awt.{Dimension, Font, RenderingHints}
import java.io.File

import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.inject.Inject
import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.{DefaultResizerFactory, Resizer}
import play.api.mvc.Controller
import types.Deck

class Graphics @Inject()(fs: FileSystem) extends Controller {

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

  def generateImage(player: (String, Option[String]),
                    side: String,
                    deck: Deck,
                    deckName: Option[String] = None
                   ): Either[Exception, File] = {

    val playersName = Option(player._1).map(s => s.substring(0, Option(s.indexOf("+")).filterNot(_ < 0).getOrElse(s.indexOf("#")))).getOrElse(player._1)
    fs.file(s"/images/background-$side.png") match {
      case Some(bg) =>
        val image = ImageIO.read(bg)
        val g = image.createGraphics()
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE))
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON))
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY))
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY))
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
          val cardFile = fs.file(s"/images/cards/$name.png").filter(_.exists())
            .getOrElse(fs.file(s"/images/MISSING.png").get)
          val cardImage = scale(ImageIO.read(cardFile), cardWidth - cardHeight, cardHeight)
          val qImage = ImageIO.read(fs.file(s"/images/quantity-blank.png").get)
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
