package controllers

import java.io.File
import java.nio.file.Files

import javax.inject.Inject
import org.docx4j.jaxb.Context
import org.docx4j.openpackaging.packages.WordprocessingMLPackage
import org.docx4j.wml.{Br, STBrType}
import play.api.mvc.{Action, AnyContent, Controller}
import types.{Deck, Tournament}

class Docs @Inject()(fs: FileSystem) extends Controller {
  def doc(tournament: Tournament, info: List[(String, Deck)]): File = {
    val exportFile = new File(s"${fs.parent}/${tournament.name}.docx")
    val wordPackage = WordprocessingMLPackage.createPackage
    val mainDocumentPart = wordPackage.getMainDocumentPart
    val factory = Context.getWmlObjectFactory
    val paragraph = factory.createP()

    val run = factory.createR

    def line(t: String) = {
      val lineElementText = factory.createText()
      lineElementText.setValue(t)
      run.getContent.add(lineElementText)
      val br = new Br
      br.setType(STBrType.TEXT_WRAPPING)
      run.getContent.add(br)
    }

    def pageBreak = {
      val br = new Br
      br.setType(STBrType.PAGE)
      run.getContent.add(br)
    }

    line(tournament.name)
    pageBreak
    for ((eternalName, deck) <- info) {
      line(eternalName.split("\\+")(0))
      line(deck.name)
      line(" ")
      for (card <- deck.eternalFormat) {
        line(card)
      }
      pageBreak
    }
    paragraph.getContent.add(run)


    mainDocumentPart.getContent.add(paragraph)
    wordPackage.save(exportFile)
    exportFile
  }
}
