package controllers

import java.io.File

import javax.inject.Inject
import play.api.Environment
import play.api.mvc.Controller

class FileSystem @Inject()(env: Environment) extends Controller {
  def file(path: String): Option[File] = Option(env.getExistingFile("/public" + path)
    .getOrElse(new File("/app/public" + path))).filter(_.exists())
  val parent: String = file(s"/images/background-left.png").map(_.getParent).get
}
