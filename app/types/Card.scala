package types

import java.util.Optional
import java.util.stream.{Collectors, IntStream}

import org.htmlcleaner.TagNode

case class Card(
                 name: String,
                 influence: Map[Faction.Value, Int],
                 cost: Int,
                 set: String,
                 eternalId: String,
                 isPower: Boolean
               ) {
  def eternalFormat: String = s"$name (Set$set #$eternalId)"

}

object Card {
  private val ASCII = "&#(\\d+);".r

  def parse(tag: TagNode): Card = Card(
    decode(tag.getAttributeByName("data-name")),
    Faction.values.map(faction => (faction, tag.getAttributeByName("data-" + faction.toString.toLowerCase()).toInt)).toMap,
    tag.getAttributeByName("data-cost").toInt,
    tag.getAttributeByName("data-set"),
    tag.getAttributeByName("data-eternalid"),
    "power".equalsIgnoreCase(tag.getAttributeByName("data-group")))

  def decode(text: String): String = {
    var modifiedText = text
    for (t <- ASCII.findAllIn(modifiedText)) {
      modifiedText = modifiedText.replace(t, Character.toString(t.toInt.toChar))
    }
    modifiedText.replaceAll("&amp;", "&")
  }
}