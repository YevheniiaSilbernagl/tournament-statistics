package types

import org.joda.time.DateTime

case class Tournament(battlefy_id: String, name: String, date: DateTime, season: Option[Int], event_type: Option[String], db_id: Option[Int] = None, checkInStarted: Boolean = false) {
  val premiere_event_types = List("invitational", "world_championship", "midseason_major")

  def isPremiereEvent: Boolean = event_type.exists(premiere_event_types.contains(_))
}