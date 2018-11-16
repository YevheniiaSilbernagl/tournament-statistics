package types

import org.joda.time.DateTime

case class Tournament(battlefy_id: String, name: String, date: DateTime, season: Option[Int], db_id: Option[Int] = None, checkInStarted: Boolean = false)