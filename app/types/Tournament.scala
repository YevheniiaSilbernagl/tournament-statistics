package types

import org.joda.time.DateTime

case class Tournament(id: Int, name: String, date: DateTime, season: Int)