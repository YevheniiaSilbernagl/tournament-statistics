package types

import org.joda.time.DateTime

case class Tournament(
                       battlefy_id: String,
                       name: String,
                       date: DateTime,
                       season: Option[Int],
                       event_type: Option[String],
                       db_id: Option[Int] = None,
                       checkInStarted: Boolean,
                       registrationEnabled:Boolean,
                       checkInStartTime: Option[DateTime],
                       currentStage: Option[String] = None) {
  def normalizedName:String = name.replaceAll("[/]","-")

  def bracketInfo: String = battlefyLink(currentStage.map(stage => s"stage/$stage/bracket/").getOrElse("info"))

  val premiere_event_types = List("invitational", "world_championship", "midseason_major")
  val include_in_series_points_event_types = List("invitational", "world_championship", "weekly")

  def isPremiereEvent: Boolean = event_type.exists(premiere_event_types.contains(_))

  def includeInSeriesPointsCalculation: Boolean = event_type.exists(include_in_series_points_event_types.contains(_))

  def isWeekly: Boolean = event_type.contains("weekly")

  def battlefyLink(section: String): String =
    s"https://battlefy.com/eternal-tournament-series/${name.replaceAll("[\\(\\)\\-\\,]","").replaceAll("\\s+", "-").toLowerCase}/$battlefy_id/$section"
}