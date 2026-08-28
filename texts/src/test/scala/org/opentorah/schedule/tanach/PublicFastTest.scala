package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.{Jewish, NewYear}
import org.opentorah.calendar.jewish.SpecialDay.*
import org.opentorah.texts.tanach.Custom
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A public fast is read with Vayechal in the morning as well as at Mincha.
 * getWeekdayMorningReading had no case for the fasts, so it fell through to
 * its throw and no fast on a weekday could be scheduled at all -- which also
 * meant a whole year could not be built, since every year contains fasts.
 */
final class PublicFastTest extends AnyFlatSpec, Matchers:

  private val fasts = Seq(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  "a public fast on a weekday" should "have a morning and an afternoon reading" in:
    var weekdays = 0
    var moved = 0
    for
      number <- 5700 to 5900
      inHolyLand <- Seq(false, true)
      fast <- fasts
    do
      val day = fast.date(Year(number))
      val schedule = Schedule.get(day, inHolyLand)
      withClue(s"$number ${fast.getClass.getSimpleName} shabbos=${day.isShabbos}: ") {
        schedule.morning.isDefined shouldBe true
        // date() is the nominal date; a fast landing on Shabbos is moved, and
        // that Shabbos is an ordinary Shabbos with no fast reading.
        if day.isShabbos then moved += 1 else
          weekdays += 1
          schedule.afternoon.isDefined shouldBe true
      }
    weekdays should be > 0
    moved should be > 0

  it should "read three aliyot" in:
    for
      number <- 5700 to 5900
      fast <- fasts
    do
      val day = fast.date(Year(number))
      if !day.isShabbos then
        withClue(s"$number ${fast.getClass.getSimpleName}: ")(
          Schedule.get(day, inHolyLand = false).morning.get
            .torah.customs.values.map(_.length).toSet shouldBe Set(3))

  "the minor fasts" should "read Vayechal in the morning, without a haftarah" in:
    var checked = 0
    for
      number <- 5700 to 5900
      fast <- Seq(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz)
    do
      val day = fast.date(Year(number))
      if !day.isShabbos then
        val morning = Schedule.get(day, inHolyLand = false).morning.get
        withClue(s"$number ${fast.getClass.getSimpleName}: ") {
          // Exodus 32:11-14 and 34:1-10 -- the same Torah that is read at
          // Mincha. Matched on the verses: toString renders the book in Hebrew.
          morning.torah.doFind(Custom.Ashkenaz).toString should include("32:11-14")
          // the haftarah at a minor fast belongs to Mincha, not to the morning
          morning.haftarah.doFind(Custom.Ashkenaz) shouldBe None
        }
        checked += 1
    checked should be > 0

  "Tisha BeAv" should "have its own morning reading, with a haftarah" in:
    var checked = 0
    for number <- 5700 to 5900 do
      val day = TishaBeAv.date(Year(number))
      if !day.isShabbos then
        val morning = Schedule.get(day, inHolyLand = false).morning.get
        withClue(s"$number: ") {
          // Deuteronomy 4:25-40, not Vayechal
          morning.torah.doFind(Custom.Ashkenaz).toString should include("4:25-29")
          // and unlike the minor fasts, the morning has a haftarah
          morning.haftarah.doFind(Custom.Ashkenaz)
            .map(_.toString).getOrElse("") should include("8:13-9:23")
        }
        checked += 1
    checked should be > 0

  "a whole year" should "be buildable" in:
    // What #149 asks for: retrieve every day's schedule for a year. Before the
    // fasts were wired in this threw partway through every single year.
    for
      number <- Seq(5787, 5788, 5789, 5790)
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val schedule = Schedule(year, inHolyLand)
      var day: Day = year.firstDay
      var days = 0
      while day.number <= year.lastDay.number do
        schedule.days.contains(day) shouldBe true
        days += 1
        day = day.next
      days should be > 350
