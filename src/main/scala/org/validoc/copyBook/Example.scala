package org.validoc.copyBook

class Location(val loc: Int) extends AnyVal
class Colour(val colour: String) extends AnyVal
class Zone(val zone: Int) extends AnyVal
class Status(val status: String) extends AnyVal
class Revenue(val revenue: Int) extends AnyVal
class Time(val i: Int) extends AnyVal

case class StationDetails(stnLoc: Location, acode: String, name: String, colour: Colour, zone: Zone, status: Status, revenue: Revenue, minInterchange: Time, maxInterchange: Time)

case class StationDetails1(@Num(4) stnLoc: Int,
                           @X(3) acode: String,
                           @X(32) name: String,
                           @X(2) colour: String,
                           @Num(3) zone: Int,
                           @X(2) status: String,
                           @Num(5) revenue: Int,
                           @C(4) minInterchange: Int,
                           @C(4) maxInterchange: Int)

object StationDetails {
  import CopyBookReader._
  //       01  STNS-REC.                                                    00002001
  //           05  LOC-LOC                  PIC 9(04).                      00003001
  //           05  LOC-ACODE                PIC X(03).                      00004001
  //           05  LOC-NAME                 PIC X(32).                      00005001
  //           05  LOC-COLOUR               PIC X(02).                      00006001
  //           05  LOC-ZONE                 PIC 9(03).                      00007001
  //           05  LOC-STATUS               PIC X(02).                      00010001
  //           05  LOC-REVENUE              PIC 9(05).                      00020001
  //           05  LOC-MIN-TIME             PIC S9(04) COMP.                00040001
  //           05  LOC-MAX-Time             PIC S9(04) COMP.                00050001

  implicit val locReader = (Num(4)).map(s => new Location(s))
  implicit val colorReader = (X(2)).map(s => new Colour(s))
  implicit val zoneReader = (Num(4)).map(s => new Zone(s))
  implicit val statusReader = (X(2)).map(s => new Status(s))
  implicit val revenueReader = (Num(4)).map(s => new Revenue(s))
  implicit val timeReader = (C(4)).map(s => new Time(s))
  implicit val stationDetailsReader = (
    locReader and X(3) and X(32) and
    colorReader and zoneReader and statusReader and
    revenueReader and timeReader and timeReader)(StationDetails.apply _)
def main(args: Array[String]): Unit = {
  CopyBookReader.print[StationDetails]("some file name")
}
}