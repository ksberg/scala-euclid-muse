/*
 * Copyright (C) 2014 Sony Mobile Communications AB.
 * All rights, including trade secret rights, reserved.
 */

package a

import bitzguild.midi.Pulses
import de.sciss.midi._

object MidiFiles {
  def mf(filename: String = "/tmp/mftest.mid") = {
    val ms  = (64 to 72).flatMap { pch => NoteOn(0, pch, 80) :: NoteOff(0, pch, 0) :: Nil }
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 1024)
    val ev  = ms.zipWithIndex.map { case (m, i) => Event((i * 0.25 * rate.value).toLong, m) }
    val mx  = ev.map(_.tick).max
    val t   = Track(ev)
    val sq  = Sequence(Vector(t))
    sq.write(filename)
  }

  def pf(filename: String = "/tmp/pftest.mid") = {
    import bitzguild.midi._

    val pch = 64
    val pulses = Pulses.bjorklund(13,5).toArray.toList
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 1024)

    val ev = pulses.zipWithIndex.flatMap { x =>
      val (p,i) = x
      if (!p) List() else List(Event((i * 0.25 * rate.value).toLong, NoteOn(0, pch, 80)), Event(((i+1) * 0.25 * rate.value).toLong, NoteOff(0, pch, 80)))
    }.toIndexedSeq
    val mx  = ev.map(_.tick).max
    val t   = Track(ev)
    val sq  = Sequence(Vector(t))
    sq.write(filename)
    println(s"WROTE MIDI TO: $filename")
    sq
  }

  def write(p: Pulses, filename: String = "/tmp/pulses.mid") = {
    val pulses = p.toList
    val noteDuration = 0.25 * 0.5
    val pitch = 12

    implicit val rate = TickRate.tempo(bpm = 120, tpq = 1024)
    val sq  = Sequence(Vector(Track(pulses.zipWithIndex.flatMap(x => if (!x._1) List() else onOff(x._2, noteDuration, pitch)).toIndexedSeq)))
    sq.write(filename)
    println(s"WROTE MIDI TO: $filename")
    sq
  }

  def onOff(i: Int, noteDuration: Double = 0.25, pitch: Int = 12, rate: TickRate = TickRate.tempo(bpm = 120, tpq = 1024)) =
    List(Event((i * noteDuration * rate.value).toLong, NoteOn(0, pitch, 80)), Event(((i+1) * noteDuration * rate.value).toLong, NoteOff(0, pitch, 80)))
}
