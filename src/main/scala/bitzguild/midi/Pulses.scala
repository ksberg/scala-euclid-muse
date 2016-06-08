/*
 * Copyright (C) 2014 Sony Mobile Communications AB.
 * All rights, including trade secret rights, reserved.
 */

package bitzguild.midi

import scala.collection.mutable.ListBuffer

/**
 * Utility class that holds a sequence of on/off states. This can be used
 * as the foundation to create beat patterns or drive a percussion loop.
 *
 * @param pattern boolean array
 */
class Pulses(protected val pattern: Array[Boolean]) {
  def this(ints: List[Int]) { this(ints.map(_ == 1).toArray) }

  def toArray = pattern

  def printXd(f: Boolean) : Char = if (f) 'X' else '.'
  def print10(f: Boolean) : Char = if (f) '1' else '0'
  def printDD(f: Boolean) : Char = if (f) '-' else '.'
  def print1o(f: Boolean) : Char = if (f) '1' else 'o'

  override def toString = pattern.map(printXd).mkString

  def shift(a: Int) : Pulses =
    if (a == 0) this
    else if (a > 0) shiftRight(a)
    else shiftLeft(Math.abs(a))

  def shiftRight(o: Int) = new Pulses((pattern ++ pattern).dropRight(o % pattern.length).takeRight(pattern.length))
  def shiftLeft(o: Int) = {
    val len = pattern.length
    val ii = o % len
    new Pulses((pattern ++ pattern).slice(ii, ii + len))
  }

  def unary_! = new Pulses(pattern.map(!_))

  def +(that: Pulses) = new Pulses(this.pattern ++ that.pattern)
  def -(that: Pulses) = new Pulses(this.pattern.zip(align(that).pattern).map(x => if (x._2) false else x._1))
  def *(n: Int) = new Pulses((1 until n).foldLeft(pattern)((p,i) => p ++ pattern))
  def &(that: Pulses) = new Pulses(this.pattern.zip(that.pattern).map(x => x._1 && x._2))
  def |(that: Pulses) = new Pulses(this.pattern.zip(that.pattern).map(x => x._1 || x._2))
  def ^(that: Pulses) = new Pulses(this.pattern.zip(that.pattern).map(x => x._1 ^ x._2))
  def <<(n: Int) = shiftLeft(n)
  def >>(n: Int) = shiftRight(n)

  def :+(b: Boolean) = new Pulses(this.pattern :+ b)

  /**
   * Answer the positive pulses following a false/true sequence, or
   * true at start of the sequence. For example:
   *
   * XXX... => .XX...
   * .XX.XX => ..X..X
   * .XXXXX => ..XXXX
   *
   * @return
   */
  def secondaries = {
    val pl = pattern.toList
    new Pulses((false :: pl.zip(pl.tail).map(x => x._1 && x._1 == x._2)).toArray)
  }

  /**
   * Answer new pattern that matches size of existing pattern
   *
   * @param that other Pulses pattern
   * @param repeat whether to fill with repeated pattern if necessary (alternative is to blank fill)
   * @return Pulses
   */
  def align(that: Pulses, repeat: Boolean = true) : Pulses =
    if (this.pattern.length == that.pattern.length) that
    else if (this.pattern.length < that.pattern.length) new Pulses(that.pattern.take(this.pattern.length))
    else if (repeat) new Pulses((that * ((this.pattern.length / that.pattern.length)+1)).pattern.take(pattern.length))
    else new Pulses(pattern ++ Array.fill(this.pattern.length - that.pattern.length)(false))

  /**
   * Rotate pattern so that 1st pulse is positive/on.
   *
   * @return
   */
  def downbeat = shiftLeft(pattern.takeWhile(!_).length)

  /**
   * Truncate or extend pattern to the given length
   *
   * @param n length
   * @return Pulses
   */
  def clip(n: Int) =
    if (pattern.length > n) new Pulses(pattern.take(n))
    else new Pulses(pattern ++ Array.fill(n - pattern.length)(false))

}

object Pulses {

  /**
   * Generate a pattern based on binary integer representation. For your amusement.
   *
   * @param n Int
   * @return Pulses
   */
  def binary(n: Int) = new Pulses(n.toBinaryString.map(x => if(x == '1') true else false).toArray)


  /**
   * Generate a simple tuple with pulse on the 1st position only.
   *
   * @param steps
   * @return Pulses
   */
  def tuple(steps: Int) = new Pulses(List(1) ++ List.fill(Math.max(0,steps-1))(0))


  /**
   * Generate a series of pulses that conform to Bjorklund/Euclid Rhythm patterns.
   * Adaptation of classic algorithm, using internal mutable state. Pattern is returned
   * in Pulse object which supports easy combination and mutation.
   *
   * @param steps number of pattern steps
   * @param pulses number of positive (on) steps distributed within pattern
   * @param offset wrapping pattern shift left or right
   * @return Pulses
   */
  def bjorklund(steps: Int, pulses: Int, offset: Int = 0) : Pulses =
    if (pulses >= steps || steps < 2 || pulses == 0) new Pulses(List.fill(Math.max(1, steps))(Math.max(0, pulses)))
    else {
      def build(counts: Seq[Int], remainders: Seq[Int]) = {
        val pattern = new ListBuffer[Int]()
        def inner_build(level: Int): Unit =
          if (level == -1) pattern += 0
          else if (level == -2) pattern += 1
          else {
            1 to counts(level) foreach { x => inner_build(level - 1) }
            if (remainders(level) != 0) inner_build(level - 2)
          }
        inner_build(counts.size - 1)
        pattern
      }

      val remainders = new ListBuffer[Int]()
      val counts = new ListBuffer[Int]()
      var divisor = steps - pulses

      remainders += pulses
      var level = 0
      do {
        counts += (divisor / remainders(level))
        remainders += (divisor % remainders(level))
        divisor = remainders(level)
        level += 1
      } while (remainders(level) > 1)
      counts += divisor

      new Pulses(build(counts, remainders).toList).shift(offset)
    }

}