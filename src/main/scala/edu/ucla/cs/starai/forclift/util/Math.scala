/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.forclift.util

import scala.language.implicitConversions

case class ComplexDouble(val re: Double, val im: Double = 0) {
  private val modulus = math.sqrt(math.pow(re, 2) + math.pow(im, 2))

  def unary_+ = this
  def unary_- = new ComplexDouble(-re, -im)
  def unary_~ = new ComplexDouble(re, -im)
  def unary_! = modulus

  def isNaN = (this.re == Double.NaN || this.im == Double.NaN)
  def isRealNegInfinity = (this.re.isNegInfinity)
  def isRealPosInfinity = (this.re.isPosInfinity)
  def isReal = this.im == 0

  def + (that: ComplexDouble): ComplexDouble = {
    val res = new ComplexDouble(this.re + that.re, this.im + that.im)
    // println(("complex add", this, that, res))
    res
  }
  def - (that: ComplexDouble): ComplexDouble = {
    val res = this + (-that)
    // println(("complex minus", this, that, res))
    res
  }
  def * (that: ComplexDouble): ComplexDouble = {
    val res = new ComplexDouble(
            this.re * that.re - this.im * that.im, 
            this.re * that.im + this.im * that.re
          )
    // println(("complex mul", this, that, res))
    res
  }
  def / (that: ComplexDouble) = {
    require(that.re != 0 || that.im != 0)
    val d = math.pow(that.re, 2) + math.pow(that.im, 2)
    val res = new ComplexDouble(
      (this.re * that.re + this.im * that.im) / d, 
      (this.im * that.re - this.re * that.im) / d
    )
    // println(("complex div", this, that, res))
    res
  }

  def exp: ComplexDouble = {
    val real = math.exp(this.re)
    val res = new ComplexDouble(real * math.cos(this.im), real * math.sin(this.im))
    // println(("complex exp", this, res))
    res
  }
  def log: ComplexDouble = {
    val res = new ComplexDouble(
            math.log(this.modulus), 
            math.atan2(this.im, this.re)
          )
    // println(("complex log", this, res))
    res
  }
  def pow(b: ComplexDouble): ComplexDouble = {
    if (b == ComplexDouble.zero) ComplexDouble.one
    else if (this == ComplexDouble.zero) {
      if (b.im != 0.0 || b.re < 0.0) ComplexDouble.NaN
      else ComplexDouble.zero
    } else {
      val c = this.log * b
      val expReal = math.exp(c.re)
      new ComplexDouble(
        expReal * math.cos(c.im), 
        expReal * math.sin(c.im)
      )
    }
  }

  override def equals(that: Any) = {
    // println((this, that, "equal"))
    that match {
        case that: ComplexDouble => this.re == that.re && this.im == that.im
        case real: Double => this.re == real && this.im == 0
        case real: Int => this.re == real && this.im == 0
        case real: Short => this.re == real && this.im == 0
        case real: Long => this.re == real && this.im == 0
        case real: Float => this.re == real && this.im == 0
        case _ => false
    }
  }

  // ensure hashcode contract is maintained for comparison to non-Complex numbers
  // x ^ 0 is x
  override def hashCode() = re.## ^ im.##

  override def toString() = 
    this match {
      case ComplexDouble.i => "i"
      case ComplexDouble(re, 0) => re.toString
      case ComplexDouble(0, im) => im.toString + "i"
      case _ => asString
    }
  private def asString = 
    re + (if (im < 0) "-" + -im else "+" + im) + "i"

  def abs: Double = modulus
}

object ComplexDouble {
  val zero = new ComplexDouble(0, 0)
  val one = new ComplexDouble(1, 0)
  val i = new ComplexDouble(0, 1)
  val NaN = new ComplexDouble(Double.NaN, Double.NaN)

  implicit def fromOthers[T](d: T)(implicit f: T => Double): ComplexDouble = new ComplexDouble(d, 0)
}

/**
 * This is a value class whose runtime type is Double -- no boxing/unboxing overhead
 */
final class LogDouble(val v: ComplexDouble) extends AnyVal {
  

  @inline def printd(str: String): Unit = if (false) println(str)

  @inline def isZero = {
    printd("iszero")
    v.isRealNegInfinity
  }
  @inline def isNaN = {
    printd("isNaN")
    v.isNaN
  }
  @inline def isInfinite = {
    throw new UnsupportedOperationException
  }
  @inline def isNegInfinite = {
    throw new UnsupportedOperationException
  }
  @inline def isReal = this.isZero || math.sin(v.im) == 0
  
  @inline def compare(that: LogDouble) =  {
    throw new UnsupportedOperationException
  }

  @inline def compareReal(that: LogDouble) = {
    // Real(exp(z)) = exp(re) * math.cos(this.im)
    this.v.re * math.log(math.cos(this.v.im)) compare that.v.re * math.log(math.cos(that.v.im))
  }
  
  // no equals or hashcode definitions allowed (defaults to proxy for v)
  // (see https://docs.google.com/document/d/10TQKgMiJTbVtkdRG53wsLYwWM2MkhtmdV25-NZvLLMA/edit?hl=en_US)
  
  /**
   * http://lingpipe-blog.com/2009/06/25/log-sum-of-exponentials/
   * https://gist.github.com/scalala/Scalala/blob/master/src/main/scala/scalala/library/Numerics.scala
   */
  @inline def + (that: LogDouble): LogDouble = {
    printd(this + " + " + that)
    if (this.v.re < that.v.re) {
      if (this.isZero) that
      else new LogDouble(that.v + (1 + (this.v - that.v).exp).log)
    }else{
      if (that.isZero) this
      else new LogDouble(this.v + (1 + (that.v - this.v).exp).log)
    }
  }

  /**
   * https://gist.github.com/scalala/Scalala/blob/master/src/main/scala/scalala/library/Numerics.scala
   */
  @inline def - (that: LogDouble): LogDouble = {
    printd(this + " - " + that)
    require(this.compareReal(that) >= 0, s"Cannot subtract $that from $this" )
    if (that.isZero) this
    else if (this == that) LogDouble.zero
    else {
      val res = new LogDouble(this.v + (1.0 - (that.v - this.v).exp))
      res
    }
  }
  
  @inline def * (that: LogDouble): LogDouble = {
    printd(this + " * " + that)
    new LogDouble(this.v + that.v)
  }
  @inline def / (that: LogDouble): LogDouble = {
    printd(this + " / " + that)
    new LogDouble(this.v - that.v)
  }
  
  @inline def pow(exp: Int): LogDouble = {
    printd(this + " ** " + exp)
    new LogDouble(this.v * exp)
  }
  @inline def pow(exp: Long): LogDouble = {
    printd(this + " ** " + exp)
    new LogDouble(this.v * exp)
  }
  @inline def pow(exp: Double): LogDouble = {
    printd(this + " ** " + exp)
    new LogDouble(this.v * exp)
  }

  @inline def log: LogDouble = {
    throw new UnsupportedOperationException
    // printd("log(" + this + ")")
    // new LogDouble(this.v.log)
  }
  @inline def logToComplexDouble: ComplexDouble = this.v
  @inline def exp: LogDouble = {
    printd("exp(" + this + ")")
    new LogDouble(this.v.exp)
  }
  @inline def inv: LogDouble = {
    printd("1 / " + this)
    new LogDouble(-this.v)
  }

  @inline override def toString = s"exp($v)"
  @inline def toComplexDouble: ComplexDouble = v.exp
  @inline def toSignDouble: SignLogDouble = new SignLogDouble(true, this)
  
}

object LogDouble{
  
   def apply(c: ComplexDouble) = new LogDouble(c)

   // implicit def doubleToLogDouble(d: Double): LogDouble = {
   //   require(d >= 0) // math.log does not throw an exception
   //   new LogDouble(new ComplexDouble(math.log(d), 0))
   // }
   implicit def complexDoubleToLogDouble[T](c: T)(implicit f: T => ComplexDouble): LogDouble = {
     // NOT negative real
     require(!(c.isReal && c.re < 0))
     val res = new LogDouble(c.log)
     res
   }
   
   // def fromLog(d: Double) = new LogDouble(d)
   def fromLog[T](c: T)(implicit f: T => ComplexDouble): LogDouble = new LogDouble(c)
   
	// not safe! has to be manually
	//   implicit def signLogDoubleToLogDouble(d: SignLogDouble): LogDouble = {
	//     d.toLogDouble
   	//   }
  
   val zero: LogDouble = 0
   val one: LogDouble = 1
   val NaN: LogDouble = ComplexDouble.NaN
   
}


/**
 * Extend LogDouble towards negative real
 * ONLY negative real have pos == false, otherwise pos == true
 */
final class SignLogDouble(_pos: Boolean, val ld: LogDouble) {

  // be careful adding @inline in this class: can cause compiler to crash
  
  // set -0 to 0
  val pos = (_pos || ld.isZero || ld.isNaN)
  def neg = !pos
  
  def isZero = ld.isZero
  def isNaN = ld.isNaN
  def isReal = ld.isReal
  
  override def equals(other: Any) = other match {
 	   case that: SignLogDouble => (this.pos == that.pos) && (this.ld == that.ld)
 	   case _ => false
  }
  
  override def hashCode = pos.hashCode * 47 + ld.hashCode

  def unary_- = new SignLogDouble(neg, ld)
  def abs = new SignLogDouble(true, ld)
  def toLogDouble = {
    require(pos)
    ld
  }

  def + (that: SignLogDouble): SignLogDouble = {
    if (this.isZero) that
    else if (that.isZero) this
    else new SignLogDouble(true, this.ld + that.ld)
    // if (this.isZero) that
    // else if (that.isZero) this
    // else if(this.pos && that.pos){
    //   new SignLogDouble(true, this.ld + that.ld)
    // }else if(this.pos && that.neg){
    //   if(this.ld.compareReal(that.ld) >= 0) new SignLogDouble(true, this.ld - that.ld)
    //   else /*(this.ld < that.ld)*/ new SignLogDouble(false, that.ld - this.ld)
    // }else if(this.neg && that.pos){
    //   if(this.ld.compareReal(that.ld) > 0) new SignLogDouble(false, this.ld - that.ld)
    //   else /*(this.ld <= that.ld)*/ new SignLogDouble(true, that.ld - this.ld)
    // }else /*(this.neg && that.neg)*/ {
    //    new SignLogDouble(false, this.ld + that.ld)
    // }
  }
  def - (that: SignLogDouble): SignLogDouble = {
    if (that.isZero) this
    else new SignLogDouble(true, this.ld - that.ld)
  }
  def * (that: SignLogDouble): SignLogDouble = {
    new SignLogDouble((this.pos == that.pos),(this.ld * that.ld))
  }
  
  def pow(exp: Int): SignLogDouble = {
    require(exp >= 0)
    val newSign = (pos || (exp % 2 == 0)) // positive number or even power
    new SignLogDouble(newSign, ld.pow(exp))
  }
  
  def pow(exp: Long): SignLogDouble = {
    require(exp >= 0)
    val newSign = (pos || (exp % 2 == 0)) // positive number or even power
    new SignLogDouble(newSign,ld.pow(exp))
  }
  
  def logToComplexDouble: ComplexDouble = {
    require(pos, s"Expected a positive number, got $ld")
    ld.logToComplexDouble
  }
  
  def exp: SignLogDouble = {
    if(pos) new SignLogDouble(true, ld.exp) 
    else (new SignLogDouble(true, ld.exp.inv))
  }

  override def toString = (if(pos) "" else "-") + ld
  def toComplexDouble = if(pos) ld.toComplexDouble else -ld.toComplexDouble
  
}

object SignLogDouble{
  def apply(pos: Boolean, ld: LogDouble) = {
    if (!pos && !ld.isReal) {
      throw new UnsupportedOperationException
    }
    new SignLogDouble(pos, ld)
  }
  
  import LogDouble._

  implicit def complexDoubleToSignLogDouble[T](c: T)(implicit f: T => ComplexDouble): SignLogDouble = {
    // ONLY negative real have pos == false
    if (c.isReal && c.re < 0) new SignLogDouble(false, -c)
    else new SignLogDouble(true, c)
  }
  
  implicit def logDoubleToSignLogDouble(ld: LogDouble): SignLogDouble = {
    new SignLogDouble(true, ld)
  }
  
  // def fromLog(d: Double) = new SignLogDouble(true, LogDouble.fromLog(d))
  def fromLog(d: ComplexDouble) = new SignLogDouble(true, LogDouble.fromLog(d))
  
  val zero: SignLogDouble = 0
  val one: SignLogDouble = 1
  val NaN: SignLogDouble = ComplexDouble.NaN
}

object Binomial {

  private[this] val factorialCache = new collection.mutable.ArrayBuffer[LogDouble] ++ List(LogDouble.one, LogDouble.one)

  def factorial(n: Int): LogDouble = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): LogDouble = factorial(n) / factorial(k) / factorial(n - k)

}
