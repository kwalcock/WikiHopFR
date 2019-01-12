package org.ml4ai

import java.security.MessageDigest

import scala.language.reflectiveCalls

package object utils {

  def using[A <: { def close() }, B](resource:A)(func: A => B): B = {
    val returnValue:B = func(resource)
    resource.close()
    returnValue
  }

  def md5Hash(s:String):String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }
}
