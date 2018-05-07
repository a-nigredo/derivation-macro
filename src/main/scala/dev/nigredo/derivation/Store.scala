package dev.nigredo.derivation

import scala.collection.mutable

/**
  * Store annotation params between macros
  */
object Store {

  lazy val params: mutable.Map[String, Seq[Def]] = mutable.Map()

  def get(k: String): Option[Seq[Def]] = params.get(k)
}
