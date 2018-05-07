package dev.nigredo.derivation

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Base simple implementation for Derivation.
  * Take derivation name A and case class B and produce derivation A from B
  */
object Derivation {
  def derive[A, B <: Product](value: B): A = macro DerivationImpl.impl[A, B]
}

object DerivationImpl {

  def impl[A: c.WeakTypeTag, B <: Product : c.WeakTypeTag](c: blackbox.Context)(value: c.Expr[B]): c.Expr[A] = {
    import c.universe._

    val typeSymbolA = weakTypeOf[A].typeSymbol

    def getPrimaryCtorParams(sym: ClassSymbol): Seq[List[c.universe.Symbol]] = {
      sym.typeSignature.decls.flatMap {
        case m: MethodSymbol if m.isPrimaryConstructor => m.typeSignature match {
          case method: MethodType if method.params.nonEmpty => m.paramLists
        }
        case _ => Seq.empty
      }
    }.toSeq

    def getConfig(cs: ClassSymbol): Def = {
      val conf = Store.get(cs.name.encodedName.toString).getOrElse(
        c.abort(c.enclosingPosition, s"${cs.fullName} is not annotated by @Derivation annotation.")
      )
      typeSymbolA.fullName.split("\\.").collectFirst {
        case x if conf.exists(_.ident == x) => conf.find(_.ident == x)
      }.flatten.getOrElse(c.abort(c.enclosingPosition, s"Could not find configuration for ${cs.fullName}"))
    }

    def getDerivation(classSymbol: ClassSymbol, values: Seq[Seq[Tree]], phantomType: Option[Tree] = None): c.universe.Tree = {
      val conf = getConfig(classSymbol)
      phantomType match {
        case Some(phantom) => q"new ${tq"${classSymbol.name.toTermName}.${TermName(conf.ident)}.${TypeName(conf.className)}"}(...$values) with $phantom"
        case None => q"new ${tq"${classSymbol.name.toTermName}.${TermName(conf.ident)}.${TypeName(conf.className)}"}(...$values)"
      }
    }

    @tailrec
    def buildPath(path: Seq[TermName], value: c.universe.Tree): c.universe.Tree =
      if (path.isEmpty) value
      else {
        val field = path.head
        if (value.isEmpty) buildPath(path.tail, q"$field")
        else buildPath(path.tail, q"$value.$field")
      }

    def gen(cs: ClassSymbol, parents: Seq[TermName]): Seq[Seq[c.universe.Tree]] = {

      val conf = getConfig(cs)

      def rename(termName: String) = conf.rename.find(_._1 == termName).map(_._2).getOrElse(termName)

      def isNotExcluded(sym: Symbol, conf: Def) = !conf.excludeFields.contains(sym.name.encodedName.toString)

      def extractAnyValParam(cs: ClassSymbol): c.universe.Symbol = {
        cs.typeSignature.decls.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m.typeSignature match {
            case method: MethodType if method.params.nonEmpty => m.paramLists.head.head
          }
        }.get
      }

      getPrimaryCtorParams(cs).map(_.filter(x => isNotExcluded(x, conf)).map { sym =>

        val classSymbol = sym.typeSignature.typeSymbol.asClass

        val value = if (classSymbol.isDerivedValueClass) {
          buildPath(parents.:+(sym.name.toTermName).:+(extractAnyValParam(classSymbol).name.toTermName), q"")
        } else if (classSymbol.isCaseClass) {
          val values: Seq[Seq[Tree]] = getPrimaryCtorParams(classSymbol).map(_.filter(x => isNotExcluded(x, conf)).flatMap { s =>
            val classSym = s.typeSignature.typeSymbol.asClass
            if (classSym.isCaseClass)
              Seq(getDerivation(classSym, gen(classSym, parents ++ List(sym.name.toTermName, s.name.encodedName.toTermName))))
            else Seq(buildPath(parents.:+(sym.name.toTermName).:+(s.name.toTermName), q""))
          })
          getDerivation(classSymbol, values)
        } else if (classSymbol.isPrimitive || c.universe.definitions.StringClass.name == classSymbol.name) {
          buildPath(parents.:+(sym.name.toTermName), q"")
        }
        else if (classSymbol.isTrait && classSymbol.isSealed) {
          q"${buildPath(parents.:+(sym.name.toTermName), q"")} match {case ..${
            classSymbol.knownDirectSubclasses.map(x => {
              val name = TermName(c.freshName())
              val phantomType = tq"${classSymbol.name.toTermName}.${TermName(conf.ident)}.${TypeName(sealedDerivationName)}"
              cq"$name: ${x.asType.name.toTypeName} => ${getDerivation(x.asClass, gen(x.asClass, List(name)), Some(phantomType))}"
            })
          }}"
        }
        else if (classSymbol.toType.<:<(typeOf[Option[Any]]) || classSymbol.toType.<:<(typeOf[Seq[Any]])) {
          val funcParamName = TermName(c.freshName())
          val typeArgsSymbol = sym.typeSignature.typeArgs.head.typeSymbol.asClass
          val func =
            if (typeArgsSymbol.isDerivedValueClass) {
              q"_.${extractAnyValParam(typeArgsSymbol).name.toTermName}"
            } else if (typeArgsSymbol.isCaseClass) {
              q"($funcParamName: ${typeArgsSymbol.name.toTypeName}) => ${getDerivation(typeArgsSymbol, gen(typeArgsSymbol, List(funcParamName)))}"
            } else q"identity"
          q"${buildPath(parents.:+(sym.name.toTermName), q"")}.map($func)"
        }
        else {
          c.abort(c.enclosingPosition, s"Unsupported type ${sym.fullName}. Support: Value classes, Case classes, primitives and sealed traits")
        }
        q"${TermName(rename(sym.name.toTermName.encodedName.toString))} = $value"
      })
    }

    val valueResult = TermName(c.freshName())
    val tree = gen(c.weakTypeOf[B].typeSymbol.asClass, Seq(valueResult))
    val code =
      q"""
         val $valueResult = ${value.tree};
         new $typeSymbolA(...$tree)
       """
    if (System.getProperty("derivation.macro.debug", "").nonEmpty) {
      c.echo(c.enclosingPosition, s"Derivation macro debug info: ${showCode(code)}")
    }
    c.Expr[A](code)
  }
}