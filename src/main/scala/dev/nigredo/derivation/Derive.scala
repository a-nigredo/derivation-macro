package dev.nigredo.derivation

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

/**
  * Derivation configuration
  *
  * @param ident         used for grouping new derivation if target of annotation has nested case class
  * @param className     new derivation class name
  * @param excludeFields define which fields have to be excluded from new derivation
  * @param rename        define how to rename fields during derivation
  */
case class Def(ident: String, className: String, excludeFields: List[String] = Nil, rename: List[(String, String)] = Nil)

/**
  * Macro creates new structure in companion object for @see dev.nigredo.derivation.Derivation according to configuration.
  *
  * {{{
  *     @Derive(Def("Bar", "MyNewCaseClass"))
  *     case class Foo(value: Int)
  *
  *     object Foo {
  *       object Bar  {
  *           case class MyNewCaseClass(value: Int)
  *       }
  *     }
  *
  *     @Derive(Def("Bar", "MyNewCaseClass", List("value"))
  *     case class Foo(value: Int, value2: String)
  *
  *     object Foo {
  *       object Bar {
  *         case class MyNewCaseClass(value2: String)
  *       }
  *     }
  *
  *     @Derive(Def("Bar", "MyNewCaseClass", List("value"), List("value2" -> "renamedValue2"))
  *     case class Foo(value: Int, value2: String)
  *
  *     object Foo {
  *       object Bar {
  *         case class MyNewCaseClass(renamedValue2: String)
  *       }
  *     }
  * }}}
  *
  * If macro is applied to sealed trait then trait SealedDerivation will be created in companion object as a phantom type.
  *
  * {{{
  *   @Derive("Bar", "MyDerivedClass")
  *   sealed trait Foo
  *
  *   object Foo {
  *     object Bar {
  *       sealed trait SealedDerivation
  *     }
  *   }
  * }}}
  *
  * Value classes will be simplified during application
  *
  * {{{
  *     case class FooAnyVal(value: String) extends AnyVal
  *
  *     @Derive(Def("Bar", "MyNewCaseClass"))
  *     case class Foo(value: FooAnyVal)
  *
  *     object Foo {
  *       object Bar  {
  *           case class MyNewCaseClass(value: String)
  *       }
  *     }
  * }}}
  *
  * Restriction: All data structure has to be define in top level.
  *
  * @param config derivation config
  */
class Derive(config: Def*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Derive.impl
}

object Derive {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def isPrimitive(clazz: String): Boolean = {
      val typeName = TypeName(clazz)
      (c.universe.definitions.StringClass.name == typeName
        || c.universe.definitions.ScalaPrimitiveValueClasses.seq.exists(_.name == typeName))
    }

    def getClassSymbol(tree: Tree): ClassSymbol =
      c.typecheck(tq"${c.untypecheck(tree.duplicate)}", c.TYPEmode, silent = false).tpe.typeSymbol.asClass

    def getDerivationName(sym: ClassSymbol, conf: Def): c.universe.Tree = {
      val default = tq"${TermName(conf.ident)}.${sym.typeSignature.typeSymbol.name.toTypeName}"
      Store.get(sym.typeSignature.typeSymbol.name.toString) match {
        case Some(y) => y.collectFirst {
          case x if x.ident == conf.ident =>
            tq"${sym.typeSignature.typeSymbol.name.toTermName}.${TermName(conf.ident)}.${TypeName(x.className)}"
        }.getOrElse(default)
        case None => default
      }
    }

    def replaceField(name: String, conf: Def) = TermName(conf.rename.collectFirst {
      case x if x._1 == name => x._2
    }.getOrElse(name))


    def deriveTypeFromSymbol(sym: ClassSymbol, conf: Def): Tree = {
      if (sym.isDerivedValueClass) {
        sym.typeSignature.decls.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m.typeSignature match {
            case method: MethodType if method.params.nonEmpty =>
              tq"${m.paramLists.head.head.typeSignature.typeSymbol.name.toTypeName}"
          }
        }.get
      }
      else if (sym.isCaseClass) tq"${getDerivationName(sym, conf)}"
      else if (sym.isTrait && sym.isSealed)
        tq"${sym.typeSignature.typeSymbol.name.toTermName}.${TermName(conf.ident)}.${TypeName(sealedDerivationName)}"
      else if (isPrimitive(sym.name.encodedName.toString)) tq"${sym.typeSignature.typeSymbol.name.toTypeName}"
      else {
        c.abort(c.enclosingPosition, s"Unsupported type ${sym.fullName}. Support: Value classes, Case clasess and primitives")
      }
    }

    def deriveType(tree: Tree): c.universe.Type =
      c.typecheck(tq"${c.untypecheck(tree.duplicate)}", c.TYPEmode, silent = false).tpe

    val packageNameAppender = new Transformer {
      override def transform(tree: c.universe.Tree): c.universe.Tree = {
        super.transform(tree match {
          case Ident(TermName(`configClass`)) =>
            packageName.split("\\.").map(TermName(_)).foldLeft(q"") {
              case (acc, v) => if (acc.isEmpty) q"$v" else q"$acc.$v"
            }
          case x => x
        })
      }
    }

    val config = c.prefix.tree.duplicate match {
      case q"new $annot(..$values)" =>
        values.map(x =>
          c.eval[Def](c.Expr(packageNameAppender.transform(c.untypecheck(x.asInstanceOf[Tree].duplicate))))
        )
      case _ => c.abort(c.enclosingPosition, "Incorrect annotation params pattern")
    }

    def gen(tree: Seq[Tree]): Tree = {
      tree match {
        case clazz@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" :: tail =>
          Store.params += (tpname.asInstanceOf[TypeName].encodedName.toString -> config)
          val finalTree = config.map { conf =>
            val isNotExcluded: PartialFunction[Trees#Tree, Boolean] = {
              case x: ValDef => !conf.excludeFields.contains(x.name.encodedName.toString)
              case _ => true
            }
            val values = Seq(paramss.flatMap(_.filter(isNotExcluded).collect {
              case x: ValDef =>
                val fTpe = deriveType(x.tpt)
                val newType = if (fTpe.<:<(typeOf[Option[Any]]) || fTpe.<:<(typeOf[Seq[Any]])) {
                  tq"${fTpe.typeSymbol.name.toTypeName}[${
                    x.tpt match {
                      case tq"$tpt[..$tpts]" => deriveTypeFromSymbol(getClassSymbol(tpts.head.duplicate), conf)
                    }
                  }]"
                } else deriveTypeFromSymbol(fTpe.typeSymbol.asClass, conf)
                Seq(q"val ${replaceField(x.name.encodedName.toString, conf)}: $newType")
              case x => Seq(x)
            }).flatten)
            val tNameStr = tpname.asInstanceOf[TypeName].encodedName.toString
            if (values.flatten.isEmpty) {
              c.warning(c.enclosingPosition, s"After fields exclusion '${conf.excludeFields.mkString(",")}' in class '$tNameStr' derivation '${conf.className}' will be an empty case class. Maybe it is not what you want")
            }
            q"""object ${TermName(conf.ident)} {
                 ${q"case class ${TypeName(conf.className)}(...$values)"}
                };"""
          }
          val companion = tail match {
            case q"object $obj extends ..$bases { ..$body }" :: _ => q"object $obj extends ..$bases {..$body;..$finalTree}"
            case _ => q"object ${TermName(tpname.toString)}{..$finalTree}"
          }
          q"${clazz.head};$companion"
        case tr@q"${mods: Modifiers} trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" :: tail if mods.hasFlag(Flag.SEALED) =>
          Store.params += (tpname.asInstanceOf[TypeName].encodedName.toString -> config)
          val finalTree = config.map { conf =>
            q"""object ${TermName(conf.ident)} {
                 sealed trait ${TypeName(sealedDerivationName)}
                };"""
          }
          val companion = tail match {
            case q"object $obj extends ..$bases { ..$body }" :: _ => q"object $obj extends ..$bases {..$body;..$finalTree}"
            case _ => q"object ${TermName(tpname.toString)}{..$finalTree}"
          }
          q"${tr.head};$companion"
        case _ => c.abort(c.enclosingPosition, "@Derivation can be applied to case class or sealed trait")
      }
    }

    annottees.map(_.tree) match {
      case x =>
        val code = gen(x)
        if (System.getProperty("derivation.macro.debug", "").nonEmpty) {
          c.echo(c.enclosingPosition, s"Derivation annotation macro debug info: ${showCode(code)}")
        }
        c.Expr[Any](code)
    }
  }
}


