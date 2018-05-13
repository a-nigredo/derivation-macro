## Derivation macro

Macro derives case classes from existing one. 

Consist of 2 parts:
 * @Derive - used for configuration and structure derivation
 * Derivation - used for creating new structure from provided case class instance

## Examples
**Simple application**
```scala
@Derive(Def("Bar", "MyNewCaseClass"))
case class Foo(value: Int)

object Foo {
     object Bar  {
         case class MyNewCaseClass(value: Int)
     }
}
```
```scala
@Derive(Def("Bar", "MyNewCaseClass", List("value")))
case class Foo(value: Int, value2: String)

object Foo {
     object Bar {
       case class MyNewCaseClass(value2: String)
     }
}
```
```scala
@Derive(Def("Bar", "MyNewCaseClass", List("value"), List("value2" -> "renamedValue2")))
case class Foo(value: Int, value2: String)
object Foo {
     object Bar {
       case class MyNewCaseClass(renamedValue2: String)
     }
}
```
```scala
//Value classes will be automatically simplified during application
case class FooAnyVal(value: String) extends AnyVal
  
@Derive(Def("Bar", "MyNewCaseClass"))
case class Foo(value: FooAnyVal)

object Foo {
    object Bar  {
         case class MyNewCaseClass(value: String)
     }
}
```
**Nested application**
```scala
@Derive(Def("Bar", "MyNewCaseClass", List("value")))
case class Foo(value: Foo2)

object Foo {
     object Bar {
       case class MyNewCaseClass(value2: Foo.Bar.MyNewCaseClass)
     }
}

@Derive(Def("Bar", "MyNewCaseClass", List("value")))
case class Foo2(value: Int)
object Foo2 {
     object Bar {
       case class MyNewCaseClass(value2: Int)
     }
}
```

**Sealed trait application**

If macro is applied to sealed trait then trait SealedDerivation will be created and will be used as a phantom type when instance of derivation will be created 
```scala
@Derive("Bar", "MyDerivedClass")
sealed trait Foo

object Foo {
    object Bar {
      sealed trait SealedDerivation
    }
}
```
## Configuration
_dev.nigredo.derivation.Def_ is a configuration new derivation and consist of:
  * **ident**         - used for grouping new derivation if target of the annotation has nested case class
  * **className**     - new derivation class name
  * **excludeFields** - define which fields have to be excluded from new derivation
  * **rename**        - define how to rename fields during derivation

If you have any questions or suggestions just create an issue on github or send me an email to [a.nigredo@gmail.com](a.nigredo@gmail.com)
