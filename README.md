# quotidian

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/io.github.kitlangton/quotidian_3.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/io.github.kitlangton/quotidian_3.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/io/github/kitlangton/quotidian_3/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/io/github/kitlangton/quotidian_3/ "Sonatype Releases"

A menagerie of macro utilities and extensions for Scala 3.

```scala
"io.github.kitlangton" %% "quotidian" % "0.0.1"
```

## Overview

Currently, this library supports `FromExpr` and `ToExpr` derivation. It also contains an opinionated set of
extensions and extractors for working with `scala.quoted`; these are brought into scope with `import quotidian.syntax.*`.

- [x] Add `FromExpr` derivation
- [x] Add `ToExpr` derivation
- [ ] Support Parameterized Types
- [ ] Add support for leaving out default values

## Derive `FromExpr` Instances

Writing `FromExpr` instances is a boilerplate-heavy task. Wouldn't it be better if it were _derivable_?

### Before

How boring! How trite! 😭

```scala
import quotidian.*
import scala.quoted.*

final case class Person(name: String, age: Int, pet: Pet)

object Person:
  given FromExpr[Person] with
    def unapply(expr: Expr[Person])(using Quotes): Option[Person] =
      import quotes.reflect.*
      expr match
        case '{ Person(${ Expr(name) }, ${ Expr(age) }, ${ Expr(pet) }) } =>
          Some(Person(name, age, pet))
        case _ => None

  given ToExpr[Person] with
    def apply(person: Person)(using Quotes): Expr[Person] =
      import quotes.reflect.*
      '{ Person(${ Expr(person.name) }, ${ Expr(person.age) }, ${ Expr(person.pet) }) }

final case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person])

object Pet:
  given FromExpr[Pet] with
    def unapply(expr: Expr[Pet])(using Quotes): Option[Pet] =
      import quotes.reflect.*
      expr match
        case '{ Pet(${ Expr(name) }, ${ Expr(hasBone) }, ${ Expr(favoritePerson) }) } =>
          Some(Pet(name, hasBone, favoritePerson))
        case _ => None

  given ToExpr[Pet] with
    def apply(pet: Pet)(using Quotes): Expr[Pet] =
      import quotes.reflect.*
      '{ Pet(${ Expr(pet.name) }, ${ Expr(pet.hasBone) }, ${ Expr(pet.favoritePerson) }) }


enum Fruit:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)

object Fruit:
  given FromExpr[Fruit] with
    def unapply(expr: Expr[Fruit])(using Quotes): Option[Fruit] =
      import quotes.reflect.*
      expr match
        case '{ Fruit.Apple(${ Expr(variety) }) }    => Some(Fruit.Apple(variety))
        case '{ Fruit.Orange(${ Expr(juiciness) }) } => Some(Fruit.Orange(juiciness))
        case '{ Fruit.Banana(${ Expr(isYellow) }) }  => Some(Fruit.Banana(isYellow))
        case _ => None

  given ToExpr[Fruit] with
    def apply(fruit: Fruit)(using Quotes): Expr[Fruit] =
      import quotes.reflect.*
      fruit match
        case Fruit.Apple(variety)    => '{ Fruit.Apple(${ Expr(variety) }) }
        case Fruit.Orange(juiciness) => '{ Fruit.Orange(${ Expr(juiciness) }) }
        case Fruit.Banana(isYellow)  => '{ Fruit.Banana(${ Expr(isYellow) }) }
```

### After

Much better! Much more concise! 😍

```scala
import quotidian.*
import scala.quoted.*

case class Person(name: String, age: Int, pet: Pet) derives FromExpr, ToExpr
case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person]) derives FromExpr, ToExpr

enum Fruit derives FromExpr, ToExpr:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)
```



