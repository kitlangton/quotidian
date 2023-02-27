# quotidian

A menagerie of macro utilities and extensions for Scala 3.

## Overview

Currently, this library supports `FromExpr` derivation. It also contains an opinionated set of
extensions and extractors for working with `scala.quoted`; these are brought into scope with `import quotidian.syntax.*`.

- [x] Add `FromExpr` derivation
- [ ] Add `ToExpr` derivation
- [ ] Add support for leaving out default values

## Derive `FromExpr` Instances

Writing `FromExpr` instances is a boilerplate-heavy task. Wouldn't it be better if it were _derivable_?

### Before

How boring! How trite! 😭

```scala
import quotidian.*
import scala.quoted.*

final case class Person(name: String, age: Int, pets: Pet)

object Person:
  given FromExpr[Person] with
    def unapply(expr: Expr[Person])(using Quotes): Option[Person] =
      import quotes.reflect.*
      expr match
        case '{ Person(${ Expr(name) }, ${ Expr(age) }, ${ Expr(pets) }) } =>
          Some(Person(name, age, pets))
        case _ => None

final case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person])

object Pet:
  given FromExpr[Pet] with
    def unapply(expr: Expr[Pet])(using Quotes): Option[Pet] =
      import quotes.reflect.*
      expr match
        case '{ Pet(${ Expr(name) }, ${ Expr(hasBone) }, ${ Expr(favoritePerson) }) } =>
          Some(Pet(name, hasBone, favoritePerson))
        case _ => None

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
```

### After

Much better! Much more concise! 😍

```scala
import quotidian.*
import scala.quoted.FromExpr

case class Person(name: String, age: Int, pets: Pet) derives FromExpr
case class Pet(name: String, hasBone: Boolean, favoritePerson: Option[Person]) derives FromExpr

enum Fruit derives FromExpr:
  case Apple(variety: String)
  case Orange(juiciness: Int)
  case Banana(isYellow: Boolean)
```



