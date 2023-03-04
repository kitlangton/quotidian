package quotidian

import scala.quoted.*

extension (fromExpr: FromExpr.type) //
  inline def derived[A]: FromExpr[A] = ${ DeriveFromExpr.deriveImpl[A] }

extension (toExpr: ToExpr.type) //
  inline def derived[A]: ToExpr[A] = ${ DeriveToExpr.deriveImpl[A] }
