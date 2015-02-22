package org.salgo.common


trait Default[T] {
  def value: T
}

trait ImplicitsForDefaults { this: Default.type =>
  implicit def forAnyRef[T](implicit ev: Null <:< T): Default[T] = Default withValue ev(null)
}

object Default extends ImplicitsForDefaults {
  def withValue[T](v: T) = new Default[T] { def value = v }

  implicit val forBoolean = Default withValue false
  implicit val forChar = Default withValue false
  implicit def forNumeric[T](implicit n: Numeric[T]) : Default[T] = Default withValue n.zero
  implicit val forString = Default withValue ""
  implicit def forOption[T]: Default[Option[T]] = Default withValue (None: Option[T])
}
