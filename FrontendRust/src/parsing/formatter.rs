// From Frontend/ParsingPass/src/dev/vale/parsing/Formatter.scala
// Code formatting utilities

/*
package dev.vale.parsing

import dev.vale.vcurious
import dev.vale.vimpl

object Formatter {
*/

pub enum IClass {
    W,
    Ab,
    Ext,
    Fn,
    FnName,
    FnTplSep,
    Rune,
}
/*
  sealed trait IClass
  case object W extends IClass
  case object Ab extends IClass
  case object Ext extends IClass
  case object Fn extends IClass
  case object FnName extends IClass
  case object FnTplSep extends IClass
  case object Rune extends IClass
*/

pub enum IElement {
    Span(Span),
    Text(Text),
}
/*
  sealed trait IElement
*/

pub struct Span {
    pub classs: IClass,
    pub elements: Vec<IElement>,
}
/*
  object Span {
    def apply(classs: IClass, elements: IElement*): Span = {
      Span(classs, elements.toVector)
    }
  }
  case class Span(classs: IClass, elements: Vector[IElement]) extends IElement { override def equals(obj: Any): Boolean = vcurious();
override def hashCode(): Int = vcurious(); }
*/

pub struct Text {
    pub string: String,
}
/*
  case class Text(string: String) extends IElement { override def equals(obj: Any): Boolean = vcurious();
override def hashCode(): Int = vcurious(); }
}
*/
