package dev.vale.typing.env

import dev.vale.highertyping.{FunctionA, ImplA, InterfaceA, StructA}
import dev.vale.typing.templata.ITemplataT
import dev.vale.{vpass, vwat}
import dev.vale.postparsing.ITemplataType
import dev.vale.typing.templata.IContainer
import dev.vale.typing.types.InterfaceTT
import dev.vale.vpass

sealed trait IEnvEntry {
  def expectTemplataEnvEntry(): TemplataEnvEntry = {
    this match {
      case t @ TemplataEnvEntry(templata) => t
      case other => vwat(other)
    }
  }
}
// We dont have the unevaluatedContainers in here because see TMRE
case class FunctionEnvEntry(function: FunctionA) extends IEnvEntry {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
case class ImplEnvEntry(impl: ImplA) extends IEnvEntry { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class StructEnvEntry(struct: StructA) extends IEnvEntry { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class InterfaceEnvEntry(interface: InterfaceA) extends IEnvEntry { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class TemplataEnvEntry(templata: ITemplataT[ITemplataType]) extends IEnvEntry {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()
}
