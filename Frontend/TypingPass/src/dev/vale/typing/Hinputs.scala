package dev.vale.typing

import dev.vale.typing.ast.{EdgeT, FunctionExportT, FunctionExternT, FunctionT, InterfaceEdgeBlueprint, KindExportT, KindExternT, PrototypeT, SignatureT}
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, IFunctionNameT, LambdaCitizenNameT}
import dev.vale.typing.templata.simpleName
import dev.vale.typing.types._
import dev.vale.{StrI, vassertOne, vassertSome, vcurious, vfail, vimpl}
import dev.vale.typing.ast._
import dev.vale.typing.names._
import dev.vale.typing.types._

case class Hinputs(
  interfaces: Vector[InterfaceDefinitionT],
  structs: Vector[StructDefinitionT],
//  emptyPackStructRef: StructTT,
  functions: Vector[FunctionT],
//  immKindToDestructor: Map[KindT, PrototypeT],

  // The typing pass keys this by placeholdered name, and the monomorphizer keys this by non-placeholdered names
  interfaceToEdgeBlueprints: Map[FullNameT[IInterfaceNameT], InterfaceEdgeBlueprint],
  edges: Vector[EdgeT],
  kindExports: Vector[KindExportT],
  functionExports: Vector[FunctionExportT],
  kindExterns: Vector[KindExternT],
  functionExterns: Vector[FunctionExternT]) {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vfail() // Would need a really good reason to hash something this big

  def lookupStruct(structFullName: FullNameT[IStructNameT]): StructDefinitionT = {
    vassertSome(structs.find(_.instantiatedCitizen.fullName == structFullName))
  }

  def lookupInterface(interfaceFullName: FullNameT[IInterfaceNameT]): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.instantiatedCitizen.fullName == interfaceFullName))
  }

  def lookupStructByTemplateFullName(structTemplateFullName: FullNameT[IStructTemplateNameT]): StructDefinitionT = {
    vassertSome(structs.find(_.templateName == structTemplateFullName))
  }

  def lookupInterfaceByTemplateFullName(interfaceTemplateFullName: FullNameT[IInterfaceTemplateNameT]): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.templateName == interfaceTemplateFullName))
  }

  def lookupStructByTemplateName(structTemplateName: StructTemplateNameT): StructDefinitionT = {
    vassertOne(structs.filter(_.templateName.last == structTemplateName))
  }

  def lookupInterfaceByTemplateName(interfaceTemplateName: InterfaceTemplateNameT): InterfaceDefinitionT = {
    vassertSome(interfaces.find(_.templateName.last == interfaceTemplateName))
  }

  def lookupFunction(signature2: SignatureT): Option[FunctionT] = {
    functions.find(_.header.toSignature == signature2).headOption
  }

  def lookupFunction(funcTemplateName: IFunctionTemplateNameT): Option[FunctionT] = {
    functions.find(_.header.fullName.last.template == funcTemplateName).headOption
  }

  def lookupFunction(humanName: String): FunctionT = {
    val matches = functions.filter(f => {
      f.header.fullName.last match {
        case FunctionNameT(n, _, _) if n.humanName.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Function \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupStruct(humanName: String): StructDefinitionT = {
    val matches = structs.filter(s => {
      s.templateName.last match {
        case StructTemplateNameT(n) if n.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Struct \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupImpl(
    structTT: FullNameT[IStructTemplateNameT],
    interfaceTT: FullNameT[IInterfaceTemplateNameT]):
  EdgeT = {
    vassertSome(
      edges.find(impl => {
      TemplataCompiler.getCitizenTemplate(impl.struct) == structTT &&
        TemplataCompiler.getInterfaceTemplate(impl.interface) == interfaceTT
    }))
  }

  def lookupInterface(humanName: String): InterfaceDefinitionT = {
    val matches = interfaces.filter(s => {
      s.templateName.last match {
        case InterfaceTemplateNameT(n) if n.str == humanName => true
        case _ => false
      }
    })
    if (matches.size == 0) {
      vfail("Interface \"" + humanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def lookupUserFunction(humanName: String): FunctionT = {
    val matches =
      functions
        .filter(function => simpleName.unapply(function.header.fullName).contains(humanName))
        .filter(_.header.isUserFunction)
    if (matches.size == 0) {
      vfail("Not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def nameIsLambdaIn(name: FullNameT[IFunctionNameT], needleFunctionHumanName: String): Boolean = {
    val lastThree = name.steps.slice(name.steps.size - 3, name.steps.size)
    println(lastThree)
    lastThree match {
      case Vector(
        FunctionNameT(FunctionTemplateNameT(StrI(hayFunctionHumanName), _), _, _),
        LambdaCitizenTemplateNameT(_),
        FunctionNameT(FunctionTemplateNameT(StrI("__call"), _), _, _)) if hayFunctionHumanName == needleFunctionHumanName => true
      case _ => false
    }
  }

  def lookupLambdaIn(needleFunctionHumanName: String): FunctionT = {
    val matches = functions.filter(f => nameIsLambdaIn(f.header.fullName, needleFunctionHumanName))
    if (matches.size == 0) {
      vfail("Lambda for \"" + needleFunctionHumanName + "\" not found!")
    } else if (matches.size > 1) {
      vfail("Multiple found!")
    }
    matches.head
  }

  def getAllNonExternFunctions: Iterable[FunctionT] = {
    functions.filter(!_.header.isExtern)
  }

  def getAllUserFunctions: Iterable[FunctionT] = {
    functions.filter(_.header.isUserFunction)
  }
}
