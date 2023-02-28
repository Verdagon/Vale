package dev.vale.instantiating

import dev.vale.instantiating.ast._
import dev.vale.instantiating.RegionCollapser._
import dev.vale.{vassertSome, vimpl, vwat}

import scala.collection.immutable.Map

object RegionCollapser {
  def collapsePrototype(map: Map[Int, Int], prototype: PrototypeI[sI]): PrototypeI[cI] = {
    val PrototypeI(id, returnType) = prototype
    PrototypeI(
      collapseFunctionId(map, id),
      collapseCoord(map, returnType))
  }

  def collapseId[T <: INameI[sI], Y <: INameI[cI]](
    map: Map[Int, Int],
    id: IdI[sI, T],
    func: T => Y):
  IdI[cI, Y] = {
    val IdI(packageCoord, initSteps, localName) = id
    IdI(
      packageCoord,
      initSteps.map(x => collapseName(map, x)),
      func(localName))
  }

  def collapseFunctionId(
    map: Map[Int, Int],
    id: IdI[sI, IFunctionNameI[sI]]):
  IdI[cI, IFunctionNameI[cI]] = {
    collapseId[IFunctionNameI[sI], IFunctionNameI[cI]](
      map,
      id,
      x => collapseFunctionName(map, x))
  }

  def collapseFunctionName(
    map: Map[Int, Int],
    name: IFunctionNameI[sI]):
  IFunctionNameI[cI] = {
    name match {
      case FunctionNameIX(FunctionTemplateNameI(humanName, codeLocation), templateArgs, parameters) => {
        FunctionNameIX(
          FunctionTemplateNameI(humanName, codeLocation),
          templateArgs.map(collapseTemplata(map, _)),
          parameters.map(collapseCoord(map, _)))
      }
    }
  }

  def collapseVarName(
    map: Map[Int, Int],
    name: IVarNameI[sI]):
  IVarNameI[cI] = {
    name match {
      case CodeVarNameI(name) => CodeVarNameI(name)
    }
  }

  def collapseName(
    map: Map[Int, Int],
    name: INameI[sI]):
  INameI[cI] = {
    name match {
      case n @ FunctionNameIX(_, _, _) => {
        collapseFunctionName(map, n)
      }
      case other => vimpl(other)
    }
  }

  def collapseTemplata(
    map: Map[Int, Int],
    templata: ITemplataI[sI]):
  ITemplataI[cI] = {
    templata match {
      case CoordTemplataI(coord) => CoordTemplataI(collapseCoord(map, coord))
      case KindTemplataI(kind) => KindTemplataI(collapseKind(map, kind))
      case RegionTemplataI(oldPureHeight) => {
        RegionTemplataI[cI](vassertSome(map.get(oldPureHeight)))
      }
      case other => vimpl(other)
    }
  }

  def collapseCoord(
    map: Map[Int, Int],
    coord: CoordI[sI]):
  CoordI[cI] = {
    val CoordI(ownership, kind) = coord
    CoordI(ownership, collapseKind(map, kind))
  }

  def collapseKind(
    map: Map[Int, Int],
    kind: KindIT[sI]):
  KindIT[cI] = {
    kind match {
      case IntIT(x) => IntIT(x)
      case StructIT(id) => StructIT(collapseStructId(map, id))
    }
  }

  def collapseRuntimeSizedArray(
    map: Map[Int, Int],
    rsa: RuntimeSizedArrayIT[sI]):
  RuntimeSizedArrayIT[cI] = {
    vimpl()
  }

  def collapseStaticSizedArray(
    map: Map[Int, Int],
    rsa: StaticSizedArrayIT[sI]):
  StaticSizedArrayIT[cI] = {
    vimpl()
  }

  def collapseStructId(
    map: Map[Int, Int],
    structId: IdI[sI, IStructNameI[sI]]):
  IdI[cI, IStructNameI[cI]] = {
    collapseId[IStructNameI[sI], IStructNameI[cI]](
      map,
      structId,
      { case StructNameI(template, templateArgs) =>
        StructNameI(
          collapseStructTemplateName(map, template),
          templateArgs.map(collapseTemplata(map, _)))
      })
  }

  def collapseStructTemplateName(
    map: Map[Int, Int],
    structName: IStructTemplateNameI[sI]):
  IStructTemplateNameI[cI] = {
    structName match {
      case StructTemplateNameI(humanName) => StructTemplateNameI(humanName)
    }
  }

  def collapseImplId(
    map: Map[Int, Int],
    structId: IdI[sI, IImplNameI[sI]]):
  IdI[cI, IImplNameI[cI]] = {
    collapseId[IImplNameI[sI], IImplNameI[cI]](
      map,
      structId,
      { case ImplNameI(template, templateArgs, subCitizen) =>
        ImplNameI[cI](
          collapseImplTemplateName(map, template),
          templateArgs.map(collapseTemplata(map, _)),
          vimpl(subCitizen))
      })
  }

  def collapseImplTemplateName(
    map: Map[Int, Int],
    structName: IImplTemplateNameI[sI]):
  IImplTemplateNameI[cI] = {
    structName match {
      case ImplTemplateNameI(humanName) => ImplTemplateNameI(humanName)
    }
  }

}
