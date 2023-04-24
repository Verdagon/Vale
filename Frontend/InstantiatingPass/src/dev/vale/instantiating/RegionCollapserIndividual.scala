package dev.vale.instantiating

import dev.vale.instantiating.ast._
import dev.vale.{vassertSome, vimpl, vwat}

import scala.collection.immutable.Map

// See ICRHRC for why/when we count the regions.
// This one will collapse every node based on only the things it contains.
// It creates a new collapsing map for each one.
object RegionCollapserIndividual {
  def collapsePrototype(prototype: PrototypeI[sI]): PrototypeI[cI] = {
    val PrototypeI(id, returnType) = prototype
    PrototypeI(
      collapseFunctionId(id),
      collapseCoord(returnType))
  }

  def collapseId[T <: INameI[sI], Y <: INameI[cI]](
    id: IdI[sI, T],
    func: T => Y):
  IdI[cI, Y] = {
    val IdI(packageCoord, initSteps, localName) = id
    IdI(
      packageCoord,
      initSteps.map(x => collapseName(x)),
      func(localName))
  }

  def collapseFunctionId(
    id: IdI[sI, IFunctionNameI[sI]]):
  IdI[cI, IFunctionNameI[cI]] = {
    collapseId[IFunctionNameI[sI], IFunctionNameI[cI]](
      id,
      x => collapseFunctionName(x))
  }

  def collapseFunctionName(
    name: IFunctionNameI[sI]):
  IFunctionNameI[cI] = {
    name match {
      case n @ FunctionNameIX(FunctionTemplateNameI(humanName, codeLocation), templateArgs, parameters) => {
        val map = RegionCounter.countFunctionName(n)
        val templateC = FunctionTemplateNameI[cI](humanName, codeLocation)
        val templateArgsC = templateArgs.map(collapseTemplata(map, _))
        val paramsC =
          parameters.map(param => {
            collapseCoord(param)
          })
        FunctionNameIX[cI](templateC, templateArgsC, paramsC)
      }
      case ExternFunctionNameI(humanName, parameters) => {
        val paramsC =
          parameters.map(param => {
            collapseCoord(param)
          })
        ExternFunctionNameI[cI](humanName, paramsC)
      }
    }
  }

  def collapseVarName(
    name: IVarNameI[sI]):
  IVarNameI[cI] = {
    name match {
      case TypingPassBlockResultVarNameI(life) => TypingPassBlockResultVarNameI(life)
      case CodeVarNameI(name) => CodeVarNameI(name)
      case TypingPassTemporaryVarNameI(life) => TypingPassTemporaryVarNameI(life)
      case TypingPassFunctionResultVarNameI() => TypingPassFunctionResultVarNameI()
    }
  }

  def collapseName(
    name: INameI[sI]):
  INameI[cI] = {
    name match {
      case n @ FunctionNameIX(_, _, _) => {
        collapseFunctionName(n)
      }
      case StructTemplateNameI(humanName) => StructTemplateNameI(humanName)
      case other => vimpl(other)
    }
  }

  def collapseCoordTemplata(
      map: Map[Int, Int],
      templata: CoordTemplataI[sI]):
  CoordTemplataI[cI] = {
    val CoordTemplataI(region, coord) = templata
    CoordTemplataI(collapseRegionTemplata(map, region), collapseCoord(coord))
  }

  def collapseTemplata(
    map: Map[Int, Int],
    templata: ITemplataI[sI]):
  ITemplataI[cI] = {
    templata match {
      case c @ CoordTemplataI(_, _) => collapseCoordTemplata(map, c)
      case KindTemplataI(kind) => KindTemplataI(collapseKind(kind))
      case r @ RegionTemplataI(_) => collapseRegionTemplata(map, r)
      case MutabilityTemplataI(mutability) => MutabilityTemplataI(mutability)
      case other => vimpl(other)
    }
  }

  def collapseRegionTemplata(
    map: Map[Int, Int],
    templata: RegionTemplataI[sI]):
  RegionTemplataI[cI] = {
    val RegionTemplataI(oldPureHeight) = templata
    RegionTemplataI[cI](vassertSome(map.get(oldPureHeight)))
  }

  def collapseCoord(
    coord: CoordI[sI]):
  CoordI[cI] = {
    val CoordI(ownership, kind) = coord
    CoordI(ownership, collapseKind(kind))
  }

  def collapseKind(
    kind: KindIT[sI]):
  KindIT[cI] = {
    kind match {
      case NeverIT(fromBreak) => NeverIT(fromBreak)
      case VoidIT() => VoidIT()
      case IntIT(x) => IntIT(x)
      case BoolIT() => BoolIT()
      case FloatIT() => FloatIT()
      case StrIT() => StrIT()
      case StructIT(id) => StructIT(collapseStructId(id))
      case ssa @ StaticSizedArrayIT(_) => collapseStaticSizedArray(ssa)
      case rsa @ RuntimeSizedArrayIT(_) => collapseRuntimeSizedArray(rsa)
    }
  }

  def collapseRuntimeSizedArray(
    rsa: RuntimeSizedArrayIT[sI]):
  RuntimeSizedArrayIT[cI] = {
    val RuntimeSizedArrayIT(rsaId) = rsa
    val map = RegionCounter.countRuntimeSizedArray(rsa)
    RuntimeSizedArrayIT(
      collapseId[RuntimeSizedArrayNameI[sI], RuntimeSizedArrayNameI[cI]](
        rsaId,
        { case RuntimeSizedArrayNameI(RuntimeSizedArrayTemplateNameI(), RawArrayNameI(mutability, elementType, selfRegion)) =>
          RuntimeSizedArrayNameI(
            RuntimeSizedArrayTemplateNameI(),
            RawArrayNameI(
              mutability,
              collapseTemplata(map, elementType).expectCoordTemplata(),
              collapseRegionTemplata(map, selfRegion)))
        }))
  }

  def collapseStaticSizedArray(
    ssa: StaticSizedArrayIT[sI]):
  StaticSizedArrayIT[cI] = {
    val StaticSizedArrayIT(ssaId) = ssa
    val map = RegionCounter.countStaticSizedArray(ssa)
    StaticSizedArrayIT(
      collapseId[StaticSizedArrayNameI[sI], StaticSizedArrayNameI[cI]](
        ssaId,
        { case StaticSizedArrayNameI(StaticSizedArrayTemplateNameI(), size, variability, RawArrayNameI(mutability, elementType, selfRegion)) =>
          StaticSizedArrayNameI(
            StaticSizedArrayTemplateNameI(),
            size,
            variability,
            RawArrayNameI(
              mutability,
              collapseTemplata(map, elementType).expectCoordTemplata(),
              collapseRegionTemplata(map, selfRegion)))
        }))
  }

  def collapseStructId(
    structId: IdI[sI, IStructNameI[sI]]):
  IdI[cI, IStructNameI[cI]] = {
    val map = RegionCounter.countStructId(structId)
    collapseId[IStructNameI[sI], IStructNameI[cI]](
      structId,
      { case StructNameI(template, templateArgs) =>
        StructNameI(
          collapseStructTemplateName(template),
          templateArgs.map(collapseTemplata(map, _)))
      })
  }

  def collapseExportId(
    map: Map[Int, Int],
    structId: IdI[sI, ExportNameI[sI]]):
  IdI[cI, ExportNameI[cI]] = {
    collapseId[ExportNameI[sI], ExportNameI[cI]](
      structId,
      { case ExportNameI(ExportTemplateNameI(codeLoc), templateArg) =>
        ExportNameI(
          ExportTemplateNameI(codeLoc),
          collapseRegionTemplata(map, templateArg))
      })
  }

  def collapseExternId(
    map: Map[Int, Int],
    structId: IdI[sI, ExternNameI[sI]]):
  IdI[cI, ExternNameI[cI]] = {
    collapseId[ExternNameI[sI], ExternNameI[cI]](
      structId,
      { case ExternNameI(ExternTemplateNameI(codeLoc), templateArg) =>
        ExternNameI(
          ExternTemplateNameI(codeLoc),
          collapseRegionTemplata(map, templateArg))
      })
  }

  def collapseStructTemplateName(
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
