package dev.vale.instantiating

import dev.vale.instantiating.ast._
import dev.vale.{vassertSome, vimpl}

// See ICRHRC for why/when we count the regions.
// This one will use one map for the entire deep collapse, rather than making a new map for everything.
object RegionCollapserConsistent {

  def collapsePrototype(map: Map[Int, Int], prototype: PrototypeI[sI]): PrototypeI[nI] = {
    val PrototypeI(id, returnType) = prototype
    PrototypeI(
      collapseFunctionId(map, id),
      collapseCoord(map, returnType))
  }

  def collapseId[T <: INameI[sI], Y <: INameI[nI]](
      map: Map[Int, Int],
      id: IdI[sI, T],
      func: T => Y):
  IdI[nI, Y] = {
    val IdI(packageCoord, initSteps, localName) = id
    IdI(
      packageCoord,
      initSteps.map(x => collapseName(map, x)),
      func(localName))
  }

  def collapseFunctionId(
      map: Map[Int, Int],
      id: IdI[sI, IFunctionNameI[sI]]):
  IdI[nI, IFunctionNameI[nI]] = {
    collapseId[IFunctionNameI[sI], IFunctionNameI[nI]](
      map,
      id,
      x => collapseFunctionName(map, x))
  }

  def collapseFunctionName(
      map: Map[Int, Int],
      name: IFunctionNameI[sI]):
  IFunctionNameI[nI] = {
    name match {
      case n @ FunctionNameIX(FunctionTemplateNameI(humanName, codeLocation), templateArgs, parameters) => {
        val templateC = FunctionTemplateNameI[nI](humanName, codeLocation)
        val templateArgsC = templateArgs.map(collapseTemplata(map, _))
        val paramsC =
          parameters.map(param => {
            collapseCoord(map, param)
          })
        FunctionNameIX[nI](templateC, templateArgsC, paramsC)
      }
      case ExternFunctionNameI(humanName, parameters) => {
        val paramsC =
          parameters.map(param => {
            collapseCoord(map, param)
          })
        ExternFunctionNameI[nI](humanName, paramsC)
      }
    }
  }

  def collapseVarName(
    name: IVarNameI[sI]):
  IVarNameI[sI] = {
    name match {
      case TypingPassBlockResultVarNameI(life) => TypingPassBlockResultVarNameI(life)
      case CodeVarNameI(name) => CodeVarNameI(name)
      case TypingPassTemporaryVarNameI(life) => TypingPassTemporaryVarNameI(life)
      case TypingPassFunctionResultVarNameI() => TypingPassFunctionResultVarNameI()
    }
  }

  def collapseName(
      map: Map[Int, Int],
      name: INameI[sI]):
  INameI[nI] = {
    name match {
      case n @ FunctionNameIX(_, _, _) => {
        collapseFunctionName(map, n)
      }
      case StructTemplateNameI(humanName) => StructTemplateNameI(humanName)
      case other => vimpl(other)
    }
  }

  def collapseCoordTemplata(
      map: Map[Int, Int],
      templata: CoordTemplataI[sI]):
  CoordTemplataI[nI] = {
    val CoordTemplataI(region, coord) = templata
    CoordTemplataI(collapseRegionTemplata(map, region), collapseCoord(map, coord))
  }

  def collapseTemplata(
    map: Map[Int, Int],
    templata: ITemplataI[sI]):
  ITemplataI[nI] = {
    templata match {
      case c @ CoordTemplataI(_, _) => collapseCoordTemplata(map, c)
      case KindTemplataI(kind) => KindTemplataI(collapseKind(map, kind))
      case r @ RegionTemplataI(_) => collapseRegionTemplata(map, r)
      case MutabilityTemplataI(mutability) => MutabilityTemplataI(mutability)
      case other => vimpl(other)
    }
  }

  def collapseRegionTemplata(
    map: Map[Int, Int],
    templata: RegionTemplataI[sI]):
  RegionTemplataI[nI] = {
    val RegionTemplataI(oldPureHeight) = templata
    RegionTemplataI[nI](vassertSome(map.get(oldPureHeight)))
  }

  def collapseCoord(
      map: Map[Int, Int],
      coord: CoordI[sI]):
  CoordI[nI] = {
    val CoordI(ownership, kind) = coord
    CoordI(ownership, collapseKind(map, kind))
  }

  def collapseKind(
      map: Map[Int, Int],
      kind: KindIT[sI]):
  KindIT[nI] = {
    kind match {
      case NeverIT(fromBreak) => NeverIT(fromBreak)
      case VoidIT() => VoidIT()
      case IntIT(x) => IntIT(x)
      case BoolIT() => BoolIT()
      case FloatIT() => FloatIT()
      case StrIT() => StrIT()
      case StructIT(id) => StructIT(collapseStructId(map, id))
      case ssa @ StaticSizedArrayIT(_) => collapseStaticSizedArray(map, ssa)
      case rsa @ RuntimeSizedArrayIT(_) => collapseRuntimeSizedArray(map, rsa)
    }
  }

  def collapseRuntimeSizedArray(
    map: Map[Int, Int],
    rsa: RuntimeSizedArrayIT[sI]):
  RuntimeSizedArrayIT[nI] = {
    val RuntimeSizedArrayIT(ssaId) = rsa
    RuntimeSizedArrayIT(
      collapseId[RuntimeSizedArrayNameI[sI], RuntimeSizedArrayNameI[nI]](
        map,
        ssaId,
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
    map: Map[Int, Int],
    ssa: StaticSizedArrayIT[sI]):
  StaticSizedArrayIT[nI] = {
    val StaticSizedArrayIT(ssaId) = ssa
    StaticSizedArrayIT(
      collapseId[StaticSizedArrayNameI[sI], StaticSizedArrayNameI[nI]](
        map,
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
    map: Map[Int, Int],
    structId: IdI[sI, IStructNameI[sI]]):
  IdI[nI, IStructNameI[nI]] = {
    collapseId[IStructNameI[sI], IStructNameI[nI]](
      map,
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
  IdI[nI, ExportNameI[nI]] = {
    collapseId[ExportNameI[sI], ExportNameI[nI]](
      map,
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
  IdI[nI, ExternNameI[nI]] = {
    collapseId[ExternNameI[sI], ExternNameI[nI]](
      map,
      structId,
      { case ExternNameI(ExternTemplateNameI(codeLoc), templateArg) =>
        ExternNameI(
          ExternTemplateNameI(codeLoc),
          collapseRegionTemplata(map, templateArg))
      })
  }

  def collapseStructTemplateName(
    structName: IStructTemplateNameI[sI]):
  IStructTemplateNameI[nI] = {
    structName match {
      case StructTemplateNameI(humanName) => StructTemplateNameI(humanName)
    }
  }

  def collapseImplId(
    map: Map[Int, Int],
    structId: IdI[sI, IImplNameI[sI]]):
  IdI[nI, IImplNameI[nI]] = {
    vimpl()
    // collapseId[IImplNameI[sI], IImplNameI[iI]](
    //   map,
    //   structId,
    //   { case ImplNameI(template, templateArgs, subCitizen) =>
    //     ImplNameI[sI](
    //       collapseImplTemplateName(map, template),
    //       templateArgs.map(collapseTemplata(map, _)),
    //       vimpl(subCitizen))
    //   })
  }

  def collapseImplTemplateName(
    map: Map[Int, Int],
    structName: IImplTemplateNameI[sI]):
  IImplTemplateNameI[nI] = {
    structName match {
      case ImplTemplateNameI(humanName) => ImplTemplateNameI(humanName)
    }
  }

}
