package dev.vale.instantiating

import dev.vale.instantiating.ast._
import dev.vale.options.GlobalOptions
import dev.vale._
import dev.vale.instantiating.RegionCollapser.{collapseCoord, collapseRuntimeSizedArray, collapseStaticSizedArray}
import dev.vale.instantiating.ast.ITemplataI.expectRegionTemplata
import dev.vale.postparsing._
import dev.vale.typing.TemplataCompiler._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplataT._
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.Map
import scala.collection.mutable

case class DenizenBoundToDenizenCallerBoundArgI(
  funcBoundToCallerSuppliedBoundArgFunc: Map[IdT[FunctionBoundNameT], PrototypeI[sI]],
  implBoundToCallerSuppliedBoundArgImpl: Map[IdT[ImplBoundNameT], IdI[sI, IImplNameI[sI]]])

// Note this has mutable stuff in it.
case class NodeEnvironment(
  maybeParent: Option[NodeEnvironment],
//  // We track these because we need to know the original type of the local, see CTOTFIPB.
//  nameToLocal: mutable.HashMap[IVarNameT, ILocalVariableT]
) {

//  def addTranslatedVariable(idT: IVarNameT, translatedLocalVar: ILocalVariableT): Unit = {
//    nameToLocal += (idT -> translatedLocalVar)
//  }
//
//  def lookupOriginalTranslatedVariable(idT: IVarNameT): ILocalVariableT = {
//    vassertSome(nameToLocal.get(idT))
//  }
}
class InstantiatedOutputs() {
  val functions: mutable.HashMap[IdI[cI, IFunctionNameI[cI]], FunctionDefinitionI] =
    mutable.HashMap()
  val structs: mutable.HashMap[IdI[cI, IStructNameI[cI]], StructDefinitionI] = mutable.HashMap()
  val interfacesWithoutMethods: mutable.HashMap[IdI[cI, IInterfaceNameI[cI]], InterfaceDefinitionI] = mutable.HashMap()

  // We can get some recursion if we have a self-referential struct like:
  //   struct Node<T> { value T; next Opt<Node<T>>; }
  // So we need these to short-circuit that nonsense.
  val structToMutability: mutable.HashMap[IdI[cI, IStructNameI[cI]], MutabilityI] = mutable.HashMap()
  val structToBounds: mutable.HashMap[IdI[sI, IStructNameI[sI]], DenizenBoundToDenizenCallerBoundArgI] = mutable.HashMap()
  val interfaceToMutability: mutable.HashMap[IdI[cI, IInterfaceNameI[cI]], MutabilityI] = mutable.HashMap()
  val interfaceToBounds: mutable.HashMap[IdI[sI, IInterfaceNameI[sI]], DenizenBoundToDenizenCallerBoundArgI] = mutable.HashMap()

  //  val immKindToDestructor: mutable.HashMap[KindT, PrototypeT] =
  //    mutable.HashMap[KindT, PrototypeT]()

  // We already know from the hinputs that Some<T> implements Opt<T>.
  // In this map, we'll know that Some<int> implements Opt<int>, Some<bool> implements Opt<bool>, etc.
  val interfaceToImpls: mutable.HashMap[IdI[cI, IInterfaceNameI[cI]], mutable.HashSet[(IdT[IImplNameT], IdI[cI, IImplNameI[cI]])]] =
  mutable.HashMap()
  val interfaceToAbstractFuncToVirtualIndex: mutable.HashMap[IdI[cI, IInterfaceNameI[cI]], mutable.HashMap[PrototypeI[cI], Int]] =
    mutable.HashMap()
  val impls:
    mutable.HashMap[
      IdI[cI, IImplNameI[cI]],
      (ICitizenIT[cI], IdI[cI, IInterfaceNameI[cI]], DenizenBoundToDenizenCallerBoundArgI, DenizenBoundToDenizenCallerBoundArgI)] =
    mutable.HashMap()
  // We already know from the hinputs that Opt<T has drop> has func drop(T).
  // In this map, we'll know that Opt<int> has func drop(int).
  val abstractFuncToInstantiatorAndSuppliedPrototypes: mutable.HashMap[IdI[cI, IFunctionNameI[cI]], (DenizenBoundToDenizenCallerBoundArgI, InstantiationBoundArgumentsI)] =
    mutable.HashMap()
  // This map collects all overrides for every impl. We'll use it to assemble vtables soon.
  val interfaceToImplToAbstractPrototypeToOverride:
    mutable.HashMap[IdI[cI, IInterfaceNameI[cI]], mutable.HashMap[IdI[cI, IImplNameI[cI]], mutable.HashMap[PrototypeI[cI], OverrideI]]] =
    mutable.HashMap()

  // These are new impls and abstract funcs we discover for interfaces.
  // As we discover a new impl or a new abstract func, we'll later need to stamp a lot more overrides either way.
  val newImpls: mutable.Queue[(IdT[IImplNameT], IdI[cI, IImplNameI[cI]], InstantiationBoundArgumentsI)] = mutable.Queue()
  // The int is a virtual index
  val newAbstractFuncs: mutable.Queue[(PrototypeT, PrototypeI[cI], Int, IdI[cI, IInterfaceNameI[cI]], InstantiationBoundArgumentsI)] = mutable.Queue()
  val newFunctions: mutable.Queue[(PrototypeT, PrototypeI[cI], InstantiationBoundArgumentsI, Option[DenizenBoundToDenizenCallerBoundArgI])] = mutable.Queue()

  def addMethodToVTable(
    implId: IdI[cI, IImplNameI[cI]],
    superInterfaceId: IdI[cI, IInterfaceNameI[cI]],
    abstractFuncPrototype: PrototypeI[cI],
    overrride: OverrideI
  ) = {
    val map =
      interfaceToImplToAbstractPrototypeToOverride
        .getOrElseUpdate(superInterfaceId, mutable.HashMap())
        .getOrElseUpdate(implId, mutable.HashMap())
    vassert(!map.contains(abstractFuncPrototype))
    map.put(abstractFuncPrototype, overrride)
  }
}

object Instantiator {
  def translate(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT):
  HinputsI = {
    val monouts = new InstantiatedOutputs()
    val instantiator = new Instantiator(opts, interner, keywords, hinputs, monouts)
    instantiator.translate()
  }
}

class Instantiator(
  opts: GlobalOptions,
  interner: Interner,
  keywords: Keywords,
  hinputs: HinputsT,
  monouts: InstantiatedOutputs) {

  def translate():
  HinputsI = {

    val HinputsT(
    interfacesT,
    structsT,
    functionsT,
    //      oldImmKindToDestructorT,
    interfaceToEdgeBlueprintsT,
    interfaceToSubCitizenToEdgeT,
    instantiationNameToFunctionBoundToRuneT,
    kindExportsT,
    functionExportsT,
    functionExternsT) = hinputs

    val kindExportsI =
      kindExportsT.map({ case KindExportT(range, tyype, placeholderedExportId, exportedName) =>
        //      val packageName = IdT(exportId.packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
        val exportTemplateId = TemplataCompiler.getTemplate(placeholderedExportId)

        val exportId = vimpl()

        val denizenName = vimpl()
        val substitutions = vimpl() //Map(exportId -> assemblePlaceholderMap(hinputs, exportId)),
        val denizenBoundToDenizenCallerSuppliedThing = vimpl()
        val kindIT =
          translateKind(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, vimpl(), tyype)
        val kindCT = RegionCollapser.collapseKind(RegionCounter.countKind(kindIT), kindIT)

        KindExportI(range, kindCT, exportId, exportedName)
      })

    val functionExportsI =
      functionExportsT.map({ case FunctionExportT(range, prototypeT, exportPlaceholderedIdT, exportedName) =>
        //      val packageName = IdT(exportId.packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
        //      val exportName =
        //        packageName.addStep(
        //          interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)), )))

        val perspectiveRegionT =
          exportPlaceholderedIdT.localName.templateArgs.last match {
            case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
              IdT(packageCoord, initSteps, r)
            }
            case _ => vwat()
          }
        val currentPureHeight = 0

        val functionTemplateId = TemplataCompiler.getFunctionTemplate(prototypeT.toSignature.id)
        val functionTemplate =
          vassertSome(hinputs.lookupFunction(functionTemplateId.localName))
        val maybeNearestPureBlockLocation =
          if (functionTemplate.isPure) Some(LocationInDenizen(Vector())) else None


//        val IdT(packageCoord, steps,  = exportPlaceholderedIdT

        val exportIdI =
          translateId[ExportNameT, ExportNameI[sI]](
            exportPlaceholderedIdT,
            { case ExportNameT(ExportTemplateNameT(codeLoc), PlaceholderTemplataT(_, RegionTemplataType())) =>
              ExportNameI(ExportTemplateNameI(codeLoc), RegionTemplataI(0))
            })
        val exportIdC =
          RegionCollapser.collapseId[ExportNameI[sI], ExportNameI[cI]](
            RegionCounter.countExportId(exportIdI),
            exportIdI,
            { case ExportNameI(ExportTemplateNameI(codeLoc), RegionTemplataI(pureHeight)) =>
              ExportNameI(ExportTemplateNameI(codeLoc), RegionTemplataI(pureHeight))
            })

        val exportTemplateIdT = TemplataCompiler.getExportTemplate(exportPlaceholderedIdT)
//        val exportTemplateNameT = exportTemplateIdT.localName
//        val exportTemplateIdI = translateId(exportTemplateIdT, translateExportTemplateName)

        //        val instantiator =
        //          new Instantiator(
        //            opts,
        //            interner,
        //            keywords,
        //            hinputs,
        //            monouts,
        //            exportTemplateIdT,
        //            exportPlaceholderedIdT,
        //            DenizenBoundToDenizenCallerBoundArgI(Map(), Map()))
        val substitutions =
          Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
            exportTemplateIdT -> assemblePlaceholderMap(exportPlaceholderedIdT, exportIdI))
        //            exportTemplateIdT -> instantiator.assemblePlaceholderMap(hinputs, exportPlaceholderedIdT))
//        Collector.all(exportIdI, { case PlaceholderTemplataT(_, _) => vwat() })
        //        val prototype = instantiator.translatePrototype(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, prototypeT)
        val (_, prototypeC) =
          translatePrototype(
            exportPlaceholderedIdT,
            DenizenBoundToDenizenCallerBoundArgI(Map(), Map()),
            substitutions,
            perspectiveRegionT,
            prototypeT)
        Collector.all(prototypeC, { case PlaceholderTemplataT(_, _) => vwat() })
        FunctionExportI(
          range,
          prototypeC,
          exportIdC,
          exportedName)
      })

    val functionExternsI = Vector()
//    val funcExternsI =
//      functionExternsT.map({ case FunctionExternT(_, prototypeT, externName) =>
//        FunctionExternI(
//          translatePrototype(
//
//          )
//        )
//      })

    while ({
      // We make structs and interfaces eagerly as we come across them
      // if (monouts.newStructs.nonEmpty) {
      //   val newStructName = monouts.newStructs.dequeue()
      //   DenizentranslateStructDefinition(opts, interner, keywords, hinputs, monouts, newStructName)
      //   true
      // } else if (monouts.newInterfaces.nonEmpty) {
      //   val (newInterfaceName, calleeRuneToSuppliedPrototype) = monouts.newInterfaces.dequeue()
      //   DenizentranslateInterfaceDefinition(
      //     opts, interner, keywords, hinputs, monouts, newInterfaceName, calleeRuneToSuppliedPrototype)
      //   true
      // } else
      if (monouts.newFunctions.nonEmpty) {
        val (newFuncNameT, newFuncName, instantiationBoundArgs, maybeDenizenBoundToDenizenCallerSuppliedThing) =
          monouts.newFunctions.dequeue()
        translateFunction1(
          opts, interner, keywords, hinputs, monouts, newFuncNameT, newFuncName, instantiationBoundArgs,
          maybeDenizenBoundToDenizenCallerSuppliedThing)
        true
      } else if (monouts.newImpls.nonEmpty) {
        val (implFullNameT, implFullNameI, instantiationBoundsForUnsubstitutedImpl) = monouts.newImpls.dequeue()
        translateImpl(
          opts, interner, keywords, hinputs, monouts, implFullNameT, implFullNameI, instantiationBoundsForUnsubstitutedImpl)
        true
      } else if (monouts.newAbstractFuncs.nonEmpty) {
        val (abstractFuncT, abstractFunc, virtualIndex, interfaceId, instantiationBoundArgs) =
          monouts.newAbstractFuncs.dequeue()
        translateAbstractFunc(
          opts, interner, keywords, hinputs, monouts, interfaceId, abstractFuncT, abstractFunc, virtualIndex, instantiationBoundArgs)
        true
      } else {
        false
      }
    }) {}

    //    interfaceToEdgeBlueprints.foreach({ case (interfacePlaceholderedFullName, edge) =>
    //      val instantiator = new DenizenInstantiator(interner, monouts, interfacePlaceholderedFullName)
    //
    //    })

    val interfaceEdgeBlueprints =
      monouts.interfaceToAbstractFuncToVirtualIndex.map({ case (interface, abstractFuncPrototypes) =>
        interface -> InterfaceEdgeBlueprintI(interface, abstractFuncPrototypes.toVector)
      }).toMap

    val interfaces =
      monouts.interfacesWithoutMethods.values.map(interface => {
        val InterfaceDefinitionI(ref, attributes, weakable, mutability, _, _, _) = interface
        InterfaceDefinitionI(
          ref, attributes, weakable, mutability, Map(), Map(),
          vassertSome(
            monouts.interfaceToAbstractFuncToVirtualIndex.get(ref.id)).toVector)
      })

    val interfaceToSubCitizenToEdge =
      monouts.interfaceToImpls.map({ case (interface, impls) =>
        interface ->
          impls.map({ case (implFullNameT, implFullNameI) =>
            val (subCitizen, parentInterface, _, implInstantiator) = vassertSome(monouts.impls.get(implFullNameI))
            vassert(parentInterface == interface)
            val abstractFuncToVirtualIndex =
              vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interface))
            val abstractFuncPrototypeToOverridePrototype =
              abstractFuncToVirtualIndex.map({ case (abstractFuncPrototype, virtualIndex) =>
                val overrride =
                  vassertSome(
                    vassertSome(
                      vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverride.get(interface))
                        .get(implFullNameI))
                      .get(abstractFuncPrototype))

                vassert(
                  abstractFuncPrototype.id.localName.parameters(virtualIndex).kind !=
                    overrride.overridePrototype.id.localName.parameters(virtualIndex).kind)

                abstractFuncPrototype.id -> overrride
              })
            val edge =
              EdgeI(
                implFullNameI,
                subCitizen,
                interface,
                Map(),
                Map(),
                abstractFuncPrototypeToOverridePrototype.toMap)
            subCitizen.id -> edge
          }).toMap
      }).toMap

    val resultHinputs =
      HinputsI(
        interfaces.toVector,
        monouts.structs.values.toVector,
        monouts.functions.values.toVector,
        //      monouts.immKindToDestructor.toMap,
        interfaceEdgeBlueprints,
        interfaceToSubCitizenToEdge,
//        Map(),
        kindExportsI,
        functionExportsI,
        functionExternsI)

    if (opts.sanityCheck) {
      Collector.all(resultHinputs, {
        case BorrowT => vfail()
        case ShareT => vfail()
      })
    }

    resultHinputs
  }

  def translateId[T <: INameT, Y <: INameI[sI]](idT: IdT[T], func: T => Y): IdI[sI, Y] = {
    val IdT(packageCoord, initStepsT, localNameT) = idT
    IdI[sI, Y](packageCoord, initStepsT.map(translateName(_)), func(localNameT))
  }

  def translateExportName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    exportNameT: ExportNameT):
  ExportNameI[sI] = {
    val ExportNameT(ExportTemplateNameT(codeLoc), region) = exportNameT
    ExportNameI(
      ExportTemplateNameI(codeLoc),
      ITemplataI.expectRegionTemplata(
        translateTemplata(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, region)))
//      translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, region))
  }

  def translateExportTemplateName(exportTemplateNameT: ExportTemplateNameT): ExportTemplateNameI[sI] = {
    val ExportTemplateNameT(codeLoc) = exportTemplateNameT
    ExportTemplateNameI(codeLoc)
  }

  def translateName(t: INameT): INameI[sI] = {
    vimpl()
  }

  def collapseAndTranslateInterfaceDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    interfaceIdT: IdT[IInterfaceNameT],
    interfaceIdI: IdI[sI, IInterfaceNameI[sI]],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
//    val interfaceIdI: IdI[sI, IInterfaceNameI[sI]] = vimpl(interfaceIdC)
    val interfaceTemplateIdT = TemplataCompiler.getInterfaceTemplate(interfaceIdT)

    val interfaceDefT =
      vassertOne(hinputs.interfaces.filter(_.templateName == interfaceTemplateIdT))

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
        interfaceTemplateIdT -> assemblePlaceholderMap(interfaceDefT.instantiatedInterface.id, interfaceIdI))
    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        assembleCalleeDenizenFunctionBounds(
          interfaceDefT.runeToFunctionBound,
          instantiationBoundArgs.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          interfaceDefT.runeToImplBound,
          instantiationBoundArgs.runeToImplBoundArg))
    vcurious(monouts.interfaceToBounds.get(interfaceIdI).isEmpty)
    monouts.interfaceToBounds.put(interfaceIdI, denizenBoundToDenizenCallerSuppliedThing)

    //    val instantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        interfaceTemplateIdT,
//        interfaceIdT,
//        denizenBoundToDenizenCallerSuppliedThing)
    val interfaceIdC = vimpl(interfaceIdI)
    translateInterfaceDefinitionCollapsed(
      interfaceIdT, denizenBoundToDenizenCallerSuppliedThing, substitutions, interfaceIdC, interfaceDefT)
  }

  def assembleCalleeDenizenFunctionBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[FunctionBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeI[sI]]
  ): Map[IdT[FunctionBoundNameT], PrototypeI[sI]] = {
    calleeRuneToSuppliedPrototype.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def assembleCalleeDenizenImplBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[ImplBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedImpl: Map[IRuneS, IdI[sI, IImplNameI[sI]]]
  ): Map[IdT[ImplBoundNameT], IdI[sI, IImplNameI[sI]]] = {
    calleeRuneToSuppliedImpl.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def collapseAndTranslateStructDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    structIdT: IdT[IStructNameT],
    structIdI: IdI[sI, IStructNameI[sI]],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
    if (opts.sanityCheck) {
      vassert(Collector.all(structIdI, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    val structTemplate = TemplataCompiler.getStructTemplate(structIdT)

    val structDefT = findStruct(hinputs, structIdT)

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        assembleCalleeDenizenFunctionBounds(
          structDefT.runeToFunctionBound, instantiationBoundArgs.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          structDefT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg))
    monouts.structToBounds.get(structIdI) match {
      case Some(x) => vcurious(x == denizenBoundToDenizenCallerSuppliedThing)
      case None =>
    }
    monouts.structToBounds.put(structIdI, denizenBoundToDenizenCallerSuppliedThing)

    val topLevelDenizenFullName =
      getTopLevelDenizenId(structIdT)
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
        topLevelDenizenTemplateFullName ->
          assemblePlaceholderMap(
            // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
            // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
            // This struct might not be the top level denizen, such as if it's a lambda.
            // DO NOT SUBMIT might be obsolete
            structDefT.instantiatedCitizen.id,
            structIdI))
//    val instantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        structTemplate,
//        structIdT,
//        denizenBoundToDenizenCallerSuppliedThing)
    val structIdC =
      RegionCollapser.collapseStructId(RegionCounter.countStructId(structIdI), structIdI)
    translateCollapsedStructDefinition(
      structIdT, denizenBoundToDenizenCallerSuppliedThing, substitutions, structIdT, structIdC, structDefT)
  }

  private def findStruct(hinputs: HinputsT, structId: IdT[IStructNameT]) = {
    vassertOne(
      hinputs.structs
        .filter(structT => {
          TemplataCompiler.getSuperTemplate(structT.instantiatedCitizen.id) ==
            TemplataCompiler.getSuperTemplate(structId)
        }))
  }

  private def findInterface(hinputs: HinputsT, interfaceId: IdT[IInterfaceNameT]) = {
    vassertOne(
      hinputs.interfaces
        .filter(interfaceT => {
          TemplataCompiler.getSuperTemplate(interfaceT.instantiatedCitizen.id) ==
            TemplataCompiler.getSuperTemplate(interfaceId)
        }))
  }

  def translateAbstractFunc(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    interfaceIdC: IdI[cI, IInterfaceNameI[cI]],
    abstractFuncT: PrototypeT,
    abstractFuncC: PrototypeI[cI],
    virtualIndex: Int,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
    val interfaceIdI: IdI[sI, IInterfaceNameI[sI]] = vimpl(interfaceIdC)
//    val abstractFuncI: PrototypeI[sI] = vimpl(abstractFuncC)

    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(abstractFuncT.id)

    val funcT =
      vassertOne(
        hinputs.functions.filter(func => {
          TemplataCompiler.getFunctionTemplate(func.header.id) == funcTemplateNameT
        }))

//    val substitutions =
//      Map(
//        funcTemplateNameT ->
//          assemblePlaceholderMap(funcT.header.id, abstractFuncI.id))
    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        assembleCalleeDenizenFunctionBounds(
          funcT.runeToFuncBound, instantiationBoundArgs.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          funcT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg))
//    val abstractFuncInstantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        funcTemplateNameT,
//        abstractFuncT.id,
//        denizenBoundToDenizenCallerSuppliedThing)

    vassert(!monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.contains(abstractFuncC.id))
    monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.put(abstractFuncC.id, (denizenBoundToDenizenCallerSuppliedThing, instantiationBoundArgs))

    val abstractFuncs = vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interfaceIdC))
    vassert(!abstractFuncs.contains(abstractFuncC))
    abstractFuncs.put(abstractFuncC, virtualIndex)

    vassertSome(monouts.interfaceToImpls.get(interfaceIdC)).foreach({ case (implT, impl) =>
      translateOverride(opts, interner, keywords, hinputs, monouts, implT, impl, abstractFuncT, abstractFuncC)
    })
  }

  def translateOverride(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    implIdT: IdT[IImplNameT],
    implIdC: IdI[cI, IImplNameI[cI]],
    abstractFuncPrototypeT: PrototypeT,
    abstractFuncPrototypeI: PrototypeI[cI]):
  Unit = {
    //    val superInterfaceFullName: FullNameT[IInterfaceNameT],

    val implTemplateFullName = TemplataCompiler.getImplTemplate(implIdT)
    val implDefinitionT =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => TemplataCompiler.getImplTemplate(edge.edgeId) == implTemplateFullName))

    val superInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(implDefinitionT.superInterface)
    val superInterfaceDefinitionT = hinputs.lookupInterfaceByTemplateFullName(superInterfaceTemplateFullName)
    val superInterfacePlaceholderedName = superInterfaceDefinitionT.instantiatedInterface
    val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(implDefinitionT.subCitizen.id)
    val subCitizenDefinitionT = hinputs.lookupCitizenByTemplateFullName(subCitizenTemplateFullName)
    val subCitizenPlaceholderedName = subCitizenDefinitionT.instantiatedCitizen

    val abstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(abstractFuncPrototypeT.id)
    val abstractFuncPlaceholderedNameT =
      vassertSome(
        hinputs.functions
          .find(func => TemplataCompiler.getFunctionTemplate(func.header.id) == abstractFuncTemplateName))
        .header.id

    val edgeT =
      vassertSome(
        vassertSome(hinputs.interfaceToSubCitizenToEdge.get(superInterfacePlaceholderedName.id))
          .get(subCitizenPlaceholderedName.id))

    val OverrideT(
    dispatcherFullNameT,
    implPlaceholderToDispatcherPlaceholder,
    implPlaceholderToCasePlaceholder,
    implSubCitizenReachableBoundsToCaseSubCitizenReachableBounds,
    dispatcherRuneToFunctionBound,
    dispatcherRuneToImplBound,
    dispatcherCaseFullNameT,
    overridePrototypeT) =
      vassertSome(edgeT.abstractFuncToOverrideFunc.get(abstractFuncPlaceholderedNameT))

    val (abstractFunctionInstantiator, abstractFunctionRuneToCallerSuppliedInstantiationBoundArgs) =
      vassertSome(monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.get(abstractFuncPrototypeI.id))
    // The dispatcher was originally made from the abstract function, they have the same runes.
    val dispatcherRuneToCallerSuppliedPrototype = abstractFunctionRuneToCallerSuppliedInstantiationBoundArgs.runeToFunctionBoundArg
    val dispatcherRuneToCallerSuppliedImpl = abstractFunctionRuneToCallerSuppliedInstantiationBoundArgs.runeToImplBoundArg

    val edgeDenizenBoundToDenizenCallerBoundArgI =
      vassertSome(monouts.impls.get(implIdC))._4

    val dispatcherPlaceholderFullNameToSuppliedTemplata =
      dispatcherFullNameT.localName.templateArgs
        .map(dispatcherPlaceholderTemplata => {// FullNameT(_, _, PlaceholderNameT(PlaceholderTemplateNameT(index))) =>
          val dispatcherPlaceholderFullName =
            TemplataCompiler.getPlaceholderTemplataId(dispatcherPlaceholderTemplata)
          val implPlaceholder =
            vassertSome(
              implPlaceholderToDispatcherPlaceholder.find(_._2 == dispatcherPlaceholderTemplata))._1
          val IdT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(index, rune))) = implPlaceholder
          val templata = implIdC.localName.templateArgs(index)
          dispatcherPlaceholderFullName -> templata
        })

    val dispatcherFunctionBoundToIncomingPrototype =
      assembleCalleeDenizenFunctionBounds(
        dispatcherRuneToFunctionBound,
        dispatcherRuneToCallerSuppliedPrototype)
    val dispatcherImplBoundToIncomingImpl =
      assembleCalleeDenizenImplBounds(
        dispatcherRuneToImplBound,
        dispatcherRuneToCallerSuppliedImpl)

    val dispatcherTemplateId = TemplataCompiler.getTemplate(dispatcherFullNameT)
    dispatcherPlaceholderFullNameToSuppliedTemplata.map(_._1).foreach(x => vassert(x.initFullName(interner) == dispatcherTemplateId))

//    val dispatcherSubstitutions =
//      Map(dispatcherTemplateId -> dispatcherPlaceholderFullNameToSuppliedTemplata.toMap)
//    val dispatcherInstantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        TemplataCompiler.getFunctionTemplate(dispatcherFullNameT),
//        dispatcherFullNameT,
//        DenizenBoundToDenizenCallerBoundArgI(
//          dispatcherFunctionBoundToIncomingPrototype,
//          dispatcherImplBoundToIncomingImpl))

    // These are the placeholders' templatas that should be visible from inside the dispatcher case.
    // These will be used to call the override properly.

    val dispatcherCasePlaceholderFullNameToSuppliedTemplata =
      dispatcherCaseFullNameT.localName.independentImplTemplateArgs.zipWithIndex.map({
        case (casePlaceholderTemplata, index) => {
          val casePlaceholderFullName =
            TemplataCompiler.getPlaceholderTemplataId(casePlaceholderTemplata)
          val implPlaceholder =
            vassertSome(
              implPlaceholderToCasePlaceholder.find(_._2 == casePlaceholderTemplata))._1
          val IdT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(index, rune))) = implPlaceholder
          val templata = implIdC.localName.templateArgs(index)
          casePlaceholderFullName -> templata
          //          // templata is the value from the edge that's doing the overriding. It comes from the impl.
          //          val dispatcherCasePlaceholderFullName =
          //            dispatcherCaseFullNameT.addStep(interner.intern(PlaceholderNameT(interner.intern(PlaceholderTemplateNameT(index)))))
          //          val templataGivenToCaseFromImpl =
          //            edgetranslateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, templataGivenToCaseFromImplT)
          //          dispatcherCasePlaceholderFullName -> templataGivenToCaseFromImpl
        }
      })

    val edgeDenizenBoundToDenizenCallerSuppliedThing =
      edgeDenizenBoundToDenizenCallerBoundArgI


    // See TIBANFC, we need this map to bring in the impl bound args for the override dispatcher
    // case.
    val caseFunctionBoundToIncomingPrototype =
      dispatcherFunctionBoundToIncomingPrototype ++
        // We're using the supplied prototypes from the impl, but we need to rephrase the keys
        // of this map to be in terms of the override dispatcher function's placeholders, not the
        // original impl's placeholders.
        edgeDenizenBoundToDenizenCallerSuppliedThing.funcBoundToCallerSuppliedBoundArgFunc
          .map({ case (implPlaceholderedBound, implPlaceholderedBoundArg) =>
            vassertSome(implSubCitizenReachableBoundsToCaseSubCitizenReachableBounds.get(implPlaceholderedBound)) -> implPlaceholderedBoundArg
          })
    val caseImplBoundToIncomingImpl =
      dispatcherImplBoundToIncomingImpl ++
        edgeDenizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl
          .map({ case (key, value) => {
            vimpl()
          }})

    val caseDenizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        caseFunctionBoundToIncomingPrototype,
        caseImplBoundToIncomingImpl)

    // we should pull in all the impl's placeholders
    // override should have info: what extra args there are, and what index from the impl full name


    //    val caseRuneToSuppliedFunction =
    //      abstractFunctionRuneToSuppliedFunction



    //    val denizenBoundToDenizenCallerSuppliedThingFromParams =
    //      paramsT.zip(argsM).flatMap({ case (a, x) =>
    //        hoistBoundsFromParameter(hinputs, monouts, a, x)
    //      })
    //    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams =
    //      Vector(denizenBoundToDenizenCallerSuppliedThingFromDenizenItself) ++
    //        denizenBoundToDenizenCallerSuppliedThingFromParams
    //    val denizenBoundToDenizenCallerSuppliedThing =
    //      DenizenBoundToDenizenCallerSuppliedThing(
    //        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
    //          .map(_.functionBoundToCallerSuppliedPrototype)
    //          .reduceOption(_ ++ _).getOrElse(Map()),
    //        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
    //          .map(_.implBoundToCallerSuppliedImpl)
    //          .reduceOption(_ ++ _).getOrElse(Map()))

    dispatcherPlaceholderFullNameToSuppliedTemplata.map(_._1).foreach(x => vassert(x.initFullName(interner) == dispatcherTemplateId))
    dispatcherCasePlaceholderFullNameToSuppliedTemplata.map(_._1).foreach(x => vassert(x.initFullName(interner) == dispatcherFullNameT))

    val dispatcherPlaceholderFullNameToSuppliedTemplataMap = dispatcherPlaceholderFullNameToSuppliedTemplata.toMap
    val dispatcherCasePlaceholderFullNameToSuppliedTemplataMap = dispatcherCasePlaceholderFullNameToSuppliedTemplata.toMap
    // Sanity check there's no overlap
    vassert(
      (dispatcherPlaceholderFullNameToSuppliedTemplataMap ++ dispatcherCasePlaceholderFullNameToSuppliedTemplataMap).size ==
        dispatcherPlaceholderFullNameToSuppliedTemplataMap.size + dispatcherCasePlaceholderFullNameToSuppliedTemplataMap.size)

    val caseSubstitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
        dispatcherTemplateId -> vimpl(dispatcherPlaceholderFullNameToSuppliedTemplataMap),
        dispatcherFullNameT -> vimpl(dispatcherCasePlaceholderFullNameToSuppliedTemplataMap))
//    val caseInstantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        dispatcherCaseFullNameT,
//        dispatcherCaseFullNameT,
//        caseDenizenBoundToDenizenCallerSuppliedThing)


    // right here we're calling it from the perspective of the abstract function
    // we need to call it from the perspective of the abstract dispatcher function's case.
    // we might need a sub-instantiator if that makes sense...

    // we need to make a instantiator that thinks in terms of impl overrides.

    val overridePrototype =
//      caseInstantiator.translatePrototype(caseSubstitutions, vimpl(), overridePrototypeT)
      translatePrototype(
        dispatcherCaseFullNameT,
        caseDenizenBoundToDenizenCallerSuppliedThing,
        caseSubstitutions,
        vimpl(),
        overridePrototypeT)

    val superInterfaceFullName = vassertSome(monouts.impls.get(implIdC))._2

    val overrride: OverrideI = vimpl()
//      OverrideI(
//        vimpl(dispatcherFullNameT), Vector(), Vector(), Map(), Map(), Map(), vimpl(dispatcherCaseFullNameT), overridePrototype)
    monouts.addMethodToVTable(implIdC, superInterfaceFullName, vimpl(abstractFuncPrototypeI), overrride)
  }

  def translateImpl(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    implIdT: IdT[IImplNameT],
    implIdC: IdI[cI, IImplNameI[cI]],
    instantiationBoundsForUnsubstitutedImpl: InstantiationBoundArgumentsI):
  Unit = {
    val implIdI: IdI[sI, IImplNameI[sI]] = vimpl(implIdC)

    val implTemplateFullName = TemplataCompiler.getImplTemplate(implIdT)
    val implDefinition =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => {
            TemplataCompiler.getImplTemplate(edge.edgeId) == implTemplateFullName
          }))


    val subCitizenT = implDefinition.subCitizen
    val subCitizenM =
      implIdI.localName match {
        case ImplNameI(template, templateArgs, subCitizen) => subCitizen
        case AnonymousSubstructImplNameI(template, templateArgs, subCitizen) => subCitizen
        case other => vimpl(other)
      }

    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItself =
      DenizenBoundToDenizenCallerBoundArgI(
        assembleCalleeDenizenFunctionBounds(
          implDefinition.runeToFuncBound, instantiationBoundsForUnsubstitutedImpl.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          implDefinition.runeToImplBound, instantiationBoundsForUnsubstitutedImpl.runeToImplBoundArg))
    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams =
      Vector(denizenBoundToDenizenCallerSuppliedThingFromDenizenItself) ++
        hoistBoundsFromParameter(hinputs, monouts, subCitizenT, subCitizenM)

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.funcBoundToCallerSuppliedBoundArgFunc)
          .reduceOption(_ ++ _).getOrElse(Map()),
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.implBoundToCallerSuppliedBoundArgImpl)
          .reduceOption(_ ++ _).getOrElse(Map()))

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
        implTemplateFullName -> assemblePlaceholderMap(implDefinition.edgeId, implIdI))
//    val instantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        implTemplateFullName,
//        implIdT,
//        denizenBoundToDenizenCallerSuppliedThing)
//    instantiator.translateImplDefinition(substitutions, implIdT, implIdI, implDefinition)
    translateCollapsedImplDefinition(
      implIdT,
      denizenBoundToDenizenCallerSuppliedThing,
      substitutions,
      implIdT,
      implIdC,
      implDefinition)


    //    val (subCitizenFullName, superInterfaceFullName, implBoundToImplCallerSuppliedPrototype) = vassertSome(monouts.impls.get(implFullName))
    //    val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenFullName)
    //    val subCitizenDefinition = hinputs.lookupCitizenByTemplateFullName(subCitizenTemplateFullName)
    //    val subCitizenPlaceholderedName = subCitizenDefinition.instantiatedCitizen
    //    val superInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(superInterfaceFullName)
    //    val superInterfaceDefinition = hinputs.lookupInterfaceByTemplateFullName(superInterfaceTemplateFullName)
    //    val superInterfacePlaceholderedName = superInterfaceDefinition.instantiatedInterface

    //    val abstractFuncToBounds = vassertSome(monouts.interfaceToAbstractFuncToBounds.get(superInterfaceFullName))
    //    abstractFuncToBounds.foreach({ case (abstractFunc, _) =>
    //      val edge =
    //        vassertSome(
    //          vassertSome(hinputs.interfaceToSubCitizenToEdge.get(superInterfacePlaceholderedName.fullName))
    //            .get(subCitizenPlaceholderedName.fullName))
    //      val abstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(abstractFunc.fullName)
    //
    //      val overridePrototype =
    //        vassertSome(edge.abstractFuncTemplateToOverrideFunc.get(abstractFuncTemplateName))
    //
    //      val funcT =
    //        DenizentranslateFunction(
    //          opts, interner, keywords, hinputs, monouts, overridePrototype.fullName,
    //          translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
    //            hinputs.getInstantiationBounds(overridePrototype.fullName)))
    //
    //      monouts.addMethodToVTable(implFullName, superInterfaceFullName, abstractFunc, funcT)
    //    })

  }

  // DO NOT SUBMIT figure out better name
  def translateFunction1(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    desiredPrototypeT: PrototypeT,
    desiredPrototypeC: PrototypeI[cI],
    suppliedBoundArgs: InstantiationBoundArgumentsI,
    // This is only Some if this is a lambda. This will contain the prototypes supplied to the top
    // level denizen by its own caller, see LCNBAFA.
    maybeDenizenBoundToDenizenCallerSuppliedThing: Option[DenizenBoundToDenizenCallerBoundArgI]):
  FunctionDefinitionI = {
    // This works because the sI/cI are never actually used in these instances, they are just a
    // compile-time type-system bit of tracking, see CCFCTS.
    val desiredPrototypeI: PrototypeI[sI] = desiredPrototypeC.asInstanceOf[PrototypeI[sI]]

    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(desiredPrototypeT.id)

    val desiredFuncSuperTemplateName = TemplataCompiler.getSuperTemplate(desiredPrototypeT.id)
    val funcT =
      vassertOne(
        hinputs.functions
          .filter(funcT => TemplataCompiler.getSuperTemplate(funcT.header.id) == desiredFuncSuperTemplateName))


    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItself =
      maybeDenizenBoundToDenizenCallerSuppliedThing.getOrElse({
        DenizenBoundToDenizenCallerBoundArgI(
          // This is a top level denizen, and someone's calling it. Assemble the bounds!
          assembleCalleeDenizenFunctionBounds(funcT.runeToFuncBound, suppliedBoundArgs.runeToFunctionBoundArg),
          // This is a top level denizen, and someone's calling it. Assemble the bounds!
          assembleCalleeDenizenImplBounds(funcT.runeToImplBound, suppliedBoundArgs.runeToImplBoundArg))
      })
    val argsM = desiredPrototypeI.id.localName.parameters.map(_.kind)
    val paramsT = funcT.header.params.map(_.tyype.kind)
    val denizenBoundToDenizenCallerSuppliedThingFromParams =
      paramsT.zip(argsM).flatMap({ case (a, x) =>
        hoistBoundsFromParameter(hinputs, monouts, a, x)
      })

    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams =
      Vector(denizenBoundToDenizenCallerSuppliedThingFromDenizenItself) ++
        denizenBoundToDenizenCallerSuppliedThingFromParams

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.funcBoundToCallerSuppliedBoundArgFunc)
          .reduceOption(_ ++ _).getOrElse(Map()),
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.implBoundToCallerSuppliedBoundArgImpl)
          .reduceOption(_ ++ _).getOrElse(Map()))


    val topLevelDenizenFullName =
      getTopLevelDenizenId(desiredPrototypeT.id)
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)
    // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
    // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
    val topLevelDenizenPlaceholderIndexToTemplata =
    topLevelDenizenFullName.localName.templateArgs

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]](
        topLevelDenizenTemplateFullName ->
          assemblePlaceholderMap(funcT.header.id, desiredPrototypeI.id))
//    val instantiator =
//      new Instantiator(
//        opts,
//        interner,
//        keywords,
//        hinputs,
//        monouts,
//        funcTemplateNameT,
//        desiredPrototypeT.id,
//        denizenBoundToDenizenCallerSuppliedThing)

    val monomorphizedFuncT =
      translateCollapsedFunction(
        desiredPrototypeT.id, denizenBoundToDenizenCallerSuppliedThing, substitutions, funcT)

    vassert(desiredPrototypeI.returnType == monomorphizedFuncT.header.returnType)

    monomorphizedFuncT
  }

  def assemblePlaceholderMap(
    idT: IdT[IInstantiationNameT],
    idI: IdI[sI, IInstantiationNameI[sI]]):
  Map[IdT[IPlaceholderNameT], ITemplataI[sI]] = {
    val containersPlaceholderMap =
    // This might be a lambda's name. If it is, its name has an init step that's the parent
    // function's name, and we want its mappings too.
      (idT.initNonPackageFullName() match {
        case Some(IdT(packageCoord, initSteps, parentLocalName : IInstantiationNameT)) => {
          assemblePlaceholderMap(
            IdT(packageCoord, initSteps, parentLocalName),
            vimpl())
        }
        case _ => Map[IdT[IPlaceholderNameT], ITemplataI[sI]]()
      })

    val placeholderedName = idT
//    val placeholderedName =
//      idT match {
//        case IdT(_, _, localName : IStructNameT) => {
//          hinputs.lookupStructByTemplate(localName.template).instantiatedCitizen.id
//        }
//        case IdT(_, _, localName : IInterfaceNameT) => {
//          hinputs.lookupInterfaceByTemplate(localName.template).instantiatedInterface.id
//        }
//        case IdT(_, _, localName : IFunctionNameT) => {
//          vassertSome(hinputs.lookupFunction(localName.template)).header.id
//        }
//        case IdT(_, _, localName : IImplNameT) => {
//          hinputs.lookupImplByTemplate(localName.template).edgeId
//        }
//        case IdT(_, _, localName : ExportNameT) => {
//          vassertOne(
//            hinputs.kindExports.filter(_.id.localName.template == localName.template).map(_.id) ++
//              hinputs.functionExports.filter(_.exportId.localName.template == localName.template).map(_.exportId))
//        }
//      }

    containersPlaceholderMap ++
      placeholderedName.localName.templateArgs
        .zip(idI.localName.templateArgs)
        .flatMap({
          case (CoordTemplataT(CoordT(placeholderOwnership, PlaceholderTemplataT(regionPlaceholderId @ IdT(_, _, RegionPlaceholderNameT(_, _, maybeRegionPureHeight)), RegionTemplataType()), KindPlaceholderT(kindPlaceholderId))), c @ CoordTemplataI(_)) => {
            vassert(placeholderOwnership == OwnT || placeholderOwnership == ShareT)
            // We might need to do something with placeholderRegion here, but I think we can just
            // assume it correctly matches up with the coord's region. The typing phase should have
            // made sure it matches up nicely.
            // If we hit this vimpl, then we might need to find some way to hand in the region,
            // even though we lost that in the translation to IdI which has no regions. We might be
            // able to scavenge it from the name, though it might be tricky to get the region of
            // region-less primitives. Perhaps we can assume theyre the same region as their
            // parent template?
            val regionTemplata =
              maybeRegionPureHeight.map(x => RegionTemplataI[sI](x)).getOrElse(vimpl())
            List(
              (regionPlaceholderId -> regionTemplata),// vimpl(/*c.coord.region*/)),
              (kindPlaceholderId -> c))
          }
          case (KindTemplataT(KindPlaceholderT(placeholderId)), kindTemplataI) => {
            List((placeholderId -> kindTemplataI))
          }
          case (PlaceholderTemplataT(placeholderId, tyype), templataI) => {
            List((placeholderId -> templataI))
          }
          case (MutabilityTemplataT(MutableT),MutabilityTemplataI(MutableI)) |
               (MutabilityTemplataT(ImmutableT),MutabilityTemplataI(ImmutableI)) => {
            // We once got a `mut` for the placeholdered name's templata.
            // That's because we do some specialization for arrays still.
            // They don't come with a placeholder, so ignore them.
            List()
          }
          case other => vimpl(other)
        })
        .toMap
  }

  // This isn't just for parameters, it's for impl subcitizens, and someday for cases too.
  // See NBIFP
  private def hoistBoundsFromParameter(
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    paramT: KindT,
    paramI: KindIT[sI]):
  Option[DenizenBoundToDenizenCallerBoundArgI] = {
    (paramT, paramI) match {
      case (StructTT(structFullNameT), StructIT(structFullNameI)) => {
        val calleeRuneToBoundArgT = hinputs.getInstantiationBoundArgs(structFullNameT)
        val structDenizenBoundToDenizenCallerSuppliedThing =
          vassertSome(monouts.structToBounds.get(structFullNameI))
        val structT = findStruct(hinputs, structFullNameT)
        val denizenBoundToDenizenCallerSuppliedThing =
          hoistBoundsFromParameterInner(
            structDenizenBoundToDenizenCallerSuppliedThing, calleeRuneToBoundArgT, structT.runeToFunctionBound, structT.runeToImplBound)
        Some(denizenBoundToDenizenCallerSuppliedThing)
      }
      case (InterfaceTT(interfaceFullNameT), InterfaceIT(interfaceFullNameM)) => {
        val calleeRuneToBoundArgT = hinputs.getInstantiationBoundArgs(interfaceFullNameT)
        val interfaceDenizenBoundToDenizenCallerSuppliedThing = vassertSome(monouts.interfaceToBounds.get(vimpl(interfaceFullNameM)))
        val interfaceT = findInterface(hinputs, interfaceFullNameT)
        val denizenBoundToDenizenCallerSuppliedThing =
          hoistBoundsFromParameterInner(
            interfaceDenizenBoundToDenizenCallerSuppliedThing,
            calleeRuneToBoundArgT,
            interfaceT.runeToFunctionBound,
            interfaceT.runeToImplBound)
        Some(denizenBoundToDenizenCallerSuppliedThing)
      }
      case _ => None
    }
  }

  // See NBIFP
  private def hoistBoundsFromParameterInner(
    parameterDenizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    calleeRuneToBoundArgT: InstantiationBoundArgumentsT,
    calleeRuneToCalleeFunctionBoundT: Map[IRuneS, IdT[FunctionBoundNameT]],
    calleeRuneToCalleeImplBoundT: Map[IRuneS, IdT[ImplBoundNameT]]):
  DenizenBoundToDenizenCallerBoundArgI = {
    val calleeFunctionBoundTToBoundArgM = parameterDenizenBoundToDenizenCallerSuppliedThing.funcBoundToCallerSuppliedBoundArgFunc
    val implBoundTToBoundArgM = parameterDenizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl

    val callerSuppliedBoundToInstantiatedFunction =
      calleeRuneToCalleeFunctionBoundT.map({ case (calleeRune, calleeBoundT) =>
        // We don't care about the callee bound, we only care about what we're sending in to it.
        val (_) = calleeBoundT

        // This is the prototype the caller is sending in to the callee to satisfy its bounds.
        val boundArgT = vassertSome(calleeRuneToBoundArgT.runeToFunctionBoundArg.get(calleeRune))
        boundArgT.id match {
          case IdT(packageCoord, initSteps, last@FunctionBoundNameT(_, _, _)) => {
            // The bound arg is also the same thing as the caller bound.
            val callerBoundT = IdT(packageCoord, initSteps, last)

            // The bound arg we're sending in is actually one of our (the caller) own bounds.
            //
            // "But wait, we didn't specify any bounds."
            // This is actually a bound that was implicitly added from NBIFP.
            //
            // We're going to pull this in as our own bound.
            val instantiatedPrototype = vassertSome(calleeFunctionBoundTToBoundArgM.get(calleeBoundT))
            Some(callerBoundT -> instantiatedPrototype)
          }
          case _ => None
        }
      }).flatten.toMap

    val callerSuppliedBoundToInstantiatedImpl =
      calleeRuneToCalleeImplBoundT.map({
        case (calleeRune, calleeBoundT) =>
          // We don't care about the callee bound, we only care about what we're sending in to it.
          val (_) = calleeBoundT

          // This is the prototype the caller is sending in to the callee to satisfy its bounds.
          val boundArgT = vassertSome(calleeRuneToBoundArgT.runeToImplBoundArg.get(calleeRune))
          boundArgT match {
            case IdT(packageCoord, initSteps, last@ImplBoundNameT(_, _)) => {
              val boundT = IdT(packageCoord, initSteps, last)
              // The bound arg we're sending in is actually one of our (the caller) own bounds.
              //
              // "But wait, we didn't specify any bounds."
              // This is actually a bound that was implicitly added from NBIFP.
              //
              // We're going to pull this in as our own bound.
              val instantiatedPrototype = vassertSome(implBoundTToBoundArgM.get(boundT))
              Some(boundT -> instantiatedPrototype)
            }
            case _ => None
          }
      }).flatten.toMap

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        callerSuppliedBoundToInstantiatedFunction,
        callerSuppliedBoundToInstantiatedImpl)
    denizenBoundToDenizenCallerSuppliedThing
  }

//  def translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, templata: ITemplataT[ITemplataType]): ITemplataI = {
//    vimpl()
//  }

  //  selfFunctionBoundToRuneUnsubstituted: Map[PrototypeT, IRuneS],
  //  denizenRuneToDenizenCallerPrototype: Map[IRuneS, PrototypeT]) {

  //  if (opts.sanityCheck) {
  //    denizenFunctionBoundToDenizenCallerSuppliedPrototype.foreach({
  //      case (denizenFunctionBound, denizenCallerSuppliedPrototype) => {
  //        vassert(Collector.all(denizenCallerSuppliedPrototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
  //      }
  //    })
  //  }

  def translateStructMember(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    member: IStructMemberT):
  (CoordI[sI], StructMemberI) = {
    member match {
      case NormalStructMemberT(name, variability, tyype) => {
        val (memberSubjectiveIT, memberTypeI) =
          tyype match {
            case ReferenceMemberTypeT(unsubstitutedCoord) => {
              val typeI =
                translateCoord(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, unsubstitutedCoord)
              val result =
                ReferenceMemberTypeI(
                  RegionCollapser.collapseCoord(RegionCounter.countCoord(typeI), typeI))
              (typeI, result)
            }
            case AddressMemberTypeT(unsubstitutedCoord) => {
              val typeI =
                translateCoord(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, unsubstitutedCoord)
              val result = AddressMemberTypeI(RegionCollapser.collapseCoord(RegionCounter.countCoord(typeI), typeI))
              (typeI, result)
            }
          }
        val nameI = translateVarName(name)
        val memberI =
          StructMemberI(
            RegionCollapser.collapseVarName(RegionCounter.countVarName(nameI), nameI),
            translateVariability(variability),
            memberTypeI)
        (memberSubjectiveIT, memberI)
      }
      case VariadicStructMemberT(name, tyype) => {
        vimpl()
      }
    }
  }

  def translateVariability(x: VariabilityT): VariabilityI = {
    x match {
      case VaryingT => VaryingI
      case FinalT => FinalI
    }
  }

  def translateMutability(m: MutabilityT): MutabilityI = {
    m match {
      case MutableT => MutableI
      case ImmutableT => ImmutableI
    }
  }

  // This is run at the call site, from the caller's perspective
  def translatePrototype(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    desiredPrototypeT: PrototypeT):
  (PrototypeI[sI], PrototypeI[cI]) = {
    val PrototypeT(desiredPrototypeFullNameUnsubstituted, desiredPrototypeReturnTypeUnsubstituted) = desiredPrototypeT

    val runeToBoundArgsForCall =
      translateBoundArgsForCallee(
        denizenName,
        denizenBoundToDenizenCallerSuppliedThing,
        substitutions,
        perspectiveRegionT,
        hinputs.getInstantiationBoundArgs(desiredPrototypeT.id))

    val returnSubjectiveIT =
      translateCoord(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, desiredPrototypeReturnTypeUnsubstituted)

    val uncollapsedDesiredPrototypeI =
      PrototypeI[sI](
        translateFunctionFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, desiredPrototypeFullNameUnsubstituted),
        returnSubjectiveIT)

    desiredPrototypeT.id match {
      case IdT(packageCoord, initSteps, name @ FunctionBoundNameT(_, _, _)) => {
        val funcBoundName = IdT(packageCoord, initSteps, name)
        val result = vassertSome(denizenBoundToDenizenCallerSuppliedThing.funcBoundToCallerSuppliedBoundArgFunc.get(funcBoundName))
        //        if (opts.sanityCheck) {
        //          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        //        }
        (result, vimpl())
      }
      case IdT(_, _, ExternFunctionNameT(_, _)) => {
        if (opts.sanityCheck) {
          vassert(Collector.all(uncollapsedDesiredPrototypeI, { case KindPlaceholderTemplateNameT(_, _) => }).isEmpty)
        }
        val collapsedDesiredPrototypeI =
          RegionCollapser.collapsePrototype(
            RegionCounter.countPrototype(uncollapsedDesiredPrototypeI),
            uncollapsedDesiredPrototypeI)
        (uncollapsedDesiredPrototypeI, collapsedDesiredPrototypeI)
      }
      case IdT(_, _, last) => {
        last match {
          case LambdaCallFunctionNameT(_, _, _) => {
            vassert(
              uncollapsedDesiredPrototypeI.id.steps.slice(0, uncollapsedDesiredPrototypeI.id.steps.length - 2) ==
                denizenName.steps)
            vcurious(uncollapsedDesiredPrototypeI.id.steps.startsWith(denizenName.steps))
          }
          case _ =>
        }

//        // Let's say we want to call 1'myPureDisplay(0'board).
//        // We want that to become 0'myPureDisplay(-1'board).
//        // The default region we send should always be zero, and all incoming imms should be negative.
//        // DO NOT SUBMIT centralize docs
//        // TODO use an array instead of a map here
//        val oldRegionPureHeights =
//          Collector.all(uncollapsedDesiredPrototypeI, {
//            case RegionTemplataI(pureHeight) => pureHeight
//          }).toVector.distinct.sorted
//        val oldToNewRegionPureHeight =
//          oldRegionPureHeights.zipWithIndex.map({ case (oldRegionPureHeight, index) =>
//            (oldRegionPureHeight, index - (oldRegionPureHeights.length - 1))
//          }).toMap
        val collapsedDesiredPrototypeI =
          RegionCollapser.collapsePrototype(
            RegionCounter.countPrototype(uncollapsedDesiredPrototypeI),
            uncollapsedDesiredPrototypeI)


        monouts.newFunctions.enqueue(
          (
            desiredPrototypeT,
            collapsedDesiredPrototypeI,
            runeToBoundArgsForCall,
            // We need to supply our bounds to our lambdas, see LCCPGB and LCNBAFA.
            if (uncollapsedDesiredPrototypeI.id.steps.startsWith(denizenName.steps)) {
              Some(denizenBoundToDenizenCallerSuppliedThing)
            } else {
              None
            }))
        (uncollapsedDesiredPrototypeI, collapsedDesiredPrototypeI)
      }
    }
  }

  private def translateBoundArgsForCallee(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    // This contains a map from rune to a prototype, specifically the prototype that we
    // (the *template* caller) is supplying to the *template* callee. This prototype might
    // be a placeholder, phrased in terms of our (the *template* caller's) placeholders
    instantiationBoundArgsForCallUnsubstituted: InstantiationBoundArgumentsT):
  InstantiationBoundArgumentsI = {
    val runeToSuppliedPrototypeForCallUnsubstituted =
      instantiationBoundArgsForCallUnsubstituted.runeToFunctionBoundArg
    val runeToSuppliedPrototypeForCall =
    // For any that are placeholders themselves, let's translate those into actual prototypes.
      runeToSuppliedPrototypeForCallUnsubstituted.map({ case (rune, suppliedPrototypeUnsubstituted) =>
        rune ->
          (suppliedPrototypeUnsubstituted.id match {
            case IdT(packageCoord, initSteps, name @ FunctionBoundNameT(_, _, _)) => {
              vassertSome(
                denizenBoundToDenizenCallerSuppliedThing.funcBoundToCallerSuppliedBoundArgFunc.get(
                  IdT(packageCoord, initSteps, name)))
            }
            case _ => {
              val (prototypeI, prototypeC) =
                translatePrototype(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, vimpl(), suppliedPrototypeUnsubstituted)
              prototypeI
            }
          })
      })
    // And now we have a map from the callee's rune to the *instantiated* callee's prototypes.

    val runeToSuppliedImplForCallUnsubstituted =
      instantiationBoundArgsForCallUnsubstituted.runeToImplBoundArg
    val runeToSuppliedImplForCall =
    // For any that are placeholders themselves, let's translate those into actual prototypes.
      runeToSuppliedImplForCallUnsubstituted.map({ case (rune, suppliedImplUnsubstituted) =>
        rune ->
          (suppliedImplUnsubstituted match {
            case IdT(packageCoord, initSteps, name @ ImplBoundNameT(_, _)) => {
              vassertSome(
                denizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl.get(
                  IdT(packageCoord, initSteps, name)))
            }
            case _ => {
              // Not sure about these three lines, but they seem to work.
              val runeToBoundArgsForCall =
                translateBoundArgsForCallee(
                  denizenName,
                  denizenBoundToDenizenCallerSuppliedThing,
                  substitutions,
                  perspectiveRegionT,
                  hinputs.getInstantiationBoundArgs(suppliedImplUnsubstituted))
              val implNameI =
                translateImplFullName(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing,substitutions, perspectiveRegionT, suppliedImplUnsubstituted, runeToBoundArgsForCall)
              implNameI
            }
          })
      })
    // And now we have a map from the callee's rune to the *instantiated* callee's impls.

    InstantiationBoundArgumentsI(runeToSuppliedPrototypeForCall, runeToSuppliedImplForCall)
  }

  def translateCollapsedStructDefinition(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    newIdT: IdT[IStructNameT],
    newId: IdI[cI, IStructNameI[cI]],
    structDefT: StructDefinitionT):
  Unit = {
    val StructDefinitionT(templateName, instantiatedCitizen, attributes, weakable, mutabilityT, members, isClosure, _, _) = structDefT

    if (opts.sanityCheck) {
      vassert(Collector.all(newId, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    val perspectiveRegionT =
      structDefT.instantiatedCitizen.id.localName.templateArgs.last match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    val mutability = ITemplataI.expectMutabilityTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, mutabilityT)).mutability

    if (monouts.structToMutability.contains(newId)) {
      return
    }
    monouts.structToMutability.put(newId, mutability)

//    val currentPureHeight = vimpl()

    val result =
      StructDefinitionI(
//        templateName,
        StructIT(newId),
        attributes.map(vimpl(_)),
        weakable,
        mutability,
        members.map(memberT => {
          translateStructMember(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, memberT)._2
        }),
        isClosure,
        Map(),
        Map())

    vassert(result.instantiatedCitizen.id == newId)

    monouts.structs.put(result.instantiatedCitizen.id, result)

    if (opts.sanityCheck) {
      vassert(Collector.all(result.instantiatedCitizen, { case KindPlaceholderNameT(_) => }).isEmpty)
      vassert(Collector.all(result.members, { case KindPlaceholderNameT(_) => }).isEmpty)
    }
    result
  }

  // This inner function is conceptually from the interface's own perspective. That's why it's
  // taking in a collapsed id.
  def translateInterfaceDefinitionCollapsed(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    newIdC: IdI[cI, IInterfaceNameI[cI]],
    interfaceDefT: InterfaceDefinitionT):
  Unit = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutabilityT, _, _, internalMethods) = interfaceDefT

    val mutability =
      ITemplataI.expectMutabilityTemplata(
        translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, vimpl(), mutabilityT)).mutability

    if (monouts.interfaceToMutability.contains(newIdC)) {
      return
    }
    monouts.interfaceToMutability.put(newIdC, mutability)

    val newInterfaceIT = InterfaceIT(newIdC)

    val result =
      InterfaceDefinitionI(
        newInterfaceIT,
        attributes.map(vimpl(_)),
        weakable,
        mutability,
        Map(),
        Map(),
        Vector())

    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    vassert(!monouts.interfaceToImplToAbstractPrototypeToOverride.contains(newIdC))
    monouts.interfaceToImplToAbstractPrototypeToOverride.put(newIdC, mutable.HashMap())

    monouts.interfacesWithoutMethods.put(newIdC, result)

    vassert(!monouts.interfaceToAbstractFuncToVirtualIndex.contains(newIdC))
    monouts.interfaceToAbstractFuncToVirtualIndex.put(newIdC, mutable.HashMap())

    vassert(!monouts.interfaceToImpls.contains(newIdC))
    monouts.interfaceToImpls.put(newIdC, mutable.HashSet())

    vassert(result.instantiatedCitizen.id == newIdC)
  }

  def translateFunctionHeader(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    header: FunctionHeaderT):
  FunctionHeaderI = {
    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = header

    val newFullNameI =
      translateFunctionFullName(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, fullName)
    val newFullNameCounts = RegionCounter.countFunctionId(newFullNameI)
    val newFullNameC =
      RegionCollapser.collapseId[IFunctionNameI[sI], IFunctionNameI[cI]](
        newFullNameCounts,
        newFullNameI,
        x => RegionCollapser.collapseFunctionName(newFullNameCounts, x))

    val returnIT =
      translateCoord(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, returnType)
    val returnIC = collapseCoord(RegionCounter.countCoord(returnIT), returnIT)

    val result =
      FunctionHeaderI(
        newFullNameC,
        attributes.map(translateFunctionAttribute),
        params.map(translateParameter(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        returnIC)

    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(result.fullName, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.attributes, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.params, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.returnType, { case PlaceholderNameT(_) => }).isEmpty)
    //    }

    result
  }

  def translateFunctionAttribute(x: IFunctionAttributeT): IFunctionAttributeI = {
    x match {
      case UserFunctionT => UserFunctionI
      case PureT => PureI
      case other => vimpl(other)
    }
  }

  // DO NOT SUBMIT why does this one not take in the collapsed id like the struct and interface things
  def translateCollapsedFunction(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    functionT: FunctionDefinitionT):
  FunctionDefinitionI = {
    val FunctionDefinitionT(headerT, _, _, bodyT) = functionT

    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = headerT

    if (opts.sanityCheck) {
      Collector.all(substitutions.toVector, {
        case RegionTemplataI(x) if x > 0 => vwat()
      })
    }

    val perspectiveRegionT =
      functionT.header.id.localName.templateArgs.last match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    val functionIdI =
      translateFunctionFullName(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, fullName)
    val functionIdC =
      RegionCollapser.collapseFunctionId(RegionCounter.countFunctionId(functionIdI), functionIdI)

    monouts.functions.get(functionIdC) match {
      case Some(func) => return func
      case None =>
    }

    val newHeader = translateFunctionHeader(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, headerT)

    val startingEnv = NodeEnvironment(None)
    val (bodySubjectiveIT, bodyIE) =
      translateRefExpr(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, startingEnv, substitutions, perspectiveRegionT, bodyT)

    val result = FunctionDefinitionI(newHeader, Map(), Map(), bodyIE)
    monouts.functions.put(result.header.id, result)
    result
  }

  def translateLocalVariable(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    variable: ILocalVariableT):
  // Returns subjective coord and the local var
  (CoordI[sI], ILocalVariableI) = {
    variable match {
      case r @ ReferenceLocalVariableT(_, _, _) => {
        translateReferenceLocalVariable(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, r)
      }
      case a @ AddressibleLocalVariableT(_, _, _) => {
        translateAddressibleLocalVariable(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, a)
      }
    }
  }

  def translateReferenceLocalVariable(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    variable: ReferenceLocalVariableT):
  // Returns subjective coord and the local var
  (CoordI[sI], ReferenceLocalVariableI) = {
    val ReferenceLocalVariableT(id, variability, coord) = variable
    val coordI =
      translateCoord(
        denizenName,
        denizenBoundToDenizenCallerSuppliedThing,
        substitutions,
        perspectiveRegionT,
        coord)
    val varNameI = translateVarName(id)
    val localI =
      ReferenceLocalVariableI(
        RegionCollapser.collapseVarName(RegionCounter.countVarName(varNameI), varNameI),
        translateVariability(variability),
        RegionCollapser.collapseCoord(RegionCounter.countCoord(coordI), coordI))
    (coordI, localI)
  }

  def translateAddressibleLocalVariable(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    variable: AddressibleLocalVariableT):
  // Returns subjective coord and the local var
  (CoordI[sI], AddressibleLocalVariableI) = {
    val AddressibleLocalVariableT(id, variability, coord) = variable
    val coordI =
      translateCoord(
        denizenName,
        denizenBoundToDenizenCallerSuppliedThing,
        substitutions,
        // The LocalVariable's type is from its own perspective.
        expectRegionPlaceholder(coord.region),
        coord)
    val varI = translateVarName(id)
    val localI =
      AddressibleLocalVariableI(
        RegionCollapser.collapseVarName(RegionCounter.countVarName(varI), varI),
        translateVariability(variability),
        RegionCollapser.collapseCoord(RegionCounter.countCoord(coordI), coordI))
    (coordI, localI)
  }

  def translateAddrExpr(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: AddressExpressionTE):
  // Returns the subjective coord (see HCCSCS) and the expression.
  (CoordI[sI], AddressExpressionIE) = {
    expr match {
      case LocalLookupTE(range, localVariableT) => {
//        // We specifically don't *translate* LocalLookupTE.localVariable because we can't translate
//        // it properly from here with our current understandings of the regions' mutabilities, we
//        // need its original type. See CTOTFIPB.
//        val localVariable = env.lookupOriginalTranslatedVariable(localVariableT.name)
        val (localSubjectiveIT, localVariableI) =
          translateLocalVariable(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, localVariableT)
//
//        val sourceRegion =
//          ITemplataI.expectRegionTemplata(
//            translateTemplata(
//              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, vimpl()))

//        val subjectiveResultIT =
//          CoordI(
//            (localVariableI.coord.ownership, coordRegionIsMutable(substitutions, perspectiveRegionT, localVariableT.coord)) match {
//              case (OwnT, _) => OwnI
//              case other => vimpl(other)
//            },
//            localVariableI.coord.kind)

        val resultSubjectiveIT = localSubjectiveIT
        val resultIE =
          LocalLookupIE(
            localVariableI,
            RegionCollapser.collapseCoord(RegionCounter.countCoord(resultSubjectiveIT), resultSubjectiveIT))
        (resultSubjectiveIT, resultIE)
      }
      case ReferenceMemberLookupTE(range, structExprT, memberNameT, memberCoordT, variability) => {
        val (structSubjectiveIT, structIE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, structExprT)
        val structSubjectiveStructIT = structSubjectiveIT.kind.expectStruct()
        val memberName = translateVarName(memberNameT)

//        // We can't translate ReferenceMemberLookupTE.memberCoord's kind here because we'll
//        // translate its template args' regions incorrectly according to their current mutabilities.
//        // They need to be the mutabilities at the time they were introduced, see CTOTFIPB. So
//        // instead, we look it up from the struct definition.
//        val structDef = vassertSome(monouts.structs.get(structSubjectiveStructIT.id))
//        val defMemberCoord: CoordT = vimpl()
////          vassertSome(structDef.members.find(_.name == memberName)) match {
////            case NormalStructMemberT(name, variability, tyype) => tyype.reference
////            case VariadicStructMemberT(name, tyype) => vimpl()
//          }
//      // However, the resulting coord's region *should* have the current mutability.

        val memberCoordI =
          translateCoord(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, memberCoordT)

        val resultRegion =
          ITemplataI.expectRegionTemplata(
            translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, memberCoordT.region))

        val resultOwnership =
          (memberCoordI.ownership, resultRegion) match {
            case (OwnI, RegionTemplataI(_)) => OwnI
//            case (MutableShareI, RegionTemplataI(true)) => MutableShareI
//            case (MutableShareI, RegionTemplataI(false)) => ImmutableShareI
            case (ImmutableShareI, _) => ImmutableShareI
            case other => vimpl(other)
          }

        val resultSubjectiveIT = CoordI(resultOwnership, memberCoordI.kind)
        val resultIE =
          ReferenceMemberLookupIE(
            range,
            structIE,
            RegionCollapser.collapseVarName(RegionCounter.countVarName(memberName), memberName),
            collapseCoord(RegionCounter.countCoord(resultSubjectiveIT), resultSubjectiveIT),
            translateVariability(variability))
        (resultSubjectiveIT, resultIE)
      }
      case StaticSizedArrayLookupTE(range, arrayExprT, indexExprT, elementTypeT, variability) => {
        // DO NOT SUBMIT combine a lot of this with the ReferenceMemberLookupTE case
        val (arraySubjectiveIT, arrayIE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExprT)
//        // We can't translate StaticSizedArrayLookupTE.elementTypeT's kind here because we'll
//        // translate its template args' regions incorrectly according to their current mutabilities.
//        // They need to be the mutabilities at the time they were introduced, see CTOTFIPB. So
//        // instead, we look it up from the struct definition.
//        val elementType =
//          arraySubjectiveIT.kind match {
//            case StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, _, _, RawArrayNameI(_, elementType, _)))) => {
//              elementType
//            }
//          }
        val elementTypeI =
          translateCoord(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, elementTypeT)

        val (indexIT, indexCE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, indexExprT)

//        // However, the resulting coord's region *should* have the current mutability.
//        val resultRegion =
//          ITemplataI.expectRegionTemplata(
//            translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, elementTypeT.region))

        val resultCoord = CoordI(elementTypeI.ownership, elementTypeI.kind)

        val resultIE =
          StaticSizedArrayLookupIE(
            range,
            arrayIE,
            indexCE,
            RegionCollapser.collapseCoord(RegionCounter.countCoord(resultCoord), resultCoord),
            translateVariability(variability))
        (resultCoord, resultIE)
      }
      case AddressMemberLookupTE(range, structExpr, memberName, resultType2, variability) => {
        val (structIT, structCE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, structExpr)
        val varNameI = translateVarName(memberName)
        val resultIT =
          translateCoord(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultType2)
        val variabilityI = translateVariability(variability)

        val resultCE =
          AddressMemberLookupIE(
            structCE,
            RegionCollapser.collapseVarName(RegionCounter.countVarName(varNameI), varNameI),
            RegionCollapser.collapseCoord(RegionCounter.countCoord(resultIT), resultIT),
            variabilityI)
        (resultIT, resultCE)
      }
      case RuntimeSizedArrayLookupTE(range, arrayExpr, rsaTT, indexExpr, variability) => {
        val (arrayIT, arrayCE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
        val rsaIT =
          translateRuntimeSizedArray(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, rsaTT)
        val (indexIT, indexCE) =
          translateRefExpr(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, indexExpr)
        val variabilityI = translateVariability(variability)

        // We can't just say rsaIT.elementType here because that's the element from the array's own
        // perspective.
        val elementIT =
          translateCoord(
            denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, rsaTT.elementType)

        val resultIT = elementIT
        val resultCE =
          RuntimeSizedArrayLookupIE(
            arrayCE, indexCE, RegionCollapser.collapseCoord(RegionCounter.countCoord(elementIT), elementIT), variabilityI)
        (resultIT, resultCE)
      }
      case other => vimpl(other)
    }
  }

  def translateExpr(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: ExpressionT):
  // Returns the subjective coord (see HCCSCS) and the expression.
  (CoordI[sI], ExpressionI) = {
    expr match {
      case r : ReferenceExpressionTE => {
        translateRefExpr(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, r)
      }
      case a : AddressExpressionTE => {
        translateAddrExpr(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, a)
      }
    }
  }

  def translateRefExpr(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: ReferenceExpressionTE):
  // Returns the subjective coord (see HCCSCS) and the expression.
  (CoordI[sI], ReferenceExpressionIE) = {
    val denizenTemplateName = TemplataCompiler.getTemplate(denizenName)
    val (resultIT: CoordI[sI], resultCE) =
      expr match {
        case LetNormalTE(variableT, innerTE) => {
          val (innerIT, innerIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerTE)
          val (localIT, localI) =
            translateLocalVariable(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, variableT)
//          env.addTranslatedVariable(variableT.name, vimpl(translatedVariable))
          val subjectiveResultIT = CoordI[sI](MutableShareI, VoidIT())
          val exprIE =
            LetNormalIE(
              localI, innerIE, collapseCoord(RegionCounter.countCoord(subjectiveResultIT), subjectiveResultIT))
          (subjectiveResultIT, exprIE)
        }
        case PureTE(location, newDefaultRegionT, oldRegionToNewRegion, inner, resultCoordT) => {
//          val oldPureHeight = currentPureHeight
          val oldPerspectiveRegionT = perspectiveRegionT
          val newDefaultRegionNameT =
            newDefaultRegionT match {
              case PlaceholderTemplataT(id @ IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
                IdT(packageCoord, initSteps, r)
              }
              case other => vwat(other)
            }
          val newPerspectiveRegionT = newDefaultRegionNameT
//          val newPureHeight = currentPureHeight + 1
//          val newDefaultRegionName =
//            translateRegionFullName(
//              substitutions, perspectiveRegionT, newDefaultRegionNameT)
          val newDefaultRegion = RegionTemplataI[sI](vassertSome(newDefaultRegionNameT.localName.pureHeight))
          val oldSubstitutionsForThisDenizenTemplate =
            substitutions.getOrElse(denizenTemplateName, Map())
          val newSubstitutionsForThisDenizenTemplate =
            oldSubstitutionsForThisDenizenTemplate + (newDefaultRegionNameT -> newDefaultRegion)
          val newSubstitutions =
            substitutions + (denizenTemplateName -> newSubstitutionsForThisDenizenTemplate)

          val (innerSubjectiveIT, innerIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, newSubstitutions, newPerspectiveRegionT, inner)

          val resultFromWithinPureIT = innerSubjectiveIT
          val resultFromWithinPureCE =
            RegionCollapser.collapseCoord(
              RegionCounter.countCoord(resultFromWithinPureIT), resultFromWithinPureIT)
          val resultFromOutsidePureIT =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, oldPerspectiveRegionT, resultCoordT)
          val resultFromOutsidePureCE =
            RegionCollapser.collapseCoord(
              RegionCounter.countCoord(resultFromOutsidePureIT), resultFromOutsidePureIT)

          if (resultFromWithinPureCE.kind != resultFromOutsidePureCE.kind) {
            vimpl("Transmigration unimplemented!")
          }

          val mutabilifyNeeded =
            (resultFromWithinPureIT.ownership, resultFromOutsidePureIT.ownership) match {
              case (x, y) if x == y => false
              case (ImmutableBorrowI, MutableBorrowI) => true
              case (ImmutableShareI, MutableShareI) => true
              case other => vimpl(other)
            }

          val resultIE =
            if (mutabilifyNeeded) {
              MutabilifyIE(innerIE, collapseCoord(RegionCounter.countCoord(resultFromOutsidePureIT), resultFromOutsidePureIT))
            } else {
              innerIE
            }

          (resultFromOutsidePureIT, resultIE)

//          MutabilifyIE(
////            location,
//////            newDefaultRegion,
//////            oldRegionToNewRegion.map({ case (oldRegionT, newRegionT) =>
//////              vassert(newRegionT == newDefaultRegionT)
//////              val newRegion = newDefaultRegion
//////              val oldRegion =
//////                expectRegionTemplata(
//////                  translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//////                    newSubstitutions, perspectiveRegionT, oldRegionT))
//////              (oldRegion, newRegion)
//////            }),
//            )
////            translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, oldPerspectiveRegionT, resultCoordT))
        }
        case BlockTE(inner) => {
          val (innerIT, innerIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, inner)
          val resultIT = innerIT
          val resultIE = BlockIE(innerIE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case ReturnTE(inner) => {
          val (innerIT, innerIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, inner)
          val resultIE = ReturnIE(innerIE)
          (CoordI[sI](MutableShareI, NeverIT(false)), resultIE)
        }
        case c @ ConsecutorTE(inners) => {
          val resultTT = c.result.coord
          val resultIT =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultTT)
          val innersIE =
            inners.map(innerTE => {
              translateRefExpr(
                denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerTE)._2
            })
          val resultIE = ConsecutorIE(innersIE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case ConstantIntTE(value, bits, region) => {
          val resultCE =
            ConstantIntIE(
              ITemplataI.expectIntegerTemplata(
                translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, value)).value,
              bits)
          (CoordI[sI](MutableShareI, IntIT(bits)), resultCE)
        }
        case ConstantStrTE(value, region) => {
          val resultCE = ConstantStrIE(value)
          (CoordI[sI](MutableShareI, StrIT()), resultCE)
        }
        case ConstantBoolTE(value, region) => {
          val resultCE = ConstantBoolIE(value)
          (CoordI[sI](MutableShareI, BoolIT()), resultCE)
        }
        case ConstantFloatTE(value, region) => {
          val resultCE = ConstantFloatIE(value)
          (CoordI[sI](MutableShareI, BoolIT()), resultCE)
        }
        case UnletTE(variable) => {
          val (localIT, localIE) =
            translateLocalVariable(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, variable)
          val resultIT = localIT
//          val local = env.lookupOriginalTranslatedVariable(variable.name)
          val resultIE = UnletIE(localIE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case DiscardTE(innerTE) => {
          val (innerIT, innerIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerTE)
          val resultIE = DiscardIE(innerIE)
          (CoordI[sI](MutableShareI, VoidIT()), resultIE)
        }
        case VoidLiteralTE(region) => {
          (CoordI[sI](MutableShareI, VoidIT()), VoidLiteralIE())
        }
        case FunctionCallTE(prototypeT, args) => {
          val (prototypeI, prototypeC) =
            translatePrototype(
              denizenName,
              denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,
              prototypeT)
          val inners =
            args.map(argTE => {
              translateRefExpr(
                denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, argTE)._2
            })
          val resultIT = prototypeI.returnType
          val resultIE = FunctionCallIE(prototypeC, inners, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case InterfaceFunctionCallTE(superFunctionPrototypeT, virtualParamIndex, resultReference, args) => {
          val (superFunctionPrototypeI, superFunctionPrototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, superFunctionPrototypeT)
          val resultIT = superFunctionPrototypeI.returnType
          val resultCE =
            InterfaceFunctionCallIE(
              superFunctionPrototypeC,
              virtualParamIndex,
              args.map(arg => {
                translateRefExpr(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arg)._2
              }),
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          val interfaceFullNameC =
            superFunctionPrototypeC.paramTypes(virtualParamIndex).kind.expectInterface().id
          //        val interfaceFullName =
          //          translateInterfaceFullName(
          //            interfaceFullNameT,
          //            translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
          //              hinputs.getInstantiationBounds(callee.toPrototype.fullName)))

          val instantiationBoundArgs =
            translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,
              // but this is literally calling itself from where its defined
              // perhaps we want the thing that originally called
              hinputs.getInstantiationBoundArgs(superFunctionPrototypeT.id))

          monouts.newAbstractFuncs.enqueue(
            (superFunctionPrototypeT, superFunctionPrototypeC, virtualParamIndex, interfaceFullNameC, instantiationBoundArgs))

          (resultIT, resultCE)
        }
        case ArgLookupTE(paramIndex, reference) => {
          val typeI =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, reference)
          val resultCE = ArgLookupIE(paramIndex, collapseCoord(RegionCounter.countCoord(typeI), typeI))
          (typeI, resultCE)
        }
        case SoftLoadTE(originalInner, originalTargetOwnership) => {
          val (innerIT, innerIE) =
            translateAddrExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, originalInner)
          val targetOwnership =
            // First, figure out what ownership it is after substitution.
            // if we have an owned T but T is a &Ship, then own + borrow = borrow
            (originalTargetOwnership, innerIT.ownership) match {
//              case (a, b) if a == b => a
              case (ShareT, ImmutableShareI) => ImmutableShareI
              case (ShareT, MutableShareI) => MutableShareI
              case (BorrowT, ImmutableShareI) => ImmutableShareI
              case (BorrowT, MutableShareI) => MutableShareI
              case (BorrowT, ImmutableBorrowI) => ImmutableBorrowI
              case (BorrowT, MutableBorrowI | OwnI) => {
//                MutableBorrowI
                if (coordRegionIsMutable(substitutions, perspectiveRegionT, originalInner.result.coord)) {
                  MutableBorrowI
                } else {
                  ImmutableBorrowI
                }
              }
              //              case (BorrowT, WeakT) => WeakT
//              case (BorrowT, OwnT) => {
//                if (coordRegionIsMutable(substitutions, perspectiveRegionT, originalInner.result.coord)) {
//                  MutableBorrowI
//                } else {
//                  ImmutableBorrowI
//                }
//              }
              case (WeakT, ImmutableShareI) => ImmutableShareI
              case (WeakT, MutableShareI) => MutableShareI
//              case (WeakT, OwnT) => WeakT
              case (WeakT, ImmutableBorrowI) => vimpl(WeakT)
              case (WeakT, MutableBorrowI) => vimpl(WeakT)
              case other => vwat(other)
            }
          val resultIT = CoordI[sI](targetOwnership, innerIT.kind)
          val resultIE =
            SoftLoadIE(innerIE, targetOwnership, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case ExternFunctionCallTE(prototype2, args) => {
          val (prototypeI, prototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, prototype2)
          val argsIE =
            args.map(argTE => {
              translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, argTE)._2
            })
          val resultIT = prototypeI.returnType
          val resultIE = ExternFunctionCallIE(prototypeC, argsIE, prototypeC.returnType)
          (resultIT, resultIE)
        }
        case ConstructTE(structTT, resultReference, args) => {
          val resultIT =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultReference)

          //          val freePrototype = translatePrototype(freePrototypeT)
          //          // They might disagree on the ownership, and thats fine.
          //          // That free prototype is only going to take an owning or a share reference, and we'll only
          //          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          val argsIE =
            args.map(argTE => {
              translateExpr(
                denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, argTE)._2
            })

          val structIT =
            translateStruct(
              denizenName,
              denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,
              structTT,
              translateBoundArgsForCallee(
                denizenName,
                denizenBoundToDenizenCallerSuppliedThing,
                substitutions,
                perspectiveRegionT,
                hinputs.getInstantiationBoundArgs(structTT.id)))

          val resultIE =
            ConstructIE(
              StructIT(RegionCollapser.collapseStructId(RegionCounter.countStructId(structIT.id), structIT.id)),
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT),
              argsIE)
          (resultIT, resultIE)
        }
        case DestroyTE(exprT, structTT, destinationReferenceVariables) => {
          val (sourceIT, sourceIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, exprT)

          val structIT =
            translateStructFullName(
              denizenName,
              denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,
              structTT.id,
              translateBoundArgsForCallee(
                denizenName,
                denizenBoundToDenizenCallerSuppliedThing,
                substitutions,
                perspectiveRegionT,
                hinputs.getInstantiationBoundArgs(structTT.id)))

//          val resultT =
//            expr.result.coord.kind match {
//              case s @ StructIT(_) => s
//              case other => vwat(other)
//            }

//          val structDef = vassertSome(monouts.structs.get(resultT.id))
//          vassert(structDef.members.size == destinationReferenceVariables.size)

          val resultIE =
            DestroyIE(
              sourceIE,
              StructIT(RegionCollapser.collapseStructId(RegionCounter.countStructId(structIT), structIT)),
              destinationReferenceVariables.map(destRefVarT => {
                translateReferenceLocalVariable(
                  denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions,
                  perspectiveRegionT, destRefVarT)._2
              }))
          (CoordI[sI](MutableShareI, VoidIT()), resultIE)
        }
        case DestroyStaticSizedArrayIntoLocalsTE(exprT, ssaTT, destinationReferenceVariables) => {
          val (sourceIT, sourceIE) = translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, exprT)
          val (ssaIT, size) =
            sourceIT.kind match {
              case s @ StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, size, _, _))) => (s, size)
              case other => vwat(other)
            }

          vassert(size == destinationReferenceVariables.size)
          val resultCE =
            DestroyStaticSizedArrayIntoLocalsIE(
              sourceIE,
              RegionCollapser.collapseStaticSizedArray(RegionCounter.countStaticSizedArray(ssaIT), ssaIT),
            destinationReferenceVariables.map(destRefVarT => {
              translateReferenceLocalVariable(
                denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, destRefVarT)._2
            }))
          (CoordI[sI](MutableShareI, VoidIT()), resultCE)
        }
        case MutateTE(destinationTT, sourceExpr) => {
          // DO NOT SUBMIT change all IE to CE like this one
          val (destinationIT, destinationCE) =
            translateAddrExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, destinationTT)
          val (sourceIT, sourceCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, sourceExpr)
          val resultIT = destinationIT
          val resultCE = MutateIE(destinationCE, sourceCE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)
        }
        case u @ UpcastTE(innerExprUnsubstituted, targetSuperKind, untranslatedImplFullName) => {
          val implFullName =
            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,

              untranslatedImplFullName,
              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(untranslatedImplFullName)))
          //          val freePrototype = translatePrototype(freePrototypeT)
          val resultIT =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, u.result.coord)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          val (innerIT, innerCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerExprUnsubstituted)

          val superKindI =
            translateSuperKind(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, targetSuperKind)

          val resultCE =
            UpcastIE(
              innerCE,
              vimpl(),//RegionCollapser.collapseSuperKind(superKindI),
              RegionCollapser.collapseImplId(RegionCounter.countImplId(implFullName), implFullName),
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))//,
          //            freePrototype)
          vimpl()
        }
        case IfTE(condition, thenCall, elseCall) => {
          val (conditionIT, conditionCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, condition)
          val (thenIT, thenCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, thenCall)
          val (elseIT, elseCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, elseCall)
          (thenIT, elseIT) match {
            case (a, b) if a == b =>
            case (_, CoordI(_, NeverIT(_))) =>
            case (CoordI(_, NeverIT(_)), _) =>
            case other => vwat(other)
          }
          val resultIT = thenIT

          val resultCE = IfIE(conditionCE, thenCE, elseCE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)
        }
        case IsSameInstanceTE(left, right) => {
          val (leftIT, leftCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, left)
          val (rightIT, rightCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, right)
          val resultCE = IsSameInstanceIE(leftCE, rightCE)
          (CoordI[sI](MutableShareI, BoolIT()), resultCE)
        }
        case StaticArrayFromValuesTE(elements, resultReference, arrayType) => {

          //          val freePrototype = translatePrototype(freePrototypeT)
          val resultIT = translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultReference)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          val elementsCE =
            elements.map(elementTE => {
              translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, elementTE)._2
            })

          val ssaTT =
            translateStaticSizedArray(
              denizenName,
              denizenBoundToDenizenCallerSuppliedThing,
              substitutions,
              perspectiveRegionT,
              arrayType)

          val resultCE =
            StaticArrayFromValuesIE(
              elementsCE,
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT),
              RegionCollapser.collapseStaticSizedArray(RegionCounter.countStaticSizedArray(ssaTT), ssaTT))
          (resultIT, resultCE)
        }
        case DeferTE(innerExpr, deferredExpr) => {
          val (innerIT, innerCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerExpr)
          val (deferredIT, deferredCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, deferredExpr)
          val resultIT = innerIT
          val resultCE =
            DeferIE(innerCE, deferredCE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)
        }
        case LetAndLendTE(variable, sourceExprT, outerOwnershipT) => {
          val (sourceSubjectiveIT, sourceIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, sourceExprT)

          val resultOwnershipI =
            translateOwnership(
              substitutions,
              perspectiveRegionT,
              composeOwnerships(outerOwnershipT, sourceExprT.result.coord.ownership),
              sourceExprT.result.coord.region)

          val resultIT = CoordI(resultOwnershipI, sourceSubjectiveIT.kind)

          val (localIT, localI) =
            translateLocalVariable(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, variable)

          val resultIE =
            LetAndLendIE(
              localI,
              sourceIE,
              resultOwnershipI,
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultIE)
        }
        case BorrowToWeakTE(innerExpr) => {
          vimpl()//BorrowToWeakIE(translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerExpr))
        }
        case WhileTE(BlockTE(inner)) => {
          val (innerIT, innerCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, inner)

          // While loops must always produce void.
          // If we want a foreach/map/whatever construct, the loop should instead
          // add things to a list inside; WhileIE shouldnt do it for it.
          val resultIT =
            innerIT match {
              case CoordI(_, VoidIT()) => innerIT
              case CoordI(_, NeverIT(true)) => CoordI[sI](MutableShareI, VoidIT())
              case CoordI(_, NeverIT(false)) => innerIT
              case _ => vwat()
            }

          val resultCE =
            WhileIE(
              BlockIE(innerCE, collapseCoord(RegionCounter.countCoord(innerIT), innerIT)),
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)
        }
        case BreakTE(region) => {
          val resultCE = BreakIE()
          (CoordI[sI](MutableShareI, NeverIT(true)), resultCE)
        }
        case LockWeakTE(innerExpr, resultOptBorrowType, someConstructor, noneConstructor, someImplUntranslatedFullName, noneImplUntranslatedFullName) => {
          vimpl()
//          LockWeakIE(
//            translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, innerExpr),
//            translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultOptBorrowType),
//            translatePrototype(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, someConstructor),
//            translatePrototype(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, noneConstructor),
//            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//              substitutions,
//              perspectiveRegionT,
//
//              someImplUntranslatedFullName,
//              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//                substitutions,
//                perspectiveRegionT,
//
//                hinputs.getInstantiationBoundArgs(someImplUntranslatedFullName))),
//            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//              substitutions,
//              perspectiveRegionT,
//
//              noneImplUntranslatedFullName,
//              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//                substitutions,
//                perspectiveRegionT,
//
//                hinputs.getInstantiationBoundArgs(noneImplUntranslatedFullName))))
        }
        case DestroyStaticSizedArrayIntoFunctionTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          val (arrayIT, arrayCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val ssaIT =
            translateStaticSizedArray(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, arrayType)
          val (consumerIT, consumerCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, consumer)
          val (consumerPrototypeI, consumerPrototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, consumerMethod)
          val resultCE =
            DestroyStaticSizedArrayIntoFunctionIE(
              arrayCE, RegionCollapser.collapseStaticSizedArray(RegionCounter.countStaticSizedArray(ssaIT), ssaIT), consumerCE, consumerPrototypeC)
          (CoordI[sI](MutableShareI, VoidIT()), resultCE)
        }
        case NewImmRuntimeSizedArrayTE(arrayType, region, sizeExpr, generator, generatorMethod) => {
          //          val freePrototype = translatePrototype(freePrototypeT)

          val rsaIT =
            translateRuntimeSizedArray(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, arrayType)
          val (sizeIT, sizeCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, sizeExpr)
          val (generatorIT, generatorCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, generator)
          val (generatorPrototypeI, generatorPrototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, generatorMethod)

          val resultIT =
            CoordI[sI](
              rsaIT.mutability match {
                case MutableI => OwnI
                case ImmutableI => MutableShareI
              },
              rsaIT)

          val resultCE =
            NewImmRuntimeSizedArrayIE(
              RegionCollapser.collapseRuntimeSizedArray(RegionCounter.countRuntimeSizedArray(rsaIT), rsaIT),
              sizeCE,
              generatorCE,
              generatorPrototypeC,
              RegionCollapser.collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)

          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

        }
        case StaticArrayFromCallableTE(arrayType, region, generator, generatorMethod) => {
          //          val freePrototype = translatePrototype(freePrototypeT)

          val ssaIT =
            translateStaticSizedArray(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, arrayType)
          val (generatorIT, generatorCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, generator)
          val (generatorPrototypeI, generatorPrototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, generatorMethod)

          val resultIT =
            CoordI[sI](
              ssaIT.mutability match {
                case MutableI => OwnI
                case ImmutableI => MutableShareI
              },
              ssaIT)

          val resultCE =
            StaticArrayFromCallableIE(
              collapseStaticSizedArray(RegionCounter.countStaticSizedArray(ssaIT), ssaIT),
              generatorCE,
              generatorPrototypeC,
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))

          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          (resultIT, resultCE)
        }
        case RuntimeSizedArrayCapacityTE(arrayExpr) => {
          val (arrayIT, arrayCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val resultCE = RuntimeSizedArrayCapacityIE(arrayCE)
          (CoordI[sI](MutableShareI, IntIT(32)), resultCE)
        }
        case PushRuntimeSizedArrayTE(arrayExpr, newElementExpr) => {
          val (arrayIT, arrayCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val (elementIT, elementCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, newElementExpr)
          val resultCE = PushRuntimeSizedArrayIE(arrayCE, elementCE)
          (CoordI[sI](MutableShareI, VoidIT()), resultCE)
        }
        case PopRuntimeSizedArrayTE(arrayExpr) => {
          val (arrayIT, arrayCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val elementIT =
            arrayIT.kind match {
              case RuntimeSizedArrayIT(IdI(_, _, RuntimeSizedArrayNameI(_, RawArrayNameI(_, elementType, _)))) => elementType
              case other => vwat(other)
            }
          val resultCE = PopRuntimeSizedArrayIE(arrayCE, collapseCoord(RegionCounter.countCoord(elementIT), elementIT))
          (elementIT, resultCE)
        }
        case ArrayLengthTE(arrayExpr) => {
          val (arrayIT, arrayCE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val resultIT = CoordI[sI](MutableShareI, IntIT(32))
          val resultCE = ArrayLengthIE(arrayCE)
          (resultIT, resultCE)
        }
        case DestroyImmRuntimeSizedArrayTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          val (arrayIT, arrayCE) = translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val rsaIT = translateRuntimeSizedArray(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, arrayType)
          val (consumerIT, consumerCE) = translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, consumer)
          val (prototypeI, prototypeC) =
            translatePrototype(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, consumerMethod)

          val resultCE =
            DestroyImmRuntimeSizedArrayIE(
              arrayCE,
              collapseRuntimeSizedArray(RegionCounter.countRuntimeSizedArray(rsaIT), rsaIT),
              consumerCE,
              prototypeC)
          (CoordI[sI](MutableShareI, VoidIT()), resultCE)
        }
        case DestroyMutRuntimeSizedArrayTE(arrayExpr) => {
          val (arrayIT, arrayIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, arrayExpr)
          val resultIE = DestroyMutRuntimeSizedArrayIE(arrayIE)
          (CoordI.void[sI], resultIE)
        }
        case NewMutRuntimeSizedArrayTE(arrayTT, region, capacityExpr) => {
          val arrayIT =
            translateRuntimeSizedArray(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, arrayTT)
          val resultIT =
            CoordI[sI](
              arrayIT.mutability match {
                case MutableI => OwnI
                case ImmutableI => MutableShareI
              },
              arrayIT)

          val (capacityIT, capacityIE) =
            translateRefExpr(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, capacityExpr)

          val resultCE =
            NewMutRuntimeSizedArrayIE(
              RegionCollapser.collapseRuntimeSizedArray(RegionCounter.countRuntimeSizedArray(arrayIT), arrayIT),
              capacityIE,
              collapseCoord(RegionCounter.countCoord(resultIT), resultIT))
          (resultIT, resultCE)
        }
        case TupleTE(elements, resultReference) => {
          val elementsCE =
            elements.map(elementTE => {
              translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, elementTE)._2
            })

          val resultIT =
            translateCoord(
              denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultReference)

          (resultIT, TupleIE(elementsCE, collapseCoord(RegionCounter.countCoord(resultIT), resultIT)))
        }
        case AsSubtypeTE(sourceExpr, targetSubtype, resultResultType, okConstructor, errConstructor, implFullNameT, okResultImplFullNameT, errResultImplFullNameT) => {
          vimpl()
//          AsSubtypeIE(
//            translateRefExpr(denizenName, denizenBoundToDenizenCallerSuppliedThing, env, substitutions, perspectiveRegionT, sourceExpr),
//            translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, targetSubtype),
//            translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, resultResultType),
//            translatePrototype(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, okConstructor),
//            translatePrototype(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, errConstructor),
//            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//              substitutions,
//              perspectiveRegionT,
//
//              implFullNameT,
//              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//                substitutions,
//                perspectiveRegionT,
//
//                hinputs.getInstantiationBoundArgs(implFullNameT))),
//            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//              substitutions,
//              perspectiveRegionT,
//
//              okResultImplFullNameT,
//              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//                substitutions,
//                perspectiveRegionT,
//
//                hinputs.getInstantiationBoundArgs(okResultImplFullNameT))),
//            translateImplFullName(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//            substitutions,
//              perspectiveRegionT,
//
//              errResultImplFullNameT,
//              translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
//                substitutions,
//                perspectiveRegionT,
//
//                hinputs.getInstantiationBoundArgs(errResultImplFullNameT))))
        }
        case other => vimpl(other)
      }
    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(resultRefExpr, { case PlaceholderNameT(_) => }).isEmpty)
    //    }
    (resultIT, resultCE)
  }

  def translateOwnership(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    ownershipT: OwnershipT,
    regionT: ITemplataT[RegionTemplataType]):
  OwnershipI = {
    ownershipT match { // Now  if it's a borrow, figure out whether it's mutable or immutable
      case OwnT => OwnI
      case BorrowT => {
        if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(regionT))) {
          MutableBorrowI
        } else {
          ImmutableBorrowI
        }
      }
      case ShareT => {
        if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(regionT))) {
          MutableShareI
        } else {
          ImmutableShareI
        }
      }
      case WeakT => vimpl()
    }
  }

  def composeOwnerships(
    outerOwnership: OwnershipT,
    innerOwnership: OwnershipT):
  OwnershipT = {
    (outerOwnership, innerOwnership) match {
      case (OwnT, OwnT) => OwnT
      case (OwnT, BorrowT) => BorrowT
      case (BorrowT, OwnT) => BorrowT
      case (BorrowT, BorrowT) => BorrowT
      case (BorrowT, WeakT) => WeakT
      case (BorrowT, ShareT) => ShareT
      case (WeakT, OwnT) => WeakT
      case (WeakT, BorrowT) => WeakT
      case (WeakT, WeakT) => WeakT
      case (WeakT, ShareT) => ShareT
      case (ShareT, ShareT) => ShareT
      case (OwnT, ShareT) => ShareT
      case other => vwat(other)
    }
  }

  private def coordRegionIsMutable(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    coord: CoordT):
  Boolean = {
    regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(coord.region))
  }


  private def regionIsMutable(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    region: IdT[RegionPlaceholderNameT]):
  Boolean = {
    val RegionPlaceholderNameT(_, _, regionPureHeight) = region.localName

    val perspectiveActualPureHeight =
      ITemplataI.expectRegionTemplata(
        vassertSome(vassertSome(substitutions.get(perspectiveRegionT.initFullName(interner)))
          .get(perspectiveRegionT)))
        .pureHeight

    val regionActualPureHeight =
      ITemplataI.expectRegionTemplata(
        vassertSome(vassertSome(substitutions.get(region.initFullName(interner)))
          .get(region)))
        .pureHeight

    perspectiveActualPureHeight == regionActualPureHeight
  }

  def translateFunctionFullName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IFunctionNameT]):
  IdI[sI, IFunctionNameI[sI]] = {
    val IdT(module, steps, last) = fullNameT
    val fullName =
      IdI(
        module,
        steps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        translateFunctionName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, last))
    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(fullName, { case PlaceholderNameT(_) => }).isEmpty)
    //    }
    fullName
  }

//  def translateRegionFullName(
//    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplata[ITemplataType]]],
//    perspectiveRegionT: IdT[RegionPlaceholderNameT],
//    fullNameT: IdT[IRegionNameT]//,
//    //instantiationBoundArgs: InstantiationBoundArguments
//  ):
//  IdT[IRegionNameT] = {
//    fullNameT match {
//      case IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _, _)) => {
//          IdT(
//            packageCoord,
//            initSteps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
//            translateRegionName(substitutions, perspectiveRegionT, r))
//      }
//    }
//  }

  def translateStructFullName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IStructNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IdI[sI, IStructNameI[sI]] = {
    val IdT(module, steps, lastT) = fullNameT

    val fullNameI =
      IdI(
        module,
        steps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        translateStructName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, lastT))


    collapseAndTranslateStructDefinition(
      opts, interner, keywords, hinputs, monouts, fullNameT, fullNameI, instantiationBoundArgs)

    fullNameI
  }

  def translateInterfaceFullName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IInterfaceNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IdI[sI, IInterfaceNameI[sI]] = {
    val IdT(module, steps, last) = fullNameT
    val newFullNameI =
      IdI(
        module,
        steps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        translateInterfaceName(denizenName, denizenBoundToDenizenCallerSuppliedThing,substitutions, perspectiveRegionT, last))


    collapseAndTranslateInterfaceDefinition(
      opts, interner, keywords, hinputs, monouts, fullNameT, newFullNameI, instantiationBoundArgs)

    newFullNameI
  }

  def translateCitizenName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    t: ICitizenNameT):
  ICitizenNameI[sI] = {
    t match {
      case s : IStructNameT => translateStructName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, s)
      case i : IInterfaceNameT => translateInterfaceName(denizenName, denizenBoundToDenizenCallerSuppliedThing,substitutions, perspectiveRegionT, i)
    }
  }

  def translateFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    id: IdT[INameT]):
  IdI[sI, INameI[sI]] = {
    id match {
      case other => vimpl(other)
    }
  }

  def translateCitizenFullName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    id: IdT[ICitizenNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IdI[sI, ICitizenNameI[sI]] = {
    id match {
      case IdT(module, steps, last : IStructNameT) => {
        translateStructFullName(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, IdT(module, steps, last), instantiationBoundArgs)
      }
      case IdT(module, steps, last : IInterfaceNameT) => {
        translateInterfaceFullName(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, IdT(module, steps, last), instantiationBoundArgs)
      }
      case other => vimpl(other)
    }
  }

  def translateImplFullName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IImplNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IdI[sI, IImplNameI[sI]] = {
    val IdT(module, steps, last) = fullNameT
    val fullNameI =
      IdI(
        module,
        steps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        translateImplName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, last, instantiationBoundArgs))
    val fullNameC = vimpl(fullNameI)

    fullNameT match {
      case IdT(packageCoord, initSteps, name@ImplBoundNameT(_, _)) => {
        val implBoundNameT = IdT(packageCoord, initSteps, name)
        val result =
          vassertSome(
            denizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl.get(implBoundNameT))
        //        if (opts.sanityCheck) {
        //          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        //        }
        result
      }
      case IdT(_, _, _) => {
        monouts.newImpls.enqueue((fullNameT, fullNameC, instantiationBoundArgs))
        fullNameI
      }
    }
  }

  def translateCoord(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    coord: CoordT):
  CoordI[sI] = {
    val CoordT(outerOwnership, outerRegion, kind) = coord
    kind match {
      case KindPlaceholderT(placeholderFullName) => {
        // Let's get the index'th placeholder from the top level denizen.
        // If we're compiling a function or a struct, it might actually be a lambda function or lambda struct.
        // In these cases, the topLevelDenizenPlaceholderIndexToTemplata actually came from the containing function,
        // see LHPCTLD.

        vassertSome(vassertSome(substitutions.get(placeholderFullName.initFullName(interner))).get(placeholderFullName)) match {
          case CoordTemplataI(CoordI(innerOwnership, kind)) => {
            val combinedOwnership =
              ((outerOwnership, innerOwnership) match {
                case (OwnT, OwnI) => OwnI
                case (OwnT, MutableShareI) => {
                  if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(outerRegion))) {
                    MutableShareI
                  } else {
                    ImmutableShareI
                  }
                }
//                case (OwnT, BorrowT) => BorrowT
//                case (BorrowT, OwnT) => BorrowT
//                case (BorrowT, BorrowT) => BorrowT
//                case (BorrowT, WeakT) => WeakT
//                case (BorrowT, ShareT) => ShareT
//                case (WeakT, OwnT) => WeakT
//                case (WeakT, BorrowT) => WeakT
//                case (WeakT, WeakT) => WeakT
//                case (WeakT, ShareT) => ShareT
//                case (ShareT, ShareT) => ShareT
//                case (OwnT, ShareT) => ShareT
                case other => vwat(other)
                  // DO NOT SUBMIT combine this with what's elsewhere in this file
              })
//            vassert(innerRegion == translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, outerRegion))
            CoordI(combinedOwnership, kind)
          }
          case KindTemplataI(kind) => {
//            val newOwnership =
//              getMutability(kind) match {
//                case ImmutableT => ShareT
//                case MutableT => outerOwnership
//              }
            CoordI(vimpl(/*newOwnership*/), vimpl(kind))
          }
        }
      }
      case other => {
        // We could, for example, be translating an Vector<myFunc$0, T> (which is temporarily regarded mutable)
        // to an Vector<imm, int> (which is immutable).
        // So, we have to check for that here and possibly make the ownership share.
        val kind = translateKind(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, other)
        val newOwnership =
          kind match {
            case IntIT(_) | BoolIT() | VoidIT() => {
              // We don't want any ImmutableShareH for primitives, it's better to only ever have one
              // ownership for primitives.
              MutableShareI
            }
            case _ => {
              val mutability = getMutability(RegionCollapser.collapseKind(RegionCounter.countKind(kind), kind))
              ((outerOwnership, mutability) match {
                case (_, ImmutableI) => ShareT
                case (other, MutableI) => other
              }) match { // Now  if it's a borrow, figure out whether it's mutable or immutable
                case BorrowT => {
                  if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(outerRegion))) {
                    MutableBorrowI
                  } else {
                    ImmutableBorrowI
                  }
                }
                case ShareT => {
                  if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(outerRegion))) {
                    MutableShareI
                  } else {
                    ImmutableShareI
                  }
                }
                case OwnT => {
                  // We don't have this assert because we sometimes can see owning references even
                  // though we dont hold them, see RMLRMO.
                  // vassert(regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(outerRegion)))
                  OwnI
                }
                case WeakT => vimpl()
              }
            }
          }
//        val newRegion = expectRegionTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, outerRegion))
        CoordI(newOwnership, translateKind(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, other))
      }
    }
  }

  def getMutability(t: KindIT[cI]): MutabilityI = {
    t match {
      case IntIT(_) | BoolIT() | StrIT() | NeverIT(_) | FloatIT() | VoidIT() => ImmutableI
      case StructIT(name) => {
        vassertSome(monouts.structToMutability.get(name))
      }
      case InterfaceIT(name) => {
        vassertSome(monouts.interfaceToMutability.get(name))
      }
      case RuntimeSizedArrayIT(IdI(_, _, RuntimeSizedArrayNameI(_, RawArrayNameI(mutability, _, region)))) => {
        mutability
      }
      case StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, _, _, RawArrayNameI(mutability, _, region)))) => {
        mutability
      }
      case other => vimpl(other)
    }
  }

  def translateCitizen(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    citizen: ICitizenTT,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  ICitizenIT[sI] = {
    citizen match {
      case s @ StructTT(_) => translateStruct(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, s, instantiationBoundArgs)
      case s @ InterfaceTT(_) => translateInterface(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, s, instantiationBoundArgs)
    }
  }

  def translateStruct(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    struct: StructTT,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  StructIT[sI] = {
    val StructTT(fullName) = struct

    val desiredStruct =
      StructIT(
        translateStructFullName(
          denizenName, denizenBoundToDenizenCallerSuppliedThing,
          substitutions, perspectiveRegionT, fullName, instantiationBoundArgs))

    desiredStruct
  }

  def translateInterface(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    interface: InterfaceTT,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  InterfaceIT[sI] = {
    val InterfaceTT(fullName) = interface

    val desiredInterface =
      InterfaceIT(
        translateInterfaceFullName(
          denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, fullName, instantiationBoundArgs))

    desiredInterface
  }

  def translateSuperKind(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    kind: ISuperKindTT):
  ISuperKindIT[sI] = {
    kind match {
      case i @ InterfaceTT(_) => {
        translateInterface(
          denizenName,
          denizenBoundToDenizenCallerSuppliedThing,
          substitutions,
          perspectiveRegionT,
          i,
          translateBoundArgsForCallee(
            denizenName,
            denizenBoundToDenizenCallerSuppliedThing,
            substitutions,
            perspectiveRegionT,
            hinputs.getInstantiationBoundArgs(i.id)))
      }
      case p @ KindPlaceholderT(_) => {
        translatePlaceholder(substitutions, p) match {
          case s : ISuperKindIT[sI] => s
          case other => vwat(other)
        }
      }
    }
  }

  def translatePlaceholder(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    t: KindPlaceholderT):
  KindIT[sI] = {
    val newSubstitutingTemplata =
      vassertSome(
        vassertSome(substitutions.get(t.id.initFullName(interner)))
        .get(t.id))
    ITemplataI.expectKindTemplata(newSubstitutingTemplata).kind
  }

  def translateStaticSizedArray(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    ssaTT: StaticSizedArrayTT):
  StaticSizedArrayIT[sI] = {
    val StaticSizedArrayTT(
    IdT(
    packageCoord,
    initSteps,
    StaticSizedArrayNameT(StaticSizedArrayTemplateNameT(), sizeT, variabilityT, RawArrayNameT(mutabilityT, elementTypeT, ssaRegionT)))) = ssaTT

    val newPerspectiveRegionT =
      ssaRegionT match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    // We use newPerspectiveRegionT for these because of TTTDRM.
    val ssaRegion = ITemplataI.expectRegionTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, ssaRegionT))
    // We dont have this assert because this might be a templata deep in a struct or function's
    // name, so the heights might actually be negative.
    // vassert(Some(ssaRegion.pureHeight) == newPerspectiveRegionT.localName.pureHeight)
    val intTemplata = ITemplataI.expectIntegerTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, sizeT)).value
    val variabilityTemplata = ITemplataI.expectVariabilityTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, variabilityT)).variability
    val mutabilityTemplata =
      ITemplataI.expectMutabilityTemplata(
        translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, mutabilityT)).mutability
    val elementType = translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, elementTypeT)

    StaticSizedArrayIT(
      IdI(
        packageCoord,
        initSteps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        StaticSizedArrayNameI(
          StaticSizedArrayTemplateNameI(),
          intTemplata,
          variabilityTemplata,
          RawArrayNameI(
            mutabilityTemplata,
            elementType,
            ssaRegion))))
  }

  def translateRuntimeSizedArray(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    ssaTT: RuntimeSizedArrayTT):
  RuntimeSizedArrayIT[sI] = {
    val RuntimeSizedArrayTT(
      IdT(
      packageCoord,
      initSteps,
      RuntimeSizedArrayNameT(RuntimeSizedArrayTemplateNameT(), RawArrayNameT(mutabilityT, elementTypeT, rsaRegionT)))) = ssaTT

    val newPerspectiveRegionT =
      rsaRegionT match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    // We use newPerspectiveRegionT for these because of TTTDRM.
    val rsaRegion = ITemplataI.expectRegionTemplata(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, rsaRegionT))
    // We dont have this assert because this might be a templata deep in a struct or function's
    // name, so the heights might actually be negative.
    // vassert(Some(ssaRegion.pureHeight) == newPerspectiveRegionT.localName.pureHeight)
    val mutabilityTemplata =
      ITemplataI.expectMutabilityTemplata(
        translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, mutabilityT)).mutability
    val elementType = translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, elementTypeT)

    RuntimeSizedArrayIT(
      IdI(
        packageCoord,
        initSteps.map(translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
        RuntimeSizedArrayNameI(
          RuntimeSizedArrayTemplateNameI(),
          RawArrayNameI(
            mutabilityTemplata,
            elementType,
            rsaRegion))))
  }

  def translateKind(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    kind: KindT):
  KindIT[sI] = {
    kind match {
      case IntT(bits) => IntIT(bits)
      case BoolT() => BoolIT()
      case FloatT() => FloatIT()
      case VoidT() => VoidIT()
      case StrT() => StrIT()
      case NeverT(fromBreak) => NeverIT(fromBreak)
      case p @ KindPlaceholderT(_) => translatePlaceholder(substitutions, p)
      case s @ StructTT(_) => {
        translateStruct(
          denizenName,
          denizenBoundToDenizenCallerSuppliedThing,
          substitutions,
          perspectiveRegionT,
          s,
          translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
            substitutions, perspectiveRegionT, hinputs.getInstantiationBoundArgs(s.id)))
      }
      case s @ InterfaceTT(_) => {
        translateInterface(
          denizenName,
          denizenBoundToDenizenCallerSuppliedThing,
          substitutions,
          perspectiveRegionT,
          s,
          translateBoundArgsForCallee(
            denizenName, denizenBoundToDenizenCallerSuppliedThing,
            substitutions, perspectiveRegionT, hinputs.getInstantiationBoundArgs(s.id)))
      }
      case a @ contentsStaticSizedArrayTT(_, _, _, _, _) => translateStaticSizedArray(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, a)
      case a @ contentsRuntimeSizedArrayTT(_, _, _) => translateRuntimeSizedArray(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, a)
      case other => vimpl(other)
    }
  }

  def translateParameter(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    param: ParameterT):
  ParameterI = {
    val ParameterT(name, virtuality, tyype) = param
    val typeIT =
      translateCoord(
        denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, tyype)
    val nameI = translateVarName(name)
    ParameterI(
      RegionCollapser.collapseVarName(RegionCounter.countVarName(nameI), nameI),
      virtuality.map({ case AbstractT() => AbstractI() }),
      collapseCoord(RegionCounter.countCoord(typeIT), typeIT))
  }

  def translateTemplata(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    templata: ITemplataT[ITemplataType]):
  ITemplataI[sI] = {
    val result =
      templata match {
        case PlaceholderTemplataT(n, tyype) => {
          val substitution =
            vassertSome(vassertSome(substitutions.get(n.initFullName(interner))).get(n))
          tyype match {
            case RegionTemplataType() => {
              n match {
                case IdT(_, _, RegionPlaceholderNameT(_, _, maybeLocalPureHeightT)) => {
                  RegionTemplataI[sI](
                    maybeLocalPureHeightT match {
                      case Some(n) => n
                      case None => expectRegionTemplata(substitution).pureHeight
                    })
                }
                case other => vwat(other)
              }
            }
            case _ => substitution
          }
        }
        case IntegerTemplataT(value) => IntegerTemplataI[sI](value)
        case BooleanTemplataT(value) => BooleanTemplataI[sI](value)
        case StringTemplataT(value) => StringTemplataI[sI](value)
        case CoordTemplataT(coord) => CoordTemplataI[sI](translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, coord))
        case MutabilityTemplataT(mutability) => MutabilityTemplataI[sI](translateMutability(mutability))
        case VariabilityTemplataT(variability) => VariabilityTemplataI[sI](translateVariability(variability))
        case KindTemplataT(kind) => KindTemplataI[sI](translateKind(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, kind))
        case other => vimpl(other)
      }
    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case KindPlaceholderNameT(_) => }).isEmpty)
    }
    result
  }

  def translateVarName(
    name: IVarNameT):
  IVarNameI[sI] = {
    name match {
      case TypingPassFunctionResultVarNameT() => TypingPassFunctionResultVarNameI()
      case CodeVarNameT(x) => CodeVarNameI(x)
      case ClosureParamNameT(x) => ClosureParamNameI(x)
      case TypingPassBlockResultVarNameT(LocationInFunctionEnvironmentT(path)) => TypingPassBlockResultVarNameI(LocationInFunctionEnvironmentI(path))
      case TypingPassTemporaryVarNameT(LocationInFunctionEnvironmentT(path)) => TypingPassTemporaryVarNameI(LocationInFunctionEnvironmentI(path))
      case ConstructingMemberNameT(x) => ConstructingMemberNameI(x)
      case IterableNameT(range) => IterableNameI(range)
      case IteratorNameT(range) => IteratorNameI(range)
      case IterationOptionNameT(range) => IterationOptionNameI(range)
      case MagicParamNameT(codeLocation2) => MagicParamNameI(codeLocation2)
      case SelfNameT() => SelfNameI()
      case other => vimpl(other)
    }
  }

  def translateFunctionName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IFunctionNameT):
  IFunctionNameI[sI] = {
    name match {
      case FunctionNameT(FunctionTemplateNameT(humanName, codeLoc), templateArgs, params) => {
        FunctionNameIX(
          FunctionTemplateNameI(humanName, codeLoc),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case ForwarderFunctionNameT(ForwarderFunctionTemplateNameT(innerTemplate, index), inner) => {
        ForwarderFunctionNameI(
          ForwarderFunctionTemplateNameI(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            vimpl(innerTemplate),
            index),
          translateFunctionName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, inner))
      }
      case ExternFunctionNameT(humanName, parameters) => {
        ExternFunctionNameI(
          humanName, parameters.map(translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case FunctionBoundNameT(FunctionBoundTemplateNameT(humanName, codeLocation), templateArgs, params) => {
        FunctionBoundNameI(
          FunctionBoundTemplateNameI(humanName, codeLocation),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case AnonymousSubstructConstructorNameT(template, templateArgs, params) => {
        AnonymousSubstructConstructorNameI(
          translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, template) match {
            case x @ AnonymousSubstructConstructorTemplateNameI(_) => x
            case other => vwat(other)
          },
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case LambdaCallFunctionNameT(LambdaCallFunctionTemplateNameT(codeLocation, paramTypesForGeneric), templateArgs, paramTypes) => {
        LambdaCallFunctionNameI(
          LambdaCallFunctionTemplateNameI(
            codeLocation,
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            vimpl(paramTypesForGeneric)),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          paramTypes.map(translateCoord(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case other => vimpl(other)
    }
  }

  def translateImplName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IImplNameT,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IImplNameI[sI] = {
    name match {
      case ImplNameT(ImplTemplateNameT(codeLocationS), templateArgs, subCitizen) => {
        ImplNameI(
          ImplTemplateNameI(codeLocationS),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          translateCitizen(denizenName, denizenBoundToDenizenCallerSuppliedThing,
            substitutions,
            perspectiveRegionT,
            subCitizen,
            vimpl(hinputs.getInstantiationBoundArgs(subCitizen.id))))
      }
      case ImplBoundNameT(ImplBoundTemplateNameT(codeLocationS), templateArgs) => {
        ImplBoundNameI(
          ImplBoundTemplateNameI(codeLocationS),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case AnonymousSubstructImplNameT(AnonymousSubstructImplTemplateNameT(interface), templateArgs, subCitizen) => {
        AnonymousSubstructImplNameI(
          AnonymousSubstructImplTemplateNameI(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            vimpl(interface)),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)),
          translateCitizen(denizenName, denizenBoundToDenizenCallerSuppliedThing,
            substitutions,
            perspectiveRegionT,
            subCitizen,
            vimpl(hinputs.getInstantiationBoundArgs(subCitizen.id))))
      }
    }
  }

//  def translateRegionName(
//    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplata[ITemplataType]]],
//    perspectiveRegionT: IdT[RegionPlaceholderNameT],
//    name: IRegionNameT):
//  IRegionNameT = {
//    name match {
//      case RegionPlaceholderNameT(index, rune, originallyIntroducedLocation, originallyMutable) => {
//
//      }
//    }
//  }

  def translateStructName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    // See TTTDRM, this is the region from which we're determining other regions' mutabilities.
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IStructNameT):
  IStructNameI[sI] = {
    val newPerspectiveRegionT =
      vassertSome(name.templateArgs.lastOption) match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }
    name match {
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        StructNameI(
          StructTemplateNameI(humanName),
          // We use newPerspectiveRegionT here because of TTTDRM.
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, _)))
      }
      case AnonymousSubstructNameT(AnonymousSubstructTemplateNameT(interface), templateArgs) => {
        AnonymousSubstructNameI(
          AnonymousSubstructTemplateNameI(
            translateInterfaceTemplateName(interface)),
          // We use newPerspectiveRegionT here because of TTTDRM.
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, newPerspectiveRegionT, _)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => {
        LambdaCitizenNameI(LambdaCitizenTemplateNameI(codeLocation))
      }
      case other => vimpl(other)
    }
  }

  def translateInterfaceName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IInterfaceNameT):
  IInterfaceNameI[sI] = {
    name match {
      case InterfaceNameT(InterfaceTemplateNameT(humanName), templateArgs) => {
        InterfaceNameI(
          InterfaceTemplateNameI(humanName),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case other => vimpl(other)
    }
  }

  def translateInterfaceTemplateName(
    name: IInterfaceTemplateNameT):
  IInterfaceTemplateNameI[sI] = {
    name match {
      case InterfaceTemplateNameT(humanName) => InterfaceTemplateNameI(humanName)
      case other => vimpl(other)
    }
  }

  def translateName(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: INameT):
  INameI[sI] = {
    name match {
      case v : IVarNameT => translateVarName(v)
      case KindPlaceholderTemplateNameT(index, _) => vwat()
      case KindPlaceholderNameT(inner) => vwat()
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        StructNameI(
          StructTemplateNameI(humanName),
          templateArgs.map(translateTemplata(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, _)))
      }
      case ForwarderFunctionTemplateNameT(inner, index) => {
        ForwarderFunctionTemplateNameI(
          // We dont translate these, as these are what uniquely identify generics, and we need that
          // information later to map this back to its originating generic.
          // See DMPOGN for a more detailed explanation. This oddity is really tricky.
          vimpl(inner),
          index)
      }
      case AnonymousSubstructConstructorTemplateNameT(substructTemplateName) => {
        AnonymousSubstructConstructorTemplateNameI(
          translateName(denizenName, denizenBoundToDenizenCallerSuppliedThing, substitutions, perspectiveRegionT, substructTemplateName) match {
            case x : ICitizenTemplateNameI[sI] => x
            case other => vwat(other)
          })
      }
      case FunctionTemplateNameT(humanName, codeLoc) => FunctionTemplateNameI(humanName, codeLoc)
      case StructTemplateNameT(humanName) => StructTemplateNameI(humanName)
      case LambdaCitizenTemplateNameT(codeLoc) => LambdaCitizenTemplateNameI(codeLoc)
      case AnonymousSubstructTemplateNameT(interface) => {
        AnonymousSubstructTemplateNameI(
          translateInterfaceTemplateName(interface))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => {
        LambdaCitizenNameI(LambdaCitizenTemplateNameI(codeLocation))
      }
      case InterfaceTemplateNameT(humanNamee) => InterfaceTemplateNameI(humanNamee)
      //      case FreeTemplateNameT(codeLoc) => name
      case f : IFunctionNameT => translateFunctionName(denizenName, denizenBoundToDenizenCallerSuppliedThing,substitutions, perspectiveRegionT, f)
      case other => vimpl(other)
    }
  }

  def translateCollapsedImplDefinition(
    denizenName: IdT[IInstantiationNameT],
    denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]],
    implIdT: IdT[IImplNameT],
    implIdI: IdI[cI, IImplNameI[cI]],
    implDefinition: EdgeT):
  Unit = {
    if (monouts.impls.contains(implIdI)) {
      return
    }

    val citizen =
      translateCitizen(
        denizenName, denizenBoundToDenizenCallerSuppliedThing,
        substitutions,
        vimpl(),
        implDefinition.subCitizen,
        translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
          substitutions,
          vimpl(),
          hinputs.getInstantiationBoundArgs(implDefinition.subCitizen.id)))
    val superInterface =
      translateInterfaceFullName(
        denizenName, denizenBoundToDenizenCallerSuppliedThing,
        substitutions,
        vimpl(),
        implDefinition.superInterface,
        translateBoundArgsForCallee(denizenName, denizenBoundToDenizenCallerSuppliedThing,
          substitutions,
          vimpl(),
          hinputs.getInstantiationBoundArgs(implDefinition.superInterface)))
    monouts.impls.put(implIdI, (vimpl(citizen), vimpl(superInterface), denizenBoundToDenizenCallerSuppliedThing, vimpl(this)))

    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverride.get(vimpl(superInterface)))
      .put(implIdI, mutable.HashMap())
    vassertSome(monouts.interfaceToImpls.get(vimpl(superInterface))).add((implIdT, implIdI))


    vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(vimpl(superInterface)))
      .foreach({ case (abstractFuncPrototype, virtualIndex) =>
        translateOverride(
          opts, interner, keywords, hinputs, monouts, implIdT, implIdI, vimpl(/*abstractFuncPrototypeT*/), abstractFuncPrototype)
      })
  }
}
