package dev.vale.instantiating

import dev.vale.instantiating.ast._
import dev.vale.options.GlobalOptions
import dev.vale._
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
  funcBoundToCallerSuppliedBoundArgFunc: Map[IdT[FunctionBoundNameT], PrototypeI],
  implBoundToCallerSuppliedBoundArgImpl: Map[IdT[ImplBoundNameT], IdI[IImplNameI]])

// Note this has mutable stuff in it.
case class NodeEnvironment(
  maybeParent: Option[NodeEnvironment],
  // We track these because we need to know the original type of the local, see CTOTFIPB.
  nameToLocal: mutable.HashMap[IVarNameT, ILocalVariableT]) {

  def addTranslatedVariable(idT: IVarNameT, translatedLocalVar: ILocalVariableT): Unit = {
    nameToLocal += (idT -> translatedLocalVar)
  }

  def lookupOriginalTranslatedVariable(idT: IVarNameT): ILocalVariableT = {
    vassertSome(nameToLocal.get(idT))
  }
}
class InstantiatedOutputs() {
  val functions: mutable.HashMap[IdI[IFunctionNameI], FunctionDefinitionI] =
    mutable.HashMap[IdI[IFunctionNameI], FunctionDefinitionI]()
  val structs: mutable.HashMap[IdI[IStructNameI], StructDefinitionI] = mutable.HashMap()
  val interfacesWithoutMethods: mutable.HashMap[IdI[IInterfaceNameI], InterfaceDefinitionI] = mutable.HashMap()

  // We can get some recursion if we have a self-referential struct like:
  //   struct Node<T> { value T; next Opt<Node<T>>; }
  // So we need these to short-circuit that nonsense.
  val startedStructs: mutable.HashMap[IdI[IStructNameI], (MutabilityI, DenizenBoundToDenizenCallerBoundArgI)] = mutable.HashMap()
  val startedInterfaces: mutable.HashMap[IdI[IInterfaceNameI], (MutabilityI, DenizenBoundToDenizenCallerBoundArgI)] = mutable.HashMap()

  //  val immKindToDestructor: mutable.HashMap[KindT, PrototypeT] =
  //    mutable.HashMap[KindT, PrototypeT]()

  // We already know from the hinputs that Some<T> implements Opt<T>.
  // In this map, we'll know that Some<int> implements Opt<int>, Some<bool> implements Opt<bool>, etc.
  val interfaceToImpls: mutable.HashMap[IdI[IInterfaceNameI], mutable.HashSet[(IdT[IImplNameT], IdI[IImplNameI])]] =
  mutable.HashMap()
  val interfaceToAbstractFuncToVirtualIndex: mutable.HashMap[IdI[IInterfaceNameI], mutable.HashMap[PrototypeI, Int]] =
    mutable.HashMap()
  val impls:
    mutable.HashMap[
      IdI[IImplNameI],
      (ICitizenIT, IdI[IInterfaceNameI], DenizenBoundToDenizenCallerBoundArgI, Instantiator)] =
    mutable.HashMap()
  // We already know from the hinputs that Opt<T has drop> has func drop(T).
  // In this map, we'll know that Opt<int> has func drop(int).
  val abstractFuncToInstantiatorAndSuppliedPrototypes: mutable.HashMap[IdI[IFunctionNameI], (Instantiator, InstantiationBoundArgumentsI)] =
  mutable.HashMap()
  // This map collects all overrides for every impl. We'll use it to assemble vtables soon.
  val interfaceToImplToAbstractPrototypeToOverride:
    mutable.HashMap[IdI[IInterfaceNameI], mutable.HashMap[IdI[IImplNameI], mutable.HashMap[PrototypeI, OverrideI]]] =
    mutable.HashMap()

  // These are new impls and abstract funcs we discover for interfaces.
  // As we discover a new impl or a new abstract func, we'll later need to stamp a lot more overrides either way.
  val newImpls: mutable.Queue[(IdT[IImplNameT], IdI[IImplNameI], InstantiationBoundArgumentsI)] = mutable.Queue()
  // The int is a virtual index
  val newAbstractFuncs: mutable.Queue[(PrototypeT, PrototypeI, Int, IdI[IInterfaceNameI], InstantiationBoundArgumentsI)] = mutable.Queue()
  val newFunctions: mutable.Queue[(PrototypeT, PrototypeI, InstantiationBoundArgumentsI, Option[DenizenBoundToDenizenCallerBoundArgI])] = mutable.Queue()

  def addMethodToVTable(
    implId: IdI[IImplNameI],
    superInterfaceId: IdI[IInterfaceNameI],
    abstractFuncPrototype: PrototypeI,
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
  def translate(opts: GlobalOptions, interner: Interner, keywords: Keywords, originalHinputs: HinputsT): HinputsI = {
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
    kindExternsT,
    functionExternsT) = originalHinputs

    val monouts = new InstantiatedOutputs()

    val kindExports =
      kindExportsT.map({ case KindExportT(range, tyype, placeholderedExportId, exportedName) =>
  //      val packageName = IdT(exportId.packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
        val exportTemplateId = TemplataCompiler.getTemplate(placeholderedExportId)

        val exportId = vimpl()

        val substitutions = vimpl() //Map(exportId -> assemblePlaceholderMap(hinputs, exportId)),
        val instantiator =
          new Instantiator(
            opts,
            interner,
            keywords,
            originalHinputs,
            monouts,
            exportTemplateId,
            exportId,
            DenizenBoundToDenizenCallerBoundArgI(Map(), Map()))
        KindExportI(
          range,
          instantiator.translateKind(substitutions, vimpl(), tyype),
          exportId,
          exportedName)
      })

    val functionExports =
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
          vassertSome(originalHinputs.lookupFunction(functionTemplateId.localName))
        val maybeNearestPureBlockLocation =
          if (functionTemplate.isPure) Some(LocationInDenizen(Vector())) else None


        val exportId = translateId(exportPlaceholderedIdT, translateExportName)

        val exportTemplateIdT = TemplataCompiler.getExportTemplate(exportPlaceholderedIdT)
        val exportTemplateNameT = exportTemplateIdT.localName
        val exportTemplateIdI = translateId(exportTemplateIdT, translateExportTemplateName)

        val substitutions =
          Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
            exportTemplateIdT -> assemblePlaceholderMap(originalHinputs, exportPlaceholderedIdT))
        val instantiator =
          new Instantiator(
            opts,
            interner,
            keywords,
            originalHinputs,
            monouts,
            exportTemplateIdT,
            exportPlaceholderedIdT,
            DenizenBoundToDenizenCallerBoundArgI(Map(), Map()))
        Collector.all(exportId, { case PlaceholderTemplataT(_, _) => vwat() })
        val prototype = instantiator.translatePrototype(substitutions, perspectiveRegionT, prototypeT)
        Collector.all(prototype, { case PlaceholderTemplataT(_, _) => vwat() })
        FunctionExportI(range, prototype, exportId, exportedName)
      })

    while ({
      // We make structs and interfaces eagerly as we come across them
      // if (monouts.newStructs.nonEmpty) {
      //   val newStructName = monouts.newStructs.dequeue()
      //   DenizenInstantiator.translateStructDefinition(opts, interner, keywords, hinputs, monouts, newStructName)
      //   true
      // } else if (monouts.newInterfaces.nonEmpty) {
      //   val (newInterfaceName, calleeRuneToSuppliedPrototype) = monouts.newInterfaces.dequeue()
      //   DenizenInstantiator.translateInterfaceDefinition(
      //     opts, interner, keywords, hinputs, monouts, newInterfaceName, calleeRuneToSuppliedPrototype)
      //   true
      // } else
      if (monouts.newFunctions.nonEmpty) {
        val (newFuncNameT, newFuncName, instantiationBoundArgs, maybeDenizenBoundToDenizenCallerSuppliedThing) =
          monouts.newFunctions.dequeue()
        Instantiator.translateFunction(
          opts, interner, keywords, originalHinputs, monouts, newFuncNameT, newFuncName, instantiationBoundArgs,
          maybeDenizenBoundToDenizenCallerSuppliedThing)
        true
      } else if (monouts.newImpls.nonEmpty) {
        val (implFullNameT, implFullNameI, instantiationBoundsForUnsubstitutedImpl) = monouts.newImpls.dequeue()
        Instantiator.translateImpl(
          opts, interner, keywords, originalHinputs, monouts, implFullNameT, implFullNameI, instantiationBoundsForUnsubstitutedImpl)
        true
      } else if (monouts.newAbstractFuncs.nonEmpty) {
        val (abstractFuncT, abstractFunc, virtualIndex, interfaceId, instantiationBoundArgs) =
          monouts.newAbstractFuncs.dequeue()
        Instantiator.translateAbstractFunc(
          opts, interner, keywords, originalHinputs, monouts, interfaceId, abstractFuncT, abstractFunc, virtualIndex, instantiationBoundArgs)
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
        Map(),
        kindExports,
        functionExports,
        vimpl(kindExternsT),
        vimpl(functionExternsT))

    if (opts.sanityCheck) {
      Collector.all(resultHinputs, {
        case BorrowT => vfail()
        case ShareT => vfail()
      })
    }

    resultHinputs
  }

  def translateId[T <: INameT, Y <: INameI](idT: IdT[T], func: T => Y): IdI[Y] = {
    val IdT(packageCoord, initStepsT, localNameT) = idT
    IdI(packageCoord, initStepsT.map(translateName(_)), func(localNameT))
  }

  def translateExportName(exportNameT: ExportNameT): ExportNameI = {
    val ExportNameT(ExportTemplateNameT(codeLoc), region) = exportNameT
    ExportNameI(ExportTemplateNameI(codeLoc), translateRegionTemplata(region))
  }

  def translateExportTemplateName(exportTemplateNameT: ExportTemplateNameT): ExportTemplateNameI = {
    val ExportTemplateNameT(codeLoc) = exportTemplateNameT
    ExportTemplateNameI(codeLoc)
  }

  def translateName(t: INameT): INameI = {
    vimpl()
  }

  def translateRegionTemplata(t: ITemplataT[RegionTemplataType]): ITemplataI = {
    vimpl()
  }

  def translateInterfaceDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    interfaceIdT: IdT[IInterfaceNameT],
    interfaceIdI: IdI[IInterfaceNameI],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
    val interfaceTemplateIdT = TemplataCompiler.getInterfaceTemplate(interfaceIdT)

    val interfaceDefT =
      vassertOne(hinputs.interfaces.filter(_.templateName == interfaceTemplateIdT))

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
        interfaceTemplateIdT -> assemblePlaceholderMap(hinputs, interfaceIdT))
    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        interfaceTemplateIdT,
        interfaceIdT,
        DenizenBoundToDenizenCallerBoundArgI(
          assembleCalleeDenizenFunctionBounds(
            interfaceDefT.runeToFunctionBound,
            instantiationBoundArgs.runeToFunctionBoundArg),
          assembleCalleeDenizenImplBounds(
            interfaceDefT.runeToImplBound,
            instantiationBoundArgs.runeToImplBoundArg)))
    instantiator.translateInterfaceDefinition(
      substitutions, interfaceIdI, interfaceDefT)
  }

  def assembleCalleeDenizenFunctionBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[FunctionBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeI]
  ): Map[IdT[FunctionBoundNameT], PrototypeI] = {
    calleeRuneToSuppliedPrototype.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def assembleCalleeDenizenImplBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[ImplBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedImpl: Map[IRuneS, IdI[IImplNameI]]
  ): Map[IdT[ImplBoundNameT], IdI[IImplNameI]] = {
    calleeRuneToSuppliedImpl.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def translateStructDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    structIdT: IdT[IStructNameT],
    structIdI: IdI[IStructNameI],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
    if (opts.sanityCheck) {
      vassert(Collector.all(structIdI, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    val structTemplate = TemplataCompiler.getStructTemplate(structIdT)

    val structDefT = findStruct(hinputs, structIdT)

    val topLevelDenizenFullName =
      getTopLevelDenizenId(structIdT)
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArgI(
        assembleCalleeDenizenFunctionBounds(
          structDefT.runeToFunctionBound, instantiationBoundArgs.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          structDefT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg))
    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
        topLevelDenizenTemplateFullName ->
          assemblePlaceholderMap(
            hinputs,
            // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
            // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
            // This struct might not be the top level denizen, such as if it's a lambda.
            topLevelDenizenFullName))
    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        structTemplate,
        structIdT,
        denizenBoundToDenizenCallerSuppliedThing)

    instantiator.translateStructDefinition(
      substitutions, structIdT, structIdI, structDefT)
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
    interfaceId: IdI[IInterfaceNameI],
    abstractFuncT: PrototypeT,
    abstractFuncI: PrototypeI,
    virtualIndex: Int,
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  Unit = {
    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(abstractFuncT.id)

    val funcT =
      vassertOne(
        hinputs.functions.filter(func => {
          TemplataCompiler.getFunctionTemplate(func.header.id) == funcTemplateNameT
        }))

    val substitutions =
      Map(
        funcTemplateNameT ->
          assemblePlaceholderMap(hinputs, abstractFuncT.id))
    val abstractFuncInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        funcTemplateNameT,
        abstractFuncT.id,
        DenizenBoundToDenizenCallerBoundArgI(
          assembleCalleeDenizenFunctionBounds(
            funcT.runeToFuncBound, instantiationBoundArgs.runeToFunctionBoundArg),
          assembleCalleeDenizenImplBounds(
            funcT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg)))

    vassert(!monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.contains(abstractFuncI.id))
    monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.put(abstractFuncI.id, (abstractFuncInstantiator, instantiationBoundArgs))

    val abstractFuncs = vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interfaceId))
    vassert(!abstractFuncs.contains(abstractFuncI))
    abstractFuncs.put(abstractFuncI, virtualIndex)

    vassertSome(monouts.interfaceToImpls.get(interfaceId)).foreach({ case (implT, impl) =>
      translateOverride(opts, interner, keywords, hinputs, monouts, implT, impl, abstractFuncT, abstractFuncI)
    })
  }

  def translateOverride(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    implIdT: IdT[IImplNameT],
    implId: IdI[IImplNameI],
    abstractFuncPrototypeT: PrototypeT,
    abstractFuncPrototypeI: PrototypeI):
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

    val edgeInstantiator =
      vassertSome(monouts.impls.get(implId))._4

    val dispatcherPlaceholderFullNameToSuppliedTemplata =
      dispatcherFullNameT.localName.templateArgs
        .map(dispatcherPlaceholderTemplata => {// FullNameT(_, _, PlaceholderNameT(PlaceholderTemplateNameT(index))) =>
          val dispatcherPlaceholderFullName =
            TemplataCompiler.getPlaceholderTemplataId(dispatcherPlaceholderTemplata)
          val implPlaceholder =
            vassertSome(
              implPlaceholderToDispatcherPlaceholder.find(_._2 == dispatcherPlaceholderTemplata))._1
          val IdT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(index, rune))) = implPlaceholder
          val templata = implId.localName.templateArgs(index)
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

    val dispatcherSubstitutions =
      Map(dispatcherTemplateId -> dispatcherPlaceholderFullNameToSuppliedTemplata.toMap)
    val dispatcherInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        TemplataCompiler.getFunctionTemplate(dispatcherFullNameT),
        dispatcherFullNameT,
        DenizenBoundToDenizenCallerBoundArgI(
          dispatcherFunctionBoundToIncomingPrototype,
          dispatcherImplBoundToIncomingImpl))

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
          val templata = implId.localName.templateArgs(index)
          casePlaceholderFullName -> templata
          //          // templata is the value from the edge that's doing the overriding. It comes from the impl.
          //          val dispatcherCasePlaceholderFullName =
          //            dispatcherCaseFullNameT.addStep(interner.intern(PlaceholderNameT(interner.intern(PlaceholderTemplateNameT(index)))))
          //          val templataGivenToCaseFromImpl =
          //            edgeInstantiator.translateTemplata(templataGivenToCaseFromImplT)
          //          dispatcherCasePlaceholderFullName -> templataGivenToCaseFromImpl
        }
      })

    val edgeDenizenBoundToDenizenCallerSuppliedThing =
      edgeInstantiator.denizenBoundToDenizenCallerSuppliedThing


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
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
        dispatcherTemplateId -> dispatcherPlaceholderFullNameToSuppliedTemplataMap,
        dispatcherFullNameT -> dispatcherCasePlaceholderFullNameToSuppliedTemplataMap)
    val caseInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        dispatcherCaseFullNameT,
        dispatcherCaseFullNameT,
        caseDenizenBoundToDenizenCallerSuppliedThing)


    // right here we're calling it from the perspective of the abstract function
    // we need to call it from the perspective of the abstract dispatcher function's case.
    // we might need a sub-instantiator if that makes sense...

    // we need to make a instantiator that thinks in terms of impl overrides.

    val overridePrototype =
      caseInstantiator.translatePrototype(caseSubstitutions, vimpl(), overridePrototypeT)

    val superInterfaceFullName = vassertSome(monouts.impls.get(implId))._2

    val overrride =
      OverrideI(
        vimpl(dispatcherFullNameT), Vector(), Vector(), Map(), Map(), Map(), vimpl(dispatcherCaseFullNameT), overridePrototype)
    monouts.addMethodToVTable(implId, superInterfaceFullName, vimpl(abstractFuncPrototypeI), overrride)
  }

  def translateImpl(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    implIdT: IdT[IImplNameT],
    implIdI: IdI[IImplNameI],
    instantiationBoundsForUnsubstitutedImpl: InstantiationBoundArgumentsI):
  Unit = {
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
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
        implTemplateFullName -> assemblePlaceholderMap(hinputs, implIdT))
    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        implTemplateFullName,
        implIdT,
        denizenBoundToDenizenCallerSuppliedThing)
    instantiator.translateImplDefinition(substitutions, implIdT, implDefinition)


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
    //        DenizenInstantiator.translateFunction(
    //          opts, interner, keywords, hinputs, monouts, overridePrototype.fullName,
    //          translateBoundArgsForCallee(
    //            hinputs.getInstantiationBounds(overridePrototype.fullName)))
    //
    //      monouts.addMethodToVTable(implFullName, superInterfaceFullName, abstractFunc, funcT)
    //    })

  }

  def translateFunction(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    desiredPrototypeT: PrototypeT,
    desiredPrototypeI: PrototypeI,
    suppliedBoundArgs: InstantiationBoundArgumentsI,
    // This is only Some if this is a lambda. This will contain the prototypes supplied to the top level denizen by its
    // own caller, see LCNBAFA.
    maybeDenizenBoundToDenizenCallerSuppliedThing: Option[DenizenBoundToDenizenCallerBoundArgI]):
  FunctionDefinitionI = {
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
      getTopLevelDenizenId(vimpl(/*desiredPrototype.id*/))
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)
    // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
    // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
    val topLevelDenizenPlaceholderIndexToTemplata =
    topLevelDenizenFullName.localName.templateArgs

    val substitutions =
      Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]](
        topLevelDenizenTemplateFullName ->
          assemblePlaceholderMap(hinputs, topLevelDenizenFullName))
    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        funcTemplateNameT,
        desiredPrototypeT.id,
        denizenBoundToDenizenCallerSuppliedThing)

    val monomorphizedFuncT = instantiator.translateFunction(substitutions, funcT)

    vassert(desiredPrototypeI.returnType == monomorphizedFuncT.header.returnType)

    monomorphizedFuncT
  }

  // This isn't just for parameters, it's for impl subcitizens, and someday for cases too.
  // See NBIFP
  private def hoistBoundsFromParameter(
    hinputs: HinputsT,
    monouts: InstantiatedOutputs,
    paramT: KindT,
    paramI: KindIT):
  Option[DenizenBoundToDenizenCallerBoundArgI] = {
    (paramT, paramI) match {
      case (StructTT(structFullNameT), StructIT(structFullNameI)) => {
        val calleeRuneToBoundArgT = hinputs.getInstantiationBoundArgs(structFullNameT)
        val (_, structDenizenBoundToDenizenCallerSuppliedThing) =
          vassertSome(monouts.startedStructs.get(structFullNameI))
        val structT = findStruct(hinputs, structFullNameT)
        val denizenBoundToDenizenCallerSuppliedThing =
          hoistBoundsFromParameterInner(
            structDenizenBoundToDenizenCallerSuppliedThing, calleeRuneToBoundArgT, structT.runeToFunctionBound, structT.runeToImplBound)
        Some(denizenBoundToDenizenCallerSuppliedThing)
      }
      case (InterfaceTT(interfaceFullNameT), InterfaceIT(interfaceFullNameM)) => {
        val calleeRuneToBoundArgT = hinputs.getInstantiationBoundArgs(interfaceFullNameT)
        val (_, interfaceDenizenBoundToDenizenCallerSuppliedThing) = vassertSome(monouts.startedInterfaces.get(interfaceFullNameM))
        val interfaceT = findInterface(hinputs, interfaceFullNameT)
        val denizenBoundToDenizenCallerSuppliedThing =
          hoistBoundsFromParameterInner(
            interfaceDenizenBoundToDenizenCallerSuppliedThing, calleeRuneToBoundArgT, interfaceT.runeToFunctionBound, interfaceT.runeToImplBound)
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

  def translateTemplata(templata: ITemplataT[ITemplataType]): ITemplataI = {
    vimpl()
  }

  def assemblePlaceholderMap(hinputs: HinputsT, id: IdT[IInstantiationNameT]):
  Map[IdT[IPlaceholderNameT], ITemplataI] = {
    val containersPlaceholderMap =
      // This might be a lambda's name. If it is, its name has an init step that's the parent
      // function's name, and we want its mappings too.
      (id.initNonPackageFullName() match {
        case Some(IdT(packageCoord, initSteps, parentLocalName : IInstantiationNameT)) => {
          assemblePlaceholderMap(hinputs, IdT(packageCoord, initSteps, parentLocalName))
        }
        case _ => Map[IdT[IPlaceholderNameT], ITemplataI]()
      })

    val placeholderedName =
      id match {
        case IdT(_, _, localName : IStructNameT) => {
          hinputs.lookupStructByTemplate(localName.template).instantiatedCitizen.id
        }
        case IdT(_, _, localName : IInterfaceNameT) => {
          hinputs.lookupInterfaceByTemplate(localName.template).instantiatedInterface.id
        }
        case IdT(_, _, localName : IFunctionNameT) => {
          vassertSome(hinputs.lookupFunction(localName.template)).header.id
        }
        case IdT(_, _, localName : IImplNameT) => {
          hinputs.lookupImplByTemplate(localName.template).edgeId
        }
        case IdT(_, _, localName : ExportNameT) => {
          vassertOne(
            hinputs.kindExports.filter(_.id.localName.template == localName.template).map(_.id) ++
              hinputs.functionExports.filter(_.exportId.localName.template == localName.template).map(_.exportId))
        }
      }

    containersPlaceholderMap ++
    placeholderedName.localName.templateArgs
      .zip(id.localName.templateArgs)
      .flatMap({
        case (CoordTemplataT(CoordT(placeholderOwnership, PlaceholderTemplataT(regionPlaceholderId, RegionTemplataType()), KindPlaceholderT(kindPlaceholderId))), c @ CoordTemplataT(_)) => {
          vassert(placeholderOwnership == OwnT || placeholderOwnership == ShareT)
          // We might need to do something with placeholderRegion here, but I think we can just
          // assume it correctly matches up with the coord's region. The typing phase should have
          // made sure it matches up nicely.
          List(
            (regionPlaceholderId -> translateTemplata(c.coord.region)),
            (kindPlaceholderId -> translateTemplata(c)))
        }
        case (KindTemplataT(KindPlaceholderT(placeholderId)), kindTemplataT) => {
          List((placeholderId -> translateTemplata(kindTemplataT)))
        }
        case (PlaceholderTemplataT(placeholderId, tyype), templataT) => {
          List((placeholderId -> translateTemplata(templataT)))
        }
        case (a, b) => {
          // We once got a `mut` for the placeholdered name's templata.
          // That's because we do some specialization for arrays still.
          // They don't come with a placeholder, so ignore them.
          vassert(a == b)
          List()
        }
      })
      .toMap
  }
}

class Instantiator(
  opts: GlobalOptions,
  interner: Interner,
  keywords: Keywords,
  hinputs: HinputsT,
  monouts: InstantiatedOutputs,
  denizenTemplateName: IdT[ITemplateNameT],
  denizenName: IdT[IInstantiationNameT],
  val denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArgI) {
  //  selfFunctionBoundToRuneUnsubstituted: Map[PrototypeT, IRuneS],
  //  denizenRuneToDenizenCallerPrototype: Map[IRuneS, PrototypeT]) {

  // This is just here to get scala to include these fields so i can see them in the debugger
  vassert(TemplataCompiler.getTemplate(denizenName) == denizenTemplateName)

  //  if (opts.sanityCheck) {
  //    denizenFunctionBoundToDenizenCallerSuppliedPrototype.foreach({
  //      case (denizenFunctionBound, denizenCallerSuppliedPrototype) => {
  //        vassert(Collector.all(denizenCallerSuppliedPrototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
  //      }
  //    })
  //  }

  def translateStructMember(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    member: IStructMemberT):
  StructMemberI = {
    member match {
      case NormalStructMemberT(name, variability, tyype) => {
        StructMemberI(
          translateVarName(name),
          translateVariability(variability),
          tyype match {
            case ReferenceMemberTypeT((unsubstitutedCoord)) => {
              ReferenceMemberTypeI(translateCoord(substitutions, perspectiveRegionT, unsubstitutedCoord))
            }
            case AddressMemberTypeT((unsubstitutedCoord)) => {
              AddressMemberTypeI(translateCoord(substitutions, perspectiveRegionT, unsubstitutedCoord))
            }
          })
      }
      case VariadicStructMemberT(name, tyype) => {
        vimpl()
      }
    }
  }

  def translateVariability(t: VariabilityT): VariabilityI = {
    vimpl()
  }

  // This is run at the call site, from the caller's perspective
  def translatePrototype(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    desiredPrototypeT: PrototypeT):
  PrototypeI = {
    val PrototypeT(desiredPrototypeFullNameUnsubstituted, desiredPrototypeReturnTypeUnsubstituted) = desiredPrototypeT

    val runeToBoundArgsForCall =
      translateBoundArgsForCallee(
        substitutions,
        perspectiveRegionT,
        hinputs.getInstantiationBoundArgs(desiredPrototypeT.id))

    val desiredPrototype =
      PrototypeI(
        translateFunctionFullName(substitutions, perspectiveRegionT, desiredPrototypeFullNameUnsubstituted),
        translateCoord(substitutions, perspectiveRegionT, desiredPrototypeReturnTypeUnsubstituted))

    desiredPrototypeT.id match {
      case IdT(packageCoord, initSteps, name @ FunctionBoundNameT(_, _, _)) => {
        val funcBoundName = IdT(packageCoord, initSteps, name)
        val result = vassertSome(denizenBoundToDenizenCallerSuppliedThing.funcBoundToCallerSuppliedBoundArgFunc.get(funcBoundName))
        //        if (opts.sanityCheck) {
        //          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        //        }
        result
      }
      case IdT(_, _, ExternFunctionNameT(_, _)) => {
        if (opts.sanityCheck) {
          vassert(Collector.all(desiredPrototype, { case KindPlaceholderTemplateNameT(_, _) => }).isEmpty)
        }
        desiredPrototype
      }
      case IdT(_, _, last) => {
        last match {
          case LambdaCallFunctionNameT(_, _, _) => {
            vassert(
              desiredPrototype.id.steps.slice(0, desiredPrototype.id.steps.length - 2) ==
                denizenName.steps)
            vcurious(desiredPrototype.id.steps.startsWith(denizenName.steps))
          }
          case _ =>
        }

        Collector.all(desiredPrototype, {
          case RegionTemplataI(_) => vwat()
        })
        Collector.all(runeToBoundArgsForCall.runeToFunctionBoundArg.toVector, {
          case RegionTemplataI(_) => vwat()
        })
        Collector.all(runeToBoundArgsForCall.runeToImplBoundArg.toVector, {
          case RegionTemplataI(_) => vwat()
        })

        monouts.newFunctions.enqueue(
          (
            desiredPrototypeT,
            desiredPrototype,
            runeToBoundArgsForCall,
            // We need to supply our bounds to our lambdas, see LCCPGB and LCNBAFA.
            if (desiredPrototype.id.steps.startsWith(denizenName.steps)) {
              Some(denizenBoundToDenizenCallerSuppliedThing)
            } else {
              None
            }))
        desiredPrototype
      }
    }
  }

  private def translateBoundArgsForCallee(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
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
              translatePrototype(substitutions, vimpl(), suppliedPrototypeUnsubstituted)
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
                  substitutions,
                  perspectiveRegionT,
                  hinputs.getInstantiationBoundArgs(suppliedImplUnsubstituted))
              translateImplFullName(substitutions, perspectiveRegionT, suppliedImplUnsubstituted, runeToBoundArgsForCall)
            }
          })
      })
    // And now we have a map from the callee's rune to the *instantiated* callee's impls.

    InstantiationBoundArgumentsI(runeToSuppliedPrototypeForCall, runeToSuppliedImplForCall)
  }

  def translateStructDefinition(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    newIdT: IdT[IStructNameT],
    newId: IdI[IStructNameI],
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

    val mutability = ITemplataI.expectMutabilityTemplata(translateTemplata(substitutions, perspectiveRegionT, mutabilityT)).mutability

    if (monouts.startedStructs.contains(newId)) {
      return
    }
    monouts.startedStructs.put(
      newId, (mutability, this.denizenBoundToDenizenCallerSuppliedThing))

//    val currentPureHeight = vimpl()

    val result =
      StructDefinitionI(
//        templateName,
        StructIT(newId),
        attributes.map(vimpl(_)),
        weakable,
        MutabilityTemplataI(mutability),
        members.map(translateStructMember(substitutions, perspectiveRegionT, _)),
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

  def translateInterfaceDefinition(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    newId: IdI[IInterfaceNameI],
    interfaceDefT: InterfaceDefinitionT):
  Unit = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutabilityT, _, _, internalMethods) = interfaceDefT

    val mutability =
      ITemplataI.expectMutabilityTemplata(
        translateTemplata(substitutions, vimpl(), mutabilityT)).mutability

    if (monouts.startedInterfaces.contains(newId)) {
      return
    }
    monouts.startedInterfaces.put(newId, (mutability, this.denizenBoundToDenizenCallerSuppliedThing))

    val newInterfaceIT = InterfaceIT(newId)

    val result =
      InterfaceDefinitionI(
        newInterfaceIT,
        attributes.map(vimpl(_)),
        weakable,
        MutabilityTemplataI(mutability),
        Map(),
        Map(),
        Vector())

    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    vassert(!monouts.interfaceToImplToAbstractPrototypeToOverride.contains(newId))
    monouts.interfaceToImplToAbstractPrototypeToOverride.put(newId, mutable.HashMap())

    monouts.interfacesWithoutMethods.put(newId, result)

    vassert(!monouts.interfaceToAbstractFuncToVirtualIndex.contains(newId))
    monouts.interfaceToAbstractFuncToVirtualIndex.put(newId, mutable.HashMap())

    vassert(!monouts.interfaceToImpls.contains(newId))
    monouts.interfaceToImpls.put(newId, mutable.HashSet())

    vassert(result.instantiatedCitizen.id == newId)
  }

  def translateFunctionHeader(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    header: FunctionHeaderT):
  FunctionHeaderI = {
    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = header

    val newFullName = translateFunctionFullName(substitutions, perspectiveRegionT, fullName)

    val result =
      FunctionHeaderI(
        newFullName,
        attributes.map(vimpl(_)),
        params.map(translateParameter(substitutions, perspectiveRegionT, _)),
        translateCoord(substitutions, perspectiveRegionT, returnType))

    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(result.fullName, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.attributes, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.params, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.returnType, { case PlaceholderNameT(_) => }).isEmpty)
    //    }

    result
  }

  def translateFunction(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    functionT: FunctionDefinitionT):
  FunctionDefinitionI = {
    val FunctionDefinitionT(headerT, _, _, bodyT) = functionT

    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = headerT

    if (opts.sanityCheck) {
      Collector.all(substitutions.toVector, {
        case RegionTemplataI(_) => vwat()
      })
    }

    val perspectiveRegionT =
      functionT.header.id.localName.templateArgs.last match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    val newFullName = translateFunctionFullName(substitutions, perspectiveRegionT, fullName)

    monouts.functions.get(newFullName) match {
      case Some(func) => return func
      case None =>
    }

    val newHeader = translateFunctionHeader(substitutions, perspectiveRegionT, headerT)

    val startingEnv = NodeEnvironment(None, mutable.HashMap())
    val bodyResult = translateRefExpr(startingEnv, substitutions, perspectiveRegionT, bodyT)
    val result = FunctionDefinitionI(newHeader, Map(), Map(), bodyResult)
    monouts.functions.put(result.header.id, result)
    result
  }

  def translateLocalVariable(
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    variable: ILocalVariableT,
    // We use this pre-translated type instead of the one in the variable because of CTOTFIPB.
    tyype: CoordT):
  ILocalVariableI = {
    variable match {
      case r @ ReferenceLocalVariableT(_, _, _) => {
        translateReferenceLocalVariable(perspectiveRegionT, r, tyype)
      }
      case AddressibleLocalVariableT(id, variability, coordUnusedT) => {
        AddressibleLocalVariableI(
          translateVarName(id), translateVariability(variability), vimpl(tyype))
      }
    }
  }

  def translateReferenceLocalVariable(
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    variable: ReferenceLocalVariableT,
    // We use this pre-translated type instead of the one in the variable because of CTOTFIPB.
    tyype: CoordT):
  ReferenceLocalVariableI = {
    val ReferenceLocalVariableT(id, variability, reference) = variable
    ReferenceLocalVariableI(translateVarName(id), translateVariability(variability), vimpl(tyype))
  }

  def translateAddrExpr(
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: AddressExpressionTE):
  AddressExpressionIE = {
    expr match {
      case LocalLookupTE(range, localVariableT) => {
        // We specifically don't *translate* LocalLookupTE.localVariable because we can't translate
        // it properly from here with our current understandings of the regions' mutabilities, we
        // need its original type. See CTOTFIPB.
        val localVariable = env.lookupOriginalTranslatedVariable(localVariableT.name)

        val sourceRegion =
          ITemplataI.expectRegionTemplata(
            translateTemplata(substitutions, perspectiveRegionT, vimpl()))

        LocalLookupIE(range, vimpl(localVariable), vimpl())
      }
      case ReferenceMemberLookupTE(range, structExprT, memberNameT, memberCoordT, variability) => {
        val structExpr = translateRefExpr(env, substitutions, perspectiveRegionT, structExprT)
        val resultStruct = structExpr.result.coord.kind.expectStruct()
        val memberName = translateVarName(memberNameT)
        // We can't translate ReferenceMemberLookupTE.memberCoord's kind here because we'll
        // translate its template args' regions incorrectly according to their current mutabilities.
        // They need to be the mutabilities at the time they were introduced, see CTOTFIPB. So
        // instead, we look it up from the struct definition.
        val structDef = vassertSome(monouts.structs.get(resultStruct.id))
        val defMemberCoord: CoordT = vimpl()
//          vassertSome(structDef.members.find(_.name == memberName)) match {
//            case NormalStructMemberT(name, variability, tyype) => tyype.reference
//            case VariadicStructMemberT(name, tyype) => vimpl()
//          }

        // However, the resulting coord's region *should* have the current mutability.
        val resultRegion =
          ITemplataI.expectRegionTemplata(
            translateTemplata(substitutions, perspectiveRegionT, memberCoordT.region))

        val resultOwnership =
          (defMemberCoord.ownership, resultRegion) match {
            case (OwnT, RegionTemplataI(_)) => OwnI
//            case (MutableShareI, RegionTemplataI(true)) => MutableShareI
//            case (MutableShareI, RegionTemplataI(false)) => ImmutableShareI
            case (otherOwnership, otherRegion) => vimpl()
          }

        val resultCoord = CoordI(resultOwnership, vimpl(defMemberCoord.kind))

        ReferenceMemberLookupIE(
          range,
          structExpr,
          memberName,
          resultCoord,
          translateVariability(variability))
      }
      case StaticSizedArrayLookupTE(range, arrayExprT, indexExprT, elementTypeT, variability) => {
        // DO NOT SUBMIT combine a lot of this with the ReferenceMemberLookupTE case
        val arrayExpr = translateRefExpr(env, substitutions, perspectiveRegionT, arrayExprT)
        // We can't translate StaticSizedArrayLookupTE.elementTypeT's kind here because we'll
        // translate its template args' regions incorrectly according to their current mutabilities.
        // They need to be the mutabilities at the time they were introduced, see CTOTFIPB. So
        // instead, we look it up from the struct definition.
        val elementType =
          arrayExpr.result.coord.kind match {
            case StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, _, _, RawArrayNameI(_, elementType, _)))) => {
              elementType
            }
          }

        // However, the resulting coord's region *should* have the current mutability.
        val resultRegion =
          ITemplataI.expectRegionTemplata(
            translateTemplata(substitutions, perspectiveRegionT, elementTypeT.region))

        val resultCoord = CoordI(elementType.ownership, elementType.kind)

        StaticSizedArrayLookupIE(
          range, arrayExpr, vimpl(indexExprT), resultCoord, translateVariability(variability))
      }
      case AddressMemberLookupTE(range, structExpr, memberName, resultType2, variability) => {
        AddressMemberLookupIE(
          range,
          translateRefExpr(env, substitutions, perspectiveRegionT, structExpr),
          translateVarName(memberName),
          translateCoord(substitutions, perspectiveRegionT, resultType2),
          translateVariability(variability))
      }
      case RuntimeSizedArrayLookupTE(range, arrayExpr, arrayType, indexExpr, variability) => {
        RuntimeSizedArrayLookupIE(
          range,
          translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr),
          translateRuntimeSizedArray(substitutions, perspectiveRegionT, arrayType),
          translateRefExpr(env, substitutions, perspectiveRegionT, indexExpr),
          translateVariability(variability))
      }
      case other => vimpl(other)
    }
  }

  def translateExpr(
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: ExpressionT):
  ExpressionI = {
    expr match {
      case r : ReferenceExpressionTE => translateRefExpr(env, substitutions, perspectiveRegionT, r)
      case a : AddressExpressionTE => translateAddrExpr(env, substitutions, perspectiveRegionT, a)
    }
  }

  def translateRefExpr(
    env: NodeEnvironment,
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    expr: ReferenceExpressionTE):
  ReferenceExpressionIE = {
    val resultRefExpr =
      expr match {
        case LetNormalTE(variableT, innerTE) => {
          val inner = translateRefExpr(env, substitutions, perspectiveRegionT, innerTE)
          val translatedVariable = translateLocalVariable(perspectiveRegionT, variableT, vimpl(inner.result.coord))
          env.addTranslatedVariable(variableT.name, vimpl(translatedVariable))
          LetNormalIE(
            translatedVariable,
            inner)
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
          val newDefaultRegion = RegionTemplataI(vimpl())
          val oldSubstitutionsForThisDenizenTemplate = substitutions.getOrElse(denizenTemplateName, Map())
          val newSubstitutionsForThisDenizenTemplate =
            oldSubstitutionsForThisDenizenTemplate + (newDefaultRegionNameT -> newDefaultRegion)
          val newSubstitutions =
            substitutions + (denizenTemplateName -> newSubstitutionsForThisDenizenTemplate)

          PureTE(
            location,
            newDefaultRegion,
            oldRegionToNewRegion.map({ case (oldRegionT, newRegionT) =>
              vassert(newRegionT == newDefaultRegionT)
              val newRegion = newDefaultRegion
              val oldRegion =
                expectRegionTemplata(
                  translateTemplata(
                    newSubstitutions, perspectiveRegionT, oldRegionT))
              (oldRegion, newRegion)
            }),
            translateRefExpr(
              env, newSubstitutions, newPerspectiveRegionT, inner),
            translateCoord(substitutions, oldPerspectiveRegionT, resultCoordT))
        }
        case BlockTE(inner) => {
          BlockIE(translateRefExpr(env, substitutions, perspectiveRegionT, inner))
        }
        case ReturnTE(inner) => {
          ReturnIE(translateRefExpr(env, substitutions, perspectiveRegionT, inner))
        }
        case ConsecutorTE(inners) => {
          ConsecutorIE(inners.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)))
        }
        case ConstantIntTE(value, bits, region) => {
          ConstantIntIE(
            ITemplataT.expectIntegerTemplata(translateTemplata(substitutions, perspectiveRegionT, value)),
            bits,
            ITemplataT.expectRegion(translateTemplata(substitutions, perspectiveRegionT, region)))
        }
        case ConstantStrTE(value, region) => {
          ConstantStrIE(value, ITemplataT.expectRegion(translateTemplata(substitutions, perspectiveRegionT, region)))
        }
        case ConstantBoolTE(value, region) => {
          ConstantBoolIE(value, ITemplataT.expectRegion(translateTemplata(substitutions, perspectiveRegionT, region)))
        }
        case ConstantFloatTE(value, region) => {
          ConstantFloatIE(value, ITemplataT.expectRegion(translateTemplata(substitutions, perspectiveRegionT, region)))
        }
        case UnletTE(variable) => {
          val local = env.lookupOriginalTranslatedVariable(variable.name)
          UnletIE(local)
        }
        case DiscardTE(expr) => {
          DiscardIE(translateRefExpr(env, substitutions, perspectiveRegionT, expr))
        }
        case VoidLiteralTE(region) => {
          VoidLiteralIE(ITemplataT.expectRegion(translateTemplata(substitutions, perspectiveRegionT, region)))
        }
        case FunctionCallTE(prototypeT, args) => {
          val prototype = translatePrototype(substitutions, perspectiveRegionT, prototypeT)
          FunctionCallIE(
            prototype,
            args.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)))
        }
        case InterfaceFunctionCallIE(superFunctionPrototypeT, virtualParamIndex, resultReference, args) => {
          val superFunctionPrototype = translatePrototype(substitutions, perspectiveRegionT, superFunctionPrototypeT)
          val result =
            InterfaceFunctionCallIE(
              superFunctionPrototype,
              virtualParamIndex,
              translateCoord(substitutions, perspectiveRegionT, resultReference),
              args.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)))
          val interfaceFullName =
            superFunctionPrototype.paramTypes(virtualParamIndex).kind.expectInterface().id
          //        val interfaceFullName =
          //          translateInterfaceFullName(
          //            interfaceFullNameT,
          //            translateBoundArgsForCallee(
          //              hinputs.getInstantiationBounds(callee.toPrototype.fullName)))

          val instantiationBoundArgs =
            translateBoundArgsForCallee(
              substitutions,
              perspectiveRegionT,

              // but this is literally calling itself from where its defined
              // perhaps we want the thing that originally called
              hinputs.getInstantiationBoundArgs(superFunctionPrototypeT.id))

          monouts.newAbstractFuncs.enqueue(
            (superFunctionPrototype, virtualParamIndex, interfaceFullName, instantiationBoundArgs))

          result
        }
        case ArgLookupTE(paramIndex, reference) => ArgLookupIE(paramIndex, translateCoord(substitutions, perspectiveRegionT, reference))
        case SoftLoadTE(originalInner, originalTargetOwnership) => {
          val inner = translateAddrExpr(env, substitutions, perspectiveRegionT, originalInner)
          val targetOwnership =
            // First, figure out what ownership it is after substitution.
            // if we have an owned T but T is a &Ship, then own + borrow = borrow
            (originalTargetOwnership, inner.result.coord.ownership) match {
              case (a, b) if a == b => a
              case (ShareT, ImmutableShareI) => ImmutableShareI
              case (ShareT, MutableShareI) => MutableShareI
              case (BorrowT, ImmutableShareI) => ImmutableShareI
              case (BorrowT, MutableShareI) => MutableShareI
              case (BorrowT, ImmutableBorrowI) => ImmutableBorrowI
              case (BorrowT, MutableBorrowI) => {
//                MutableBorrowI
                if (coordRegionIsMutable(substitutions, perspectiveRegionT, originalInner.result.coord)) {
                  MutableBorrowI
                } else {
                  ImmutableBorrowI
                }
              }
              case (BorrowT, WeakT) => WeakT
              case (BorrowT, OwnT) => {
                if (coordRegionIsMutable(substitutions, perspectiveRegionT, originalInner.result.coord)) {
                  MutableBorrowI
                } else {
                  ImmutableBorrowI
                }
              }
              case (WeakT, ImmutableShareI) => ImmutableShareI
              case (WeakT, MutableShareI) => MutableShareI
              case (WeakT, OwnT) => WeakT
              case (WeakT, ImmutableBorrowI) => vimpl(WeakT)
              case (WeakT, MutableBorrowI) => vimpl(WeakT)
              case other => vwat(other)
            }
          SoftLoadTE(inner, targetOwnership)
        }
        case ExternFunctionCallTE(prototype2, args) => {
          ExternFunctionCallIE(
            translatePrototype(substitutions, perspectiveRegionT, prototype2),
            args.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)))
        }
        case ConstructTE(structTT, resultReference, args) => {
          val coord = translateCoord(substitutions, perspectiveRegionT, resultReference)

          //          val freePrototype = translatePrototype(freePrototypeT)
          //          // They might disagree on the ownership, and thats fine.
          //          // That free prototype is only going to take an owning or a share reference, and we'll only
          //          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          ConstructIE(
            translateStruct(
              substitutions,
              perspectiveRegionT,

              structTT,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(structTT.id))),
            coord,
            args.map(translateExpr(env, substitutions, perspectiveRegionT, _)))
        }
        case DestroyTE(exprT, structTT, destinationReferenceVariables) => {
          val expr = translateRefExpr(env, substitutions, perspectiveRegionT, exprT)
          val resultT =
            expr.result.coord.kind match {
              case s @ StructIT(_) => s
              case other => vwat(other)
            }
          val structDef = vassertSome(monouts.structs.get(resultT.id))

          vassert(structDef.members.size == destinationReferenceVariables.size)
          DestroyIE(
            expr,
            resultT,
            structDef.members.zip(destinationReferenceVariables).map({
              case (NormalStructMemberT(_, _, ReferenceMemberTypeT(memberCoord)), destRefVarT) => {
                val destRefVar =
                  translateReferenceLocalVariable(
                    perspectiveRegionT, destRefVarT, memberCoord)
                env.addTranslatedVariable(destRefVar.name, destRefVar)
                destRefVar
              }
              case other => vimpl(other)
            }))
        }
        case DestroyStaticSizedArrayIntoLocalsTE(exprT, ssaTT, destinationReferenceVariables) => {
          val expr = translateRefExpr(env, substitutions, perspectiveRegionT, exprT)
          val (ssaTT, size) =
            expr.result.coord.kind match {
              case s @ StaticSizedArrayIT(IdI(_, _, StaticSizedArrayNameI(_, IntegerTemplataI(size), _, _))) => (s, size)
              case other => vwat(other)
            }

          vassert(size == destinationReferenceVariables.size)
          DestroyStaticSizedArrayIntoLocalsIE(
            expr,
            ssaTT,
            destinationReferenceVariables.map(destRefVarT => {
              val destRefVar =
                translateReferenceLocalVariable(
                  perspectiveRegionT, destRefVarT, ssaTT.elementType)
              env.addTranslatedVariable(destRefVar.name, destRefVar)
              destRefVar
            }))
        }
        case MutateTE(destinationExpr, sourceExpr) => {
          MutateIE(
            translateAddrExpr(env, substitutions, perspectiveRegionT, destinationExpr),
            translateRefExpr(env, substitutions, perspectiveRegionT, sourceExpr))
        }
        case u @ UpcastTE(innerExprUnsubstituted, targetSuperKind, untranslatedImplFullName) => {
          val implFullName =
            translateImplFullName(
              substitutions,
              perspectiveRegionT,

              untranslatedImplFullName,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(untranslatedImplFullName)))
          //          val freePrototype = translatePrototype(freePrototypeT)
          val coord = translateCoord(substitutions, perspectiveRegionT, u.result.coord)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          UpcastIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, innerExprUnsubstituted),
            translateSuperKind(substitutions, perspectiveRegionT, targetSuperKind),
            implFullName)//,
          //            freePrototype)
        }
        case IfTE(condition, thenCall, elseCall) => {
          IfIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, condition),
            translateRefExpr(env, substitutions, perspectiveRegionT, thenCall),
            translateRefExpr(env, substitutions, perspectiveRegionT, elseCall))
        }
        case IsSameInstanceTE(left, right) => {
          IsSameInstanceIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, left),
            translateRefExpr(env, substitutions, perspectiveRegionT, right))
        }
        case StaticArrayFromValuesTE(elements, resultReference, arrayType) => {

          //          val freePrototype = translatePrototype(freePrototypeT)
          val coord = translateCoord(substitutions, perspectiveRegionT, resultReference)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          StaticArrayFromValuesIE(
            elements.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)),
            coord,
            translateStaticSizedArray(
              substitutions,
              perspectiveRegionT,

              arrayType))
        }
        case DeferTE(innerExpr, deferredExpr) => {
          DeferIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, innerExpr),
            translateRefExpr(env, substitutions, perspectiveRegionT, deferredExpr))
        }
        case LetAndLendTE(variable, sourceExprT, targetOwnership) => {
          val sourceExpr = translateRefExpr(env, substitutions, perspectiveRegionT, sourceExprT)

          val resultOwnership = vimpl()
//            ((targetOwnership, sourceExpr.result.coord.ownership) match {
//              case (OwnT, OwnT) => OwnT
//              case (OwnT, BorrowT) => BorrowT
//              case (BorrowT, OwnT) => BorrowT
//              case (BorrowT, BorrowT) => BorrowT
//              case (BorrowT, WeakT) => WeakT
//              case (BorrowT, ShareT) => ShareT
//              case (WeakT, OwnT) => WeakT
//              case (WeakT, BorrowT) => WeakT
//              case (WeakT, WeakT) => WeakT
//              case (WeakT, ShareT) => ShareT
//              case (ShareT, ShareT) => ShareT
//              case (OwnT, ShareT) => ShareT
//              case other => vwat(other)
//            }) match { // Now  if it's a borrow, figure out whether it's mutable or immutable
//              case BorrowT => {
//                if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(sourceExpr.result.coord.region))) {
//                  MutableBorrowI
//                } else {
//                  ImmutableBorrowI
//                }
//              }
//              case ShareT => {
//                if (regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(sourceExpr.result.coord.region))) {
//                  MutableShareI
//                } else {
//                  ImmutableShareI
//                }
//              }
//              case other => other
//            }

          LetAndLendIE(
            translateLocalVariable(perspectiveRegionT, variable, vimpl()),
            sourceExpr,
            resultOwnership)
        }
        case BorrowToWeakTE(innerExpr) => {
          BorrowToWeakIE(translateRefExpr(env, substitutions, perspectiveRegionT, innerExpr))
        }
        case WhileTE(BlockTE(inner)) => {
          WhileIE(BlockIE(translateRefExpr(env, substitutions, perspectiveRegionT, inner)))
        }
        case BreakTE(region) => BreakIE()
        case LockWeakTE(innerExpr, resultOptBorrowType, someConstructor, noneConstructor, someImplUntranslatedFullName, noneImplUntranslatedFullName) => {
          LockWeakIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, innerExpr),
            translateCoord(substitutions, perspectiveRegionT, resultOptBorrowType),
            translatePrototype(substitutions, perspectiveRegionT, someConstructor),
            translatePrototype(substitutions, perspectiveRegionT, noneConstructor),
            translateImplFullName(
              substitutions,
              perspectiveRegionT,

              someImplUntranslatedFullName,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(someImplUntranslatedFullName))),
            translateImplFullName(
              substitutions,
              perspectiveRegionT,

              noneImplUntranslatedFullName,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(noneImplUntranslatedFullName))))
        }
        case DestroyStaticSizedArrayIntoFunctionTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          DestroyStaticSizedArrayIntoFunctionIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr),
            translateStaticSizedArray(substitutions, perspectiveRegionT, arrayType),
            translateRefExpr(env, substitutions, perspectiveRegionT, consumer),
            translatePrototype(substitutions, perspectiveRegionT, consumerMethod))
        }
        case NewImmRuntimeSizedArrayTE(arrayType, region, sizeExpr, generator, generatorMethod) => {
          //          val freePrototype = translatePrototype(freePrototypeT)

          val result =
            NewImmRuntimeSizedArrayIE(
              translateRuntimeSizedArray(substitutions, perspectiveRegionT, arrayType),
              translateRefExpr(env, substitutions, perspectiveRegionT, sizeExpr),
              translateRefExpr(env, substitutions, perspectiveRegionT, generator),
              translatePrototype(substitutions, perspectiveRegionT, generatorMethod))

          val coord = result.result.coord
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          result
        }
        case StaticArrayFromCallableTE(arrayType, region, generator, generatorMethod) => {
          //          val freePrototype = translatePrototype(freePrototypeT)

          val result =
            StaticArrayFromCallableIE(
              translateStaticSizedArray(substitutions, perspectiveRegionT, arrayType),
              translateRefExpr(env, substitutions, perspectiveRegionT, generator),
              translatePrototype(substitutions, perspectiveRegionT, generatorMethod))

          val coord = result.result.coord
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          result
        }
        case RuntimeSizedArrayCapacityTE(arrayExpr) => {
          RuntimeSizedArrayCapacityIE(translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr))
        }
        case PushRuntimeSizedArrayTE(arrayExpr, newElementExpr) => {
          PushRuntimeSizedArrayIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr),
            translateRefExpr(env, substitutions, perspectiveRegionT, newElementExpr))
        }
        case PopRuntimeSizedArrayTE(arrayExpr) => {
          PopRuntimeSizedArrayIE(translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr))
        }
        case ArrayLengthTE(arrayExpr) => {
          ArrayLengthIE(translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr))
        }
        case DestroyImmRuntimeSizedArrayTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          DestroyImmRuntimeSizedArrayIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr),
            translateRuntimeSizedArray(substitutions, perspectiveRegionT, arrayType),
            translateRefExpr(env, substitutions, perspectiveRegionT, consumer),
            translatePrototype(substitutions, perspectiveRegionT, consumerMethod))
          //            translatePrototype(freePrototype))
        }
        case DestroyMutRuntimeSizedArrayTE(arrayExpr) => {
          DestroyMutRuntimeSizedArrayIE(translateRefExpr(env, substitutions, perspectiveRegionT, arrayExpr))
        }
        case NewMutRuntimeSizedArrayTE(arrayType, region, capacityExpr) => {
          NewMutRuntimeSizedArrayIE(
            translateRuntimeSizedArray(substitutions, perspectiveRegionT, arrayType),
            translateRefExpr(env, substitutions, perspectiveRegionT, capacityExpr))
        }
        case TupleTE(elements, resultReference) => {
          TupleIE(
            elements.map(translateRefExpr(env, substitutions, perspectiveRegionT, _)),
            translateCoord(substitutions, perspectiveRegionT, resultReference))
        }
        case AsSubtypeTE(sourceExpr, targetSubtype, resultResultType, okConstructor, errConstructor, implFullNameT, okResultImplFullNameT, errResultImplFullNameT) => {
          AsSubtypeIE(
            translateRefExpr(env, substitutions, perspectiveRegionT, sourceExpr),
            translateCoord(substitutions, perspectiveRegionT, targetSubtype),
            translateCoord(substitutions, perspectiveRegionT, resultResultType),
            translatePrototype(substitutions, perspectiveRegionT, okConstructor),
            translatePrototype(substitutions, perspectiveRegionT, errConstructor),
            translateImplFullName(
              substitutions,
              perspectiveRegionT,

              implFullNameT,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(implFullNameT))),
            translateImplFullName(
              substitutions,
              perspectiveRegionT,

              okResultImplFullNameT,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(okResultImplFullNameT))),
            translateImplFullName(
            substitutions,
              perspectiveRegionT,

              errResultImplFullNameT,
              translateBoundArgsForCallee(
                substitutions,
                perspectiveRegionT,

                hinputs.getInstantiationBoundArgs(errResultImplFullNameT))))
        }
        case other => vimpl(other)
      }
    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(resultRefExpr, { case PlaceholderNameT(_) => }).isEmpty)
    //    }
    resultRefExpr
  }

  private def coordRegionIsMutable(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    coord: CoordT):
  Boolean = {
    regionIsMutable(substitutions, perspectiveRegionT, expectRegionPlaceholder(coord.region))
  }


  private def regionIsMutable(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    region: IdT[RegionPlaceholderNameT]):
  Boolean = {
    val RegionPlaceholderNameT(_, _, regionPureHeight) = region.localName

    vassert(perspectiveRegionT.localName.pureHeight >= regionPureHeight)
    perspectiveRegionT.localName.pureHeight == regionPureHeight

//    // There are two possible sources of immutability:
//    // - This placeholder is being substituted with an immutable region from the caller
//    // - There is a pure block between the introduction of this region and here
//
//    vimpl()
//    val initiallyMutable =
//      expectRegionTemplata(
//        vassertSome(vassertSome(substitutions.get(region.initFullName(interner))).get(region)))
//        .mutable
//    if (!initiallyMutable) {
//      return false
//    }

//    perspectiveRegionT.localName.originalMaybeNearestPureLocation match {
//      case Some(nearestPureBlockLocation) => {
//        if (regionIntroducedLocation.before(nearestPureBlockLocation)) {
//          false
//        } else {
//          originallyMutable
//        }
//      }
//      case None => {
//        originallyMutable
//      }
//    }
  }

  def translateFunctionFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IFunctionNameT]):
  IdI[IFunctionNameI] = {
    val IdT(module, steps, last) = fullNameT
    val fullName =
      IdI(
        module,
        steps.map(translateName(substitutions, perspectiveRegionT, _)),
        translateFunctionName(substitutions, perspectiveRegionT, last))
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
//            initSteps.map(translateName(substitutions, perspectiveRegionT, _)),
//            translateRegionName(substitutions, perspectiveRegionT, r))
//      }
//    }
//  }

  def translateStructFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IStructNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  IdT[IStructNameT] = {
    val IdT(module, steps, lastT) = fullNameT

    val fullName =
      IdT(
        module,
        steps.map(translateName(substitutions, perspectiveRegionT, _)),
        translateStructName(substitutions, perspectiveRegionT, lastT))


    Instantiator.translateStructDefinition(
      opts, interner, keywords, hinputs, monouts, fullName, instantiationBoundArgs)

    return fullName
  }

  def translateInterfaceFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IInterfaceNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  IdI[IInterfaceNameI] = {
    val IdT(module, steps, last) = fullNameT
    val newFullName =
      IdI(
        module,
        steps.map(translateName(substitutions, perspectiveRegionT, _)),
        translateInterfaceName(substitutions, perspectiveRegionT, last))


    Instantiator.translateInterfaceDefinition(
      opts, interner, keywords, hinputs, monouts, newFullName, instantiationBoundArgs)

    newFullName
  }

  def translateCitizenName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    t: ICitizenNameT):
  ICitizenNameI = {
    t match {
      case s : IStructNameT => translateStructName(substitutions, perspectiveRegionT, s)
      case i : IInterfaceNameT => translateInterfaceName(substitutions, perspectiveRegionT, i)
    }
  }

  def translateFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    id: IdT[INameT]):
  IdI[INameI] = {
    id match {
      case other => vimpl(other)
    }
  }

  def translateCitizenFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    id: IdT[ICitizenNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  IdT[ICitizenNameI] = {
    id match {
      case IdT(module, steps, last : IStructNameT) => {
        translateStructFullName(
          substitutions, perspectiveRegionT, IdT(module, steps, last), instantiationBoundArgs)
      }
      case IdT(module, steps, last : IInterfaceNameT) => {
        translateInterfaceFullName(
          substitutions, perspectiveRegionT, IdT(module, steps, last), instantiationBoundArgs)
      }
      case other => vimpl(other)
    }
  }

  def translateImplFullName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    fullNameT: IdT[IImplNameT],
    instantiationBoundArgs: InstantiationBoundArgumentsI):
  IdI[IImplNameI] = {
    val IdT(module, steps, last) = fullNameT
    val fullName =
      IdI(
        module,
        steps.map(translateName(substitutions, perspectiveRegionT, _)),
        translateImplName(substitutions, perspectiveRegionT, last, instantiationBoundArgs))


    fullNameT match {
      case IdT(packageCoord, initSteps, name@ImplBoundNameT(_, _)) => {
        val implBoundNameT = IdT(packageCoord, initSteps, name)
        val result = vassertSome(denizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl.get(implBoundNameT))
        //        if (opts.sanityCheck) {
        //          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        //        }
        result
      }
      case IdT(_, _, _) => {
        monouts.newImpls.enqueue((fullNameT, fullName, instantiationBoundArgs))
        fullName
      }
    }
  }

  def translateCoord(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    coord: CoordT):
  CoordI = {
    val CoordT(outerOwnership, outerRegion, kind) = coord
    kind match {
      case KindPlaceholderT(placeholderFullName) => {
        // Let's get the index'th placeholder from the top level denizen.
        // If we're compiling a function or a struct, it might actually be a lambda function or lambda struct.
        // In these cases, the topLevelDenizenPlaceholderIndexToTemplata actually came from the containing function,
        // see LHPCTLD.

        vassertSome(vassertSome(substitutions.get(placeholderFullName.initFullName(interner))).get(placeholderFullName)) match {
          case CoordTemplataT(CoordT(innerOwnership, innerRegion, kind)) => {
            val combinedOwnership =
              ((outerOwnership, innerOwnership) match {
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
                  // DO NOT SUBMIT combine this with what's elsewhere in this file
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
                case other => other
              }
            vassert(innerRegion == translateTemplata(substitutions, perspectiveRegionT, outerRegion))
            CoordT(combinedOwnership, innerRegion, kind)
          }
          case KindTemplataT(kind) => {
            val newOwnership =
              getMutability(kind) match {
                case ImmutableT => ShareT
                case MutableT => outerOwnership
              }
            CoordT(vimpl(newOwnership), vimpl(), kind)
          }
        }
      }
      case other => {
        // We could, for example, be translating an Vector<myFunc$0, T> (which is temporarily regarded mutable)
        // to an Vector<imm, int> (which is immutable).
        // So, we have to check for that here and possibly make the ownership share.
        val kind = translateKind(substitutions, perspectiveRegionT, other)
        val mutability = getMutability(kind)
        val newOwnership =
          ((outerOwnership, mutability) match {
            case (_, ImmutableT) => ShareT
            case (other, MutableT) => other
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
            case other => other
          }
        val newRegion = expectRegionTemplata(translateTemplata(substitutions, perspectiveRegionT, outerRegion))
        CoordT(newOwnership, newRegion, translateKind(substitutions, perspectiveRegionT, other))
      }
    }
  }

  def getMutability(t: KindT): MutabilityI = {
    t match {
      case IntT(_) | BoolT() | StrT() | NeverT(_) | FloatT() | VoidT() => ImmutableI
      case StructTT(name) => {
        vassertSome(monouts.startedStructs.get(name))._1
      }
      case InterfaceTT(name) => {
        vassertSome(monouts.startedInterfaces.get(name))._1
      }
      case RuntimeSizedArrayTT(IdT(_, _, RuntimeSizedArrayNameT(_, RawArrayNameT(mutability, _, region)))) => {
        expectMutabilityTemplata(mutability).mutability
      }
      case StaticSizedArrayTT(IdT(_, _, StaticSizedArrayNameT(_, _, _, RawArrayNameT(mutability, _, region)))) => {
        expectMutabilityTemplata(mutability).mutability
      }
      case other => vimpl(other)
    }
  }

  def translateCitizen(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    citizen: ICitizenTT,
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  ICitizenIT = {
    citizen match {
      case s @ StructTT(_) => translateStruct(substitutions, perspectiveRegionT, s, instantiationBoundArgs)
      case s @ InterfaceTT(_) => translateInterface(substitutions, perspectiveRegionT, s, instantiationBoundArgs)
    }
  }

  def translateStruct(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    struct: StructTT,
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  StructTT = {
    val StructTT(fullName) = struct

    val desiredStruct =
      interner.intern(StructTT(
        translateStructFullName(
          substitutions, perspectiveRegionT, fullName, instantiationBoundArgs)))

    desiredStruct
  }

  def translateInterface(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    interface: InterfaceTT,
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  InterfaceTT = {
    val InterfaceTT(fullName) = interface

    val desiredInterface = interner.intern(InterfaceTT(translateInterfaceFullName(substitutions, perspectiveRegionT, fullName, instantiationBoundArgs)))

    desiredInterface
  }

  def translateSuperKind(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    kind: ISuperKindTT):
  ISuperKindIT = {
    kind match {
      case i @ InterfaceTT(_) => {
        translateInterface(
          substitutions,
          perspectiveRegionT,

          i,
          translateBoundArgsForCallee(
            substitutions,
            perspectiveRegionT,

            hinputs.getInstantiationBoundArgs(i.id)))
      }
      case p @ KindPlaceholderT(_) => {
        translatePlaceholder(substitutions, p) match {
          case s : ISuperKindTT => s
          case other => vwat(other)
        }
      }
    }
  }

  def translatePlaceholder(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    t: KindPlaceholderT):
  KindT = {
    val newSubstitutingTemplata =
      vassertSome(
        vassertSome(substitutions.get(t.id.initFullName(interner)))
        .get(t.id))
    ITemplataT.expectKindTemplata(newSubstitutingTemplata).kind
  }

  def translateStaticSizedArray(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    ssaTT: StaticSizedArrayTT):
  StaticSizedArrayIT = {
    val StaticSizedArrayTT(
    IdT(
    packageCoord,
    initSteps,
    StaticSizedArrayNameT(template, sizeT, variabilityT, RawArrayNameT(mutabilityT, elementTypeT, ssaRegionT)))) = ssaTT

    val newPerspectiveRegionT =
      ssaRegionT match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    // We use newPerspectiveRegionT for these because of TTTDRM.
    val ssaRegion = expectRegionTemplata(translateTemplata(substitutions, newPerspectiveRegionT, ssaRegionT))
    if (ssaRegion != RegionTemplataI(true)) {
      expectRegionTemplata(translateTemplata(substitutions, newPerspectiveRegionT, ssaRegionT))
    }
    vassert(ssaRegion == RegionTemplataI(true)) // Everything sees itself as mutable, see TTTDRM and CTOTFIPB.
    val intTemplata = expectIntegerTemplata(translateTemplata(substitutions, newPerspectiveRegionT, sizeT))
    val variabilityTemplata = expectVariabilityTemplata(translateTemplata(substitutions, newPerspectiveRegionT, variabilityT))
    val mutabilityTemplata = expectMutabilityTemplata(translateTemplata(substitutions, newPerspectiveRegionT, mutabilityT))
    val elementType = translateCoord(substitutions, newPerspectiveRegionT, elementTypeT)

    interner.intern(StaticSizedArrayTT(
      IdT(
        packageCoord,
        initSteps,
        interner.intern(StaticSizedArrayNameT(
          template,
          intTemplata,
          variabilityTemplata,
          interner.intern(RawArrayNameT(
            mutabilityTemplata,
            elementType,
            ssaRegion)))))))
  }

  def translateRuntimeSizedArray(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    ssaTT: RuntimeSizedArrayTT):
  RuntimeSizedArrayIT = {
    val RuntimeSizedArrayIT(
    IdT(
    packageCoord,
    initSteps,
    RuntimeSizedArrayNameT(template, RawArrayNameT(mutabilityT, elementTypeT, rsaRegionT)))) = ssaTT

    val newPerspectiveRegionT =
      rsaRegionT match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }

    // We use newPerspectiveRegionT for these because of TTTDRM.
    val rsaRegion = expectRegionTemplata(translateTemplata(substitutions, newPerspectiveRegionT, rsaRegionT))
    vassert(rsaRegion == RegionTemplataI(true)) // Everything sees itself as mutable, see TTTDRM and CTOTFIPB.
    val mutabilityTemplata = expectMutabilityTemplata(translateTemplata(substitutions, newPerspectiveRegionT, mutabilityT))
    val elementType = translateCoord(substitutions, newPerspectiveRegionT, elementTypeT)

    interner.intern(RuntimeSizedArrayTT(
      IdT(
        packageCoord,
        initSteps,
        interner.intern(RuntimeSizedArrayNameT(
          template,
          interner.intern(RawArrayNameT(
            mutabilityTemplata,
            elementType,
            rsaRegion)))))))
  }

  def translateKind(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    kind: KindT):
  KindIT = {
    kind match {
      case IntT(bits) => IntT(bits)
      case BoolT() => BoolT()
      case FloatT() => FloatT()
      case VoidT() => VoidT()
      case StrT() => StrT()
      case NeverT(fromBreak) => NeverT(fromBreak)
      case p @ KindPlaceholderT(_) => translatePlaceholder(substitutions, p)
      case s @ StructTT(_) => {
        translateStruct(
          substitutions,
          perspectiveRegionT,

          s,
          translateBoundArgsForCallee(
            substitutions, perspectiveRegionT, hinputs.getInstantiationBoundArgs(s.id)))
      }
      case s @ InterfaceTT(_) => {
        translateInterface(
          substitutions,
          perspectiveRegionT,

          s,
          translateBoundArgsForCallee(
            substitutions, perspectiveRegionT, hinputs.getInstantiationBoundArgs(s.id)))
      }
      case a @ contentsStaticSizedArrayTT(_, _, _, _, _) => translateStaticSizedArray(substitutions, perspectiveRegionT, a)
      case a @ contentsRuntimeSizedArrayTT(_, _, _) => translateRuntimeSizedArray(substitutions, perspectiveRegionT, a)
      case other => vimpl(other)
    }
  }

  def translateParameter(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    param: ParameterT):
  ParameterI = {
    val ParameterT(name, virtuality, tyype) = param
    ParameterI(
      translateVarName(name),
      virtuality,
      translateCoord(substitutions, perspectiveRegionT, tyype))
  }

  def translateTemplata(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    templata: ITemplataT[ITemplataType]):
  ITemplataI = {
    val result =
      templata match {
        case PlaceholderTemplataT(n, tyype) => {
          tyype match {
            case RegionTemplataType() => {
              n match {
                case IdT(packageCoord, initSteps, r@RegionPlaceholderNameT(_, _, pureHeight)) => {
                  val regionId = IdT(packageCoord, initSteps, r)
                  val mutable = regionIsMutable(substitutions, perspectiveRegionT, regionId)
                  RegionTemplataI(mutable)
                }
                case other => vwat(other)
              }
            }
            case _ => vassertSome(vassertSome(substitutions.get(n.initFullName(interner))).get(n))
          }
        }
        case IntegerTemplataT(value) => IntegerTemplataT(value)
        case BooleanTemplataT(value) => BooleanTemplataT(value)
        case StringTemplataT(value) => StringTemplataT(value)
        case CoordTemplataT(coord) => CoordTemplataT(translateCoord(substitutions, perspectiveRegionT, coord))
        case MutabilityTemplataT(mutability) => MutabilityTemplataT(mutability)
        case VariabilityTemplataT(variability) => VariabilityTemplataT(variability)
        case KindTemplataT(kind) => KindTemplataT(translateKind(substitutions, perspectiveRegionT, kind))
        case RegionTemplataI(mutable) => RegionTemplataI(mutable)
        case other => vimpl(other)
      }
    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case KindPlaceholderNameT(_) => }).isEmpty)
    }
    result
  }

  def translateVarName(
    name: IVarNameT):
  IVarNameI = {
    name match {
      case TypingPassFunctionResultVarNameT() => name
      case CodeVarNameT(_) => name
      case ClosureParamNameT(_) => name
      case TypingPassBlockResultVarNameT(life) => name
      case TypingPassTemporaryVarNameT(life) => name
      case ConstructingMemberNameT(_) => name
      case IterableNameT(range) => name
      case IteratorNameT(range) => name
      case IterationOptionNameT(range) => name
      case MagicParamNameT(codeLocation2) => name
      case SelfNameT() => name
      case other => vimpl(other)
    }
  }

  def translateFunctionName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IFunctionNameT):
  IFunctionNameI = {
    name match {
      case FunctionNameT(FunctionTemplateNameT(humanName, codeLoc), templateArgs, params) => {
        interner.intern(FunctionNameI(
          interner.intern(FunctionTemplateNameI(humanName, codeLoc)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(substitutions, perspectiveRegionT, _))))
      }
      case ForwarderFunctionNameT(ForwarderFunctionTemplateNameT(innerTemplate, index), inner) => {
        interner.intern(ForwarderFunctionNameI(
          interner.intern(ForwarderFunctionTemplateNameI(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            innerTemplate,
            index)),
          translateFunctionName(substitutions, perspectiveRegionT, inner)))
      }
      case ExternFunctionNameT(humanName, parameters) => {
        interner.intern(ExternFunctionNameI(
          humanName, parameters.map(translateCoord(substitutions, perspectiveRegionT, _))))
      }
      case FunctionBoundNameT(FunctionBoundTemplateNameT(humanName, codeLocation), templateArgs, params) => {
        interner.intern(FunctionBoundNameI(
          interner.intern(FunctionBoundTemplateNameI(humanName, codeLocation)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(substitutions, perspectiveRegionT, _))))
      }
      case AnonymousSubstructConstructorNameT(template, templateArgs, params) => {
        interner.intern(AnonymousSubstructConstructorNameI(
          translateName(substitutions, perspectiveRegionT, template) match {
            case x @ AnonymousSubstructConstructorTemplateNameI(_) => x
            case other => vwat(other)
          },
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          params.map(translateCoord(substitutions, perspectiveRegionT, _))))
      }
      case LambdaCallFunctionNameT(LambdaCallFunctionTemplateNameT(codeLocation, paramTypesForGeneric), templateArgs, paramTypes) => {
        interner.intern(LambdaCallFunctionNameI(
          interner.intern(LambdaCallFunctionTemplateNameI(
            codeLocation,
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            paramTypesForGeneric)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          paramTypes.map(translateCoord(substitutions, perspectiveRegionT, _))))
      }
      case other => vimpl(other)
    }
  }

  def translateImplName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IImplNameT,
    instantiationBoundArgs: InstantiationBoundArgumentsT):
  IImplNameI = {
    name match {
      case ImplNameT(ImplTemplateNameT(codeLocationS), templateArgs, subCitizen) => {
        interner.intern(ImplNameI(
          interner.intern(ImplTemplateNameI(codeLocationS)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          translateCitizen(
            substitutions,
            perspectiveRegionT,
            subCitizen,
            hinputs.getInstantiationBoundArgs(subCitizen.id))))
      }
      case ImplBoundNameT(ImplBoundTemplateNameT(codeLocationS), templateArgs) => {
        interner.intern(ImplBoundNameI(
          interner.intern(ImplBoundTemplateNameI(codeLocationS)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _))))
      }
      case AnonymousSubstructImplNameT(AnonymousSubstructImplTemplateNameT(interface), templateArgs, subCitizen) => {
        interner.intern(AnonymousSubstructImplNameI(
          interner.intern(AnonymousSubstructImplTemplateNameI(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            interface)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _)),
          translateCitizen(
            substitutions,
            perspectiveRegionT,
            subCitizen,
            hinputs.getInstantiationBoundArgs(subCitizen.id))))
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
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    // See TTTDRM, this is the region from which we're determining other regions' mutabilities.
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IStructNameT):
  IStructNameI = {
    val newPerspectiveRegionT =
      vassertSome(name.templateArgs.lastOption) match {
        case PlaceholderTemplataT(IdT(packageCoord, initSteps, r @ RegionPlaceholderNameT(_, _, _)), RegionTemplataType()) => {
          IdT(packageCoord, initSteps, r)
        }
        case _ => vwat()
      }
    name match {
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        interner.intern(StructNameI(
          interner.intern(StructTemplateNameI(humanName)),
          // We use newPerspectiveRegionT here because of TTTDRM.
          templateArgs.map(translateTemplata(substitutions, newPerspectiveRegionT, _))))
      }
      case AnonymousSubstructNameT(AnonymousSubstructTemplateNameT(interface), templateArgs) => {
        interner.intern(AnonymousSubstructNameI(
          interner.intern(AnonymousSubstructTemplateNameI(
            translateInterfaceTemplateName(interface))),
          // We use newPerspectiveRegionT here because of TTTDRM.
          templateArgs.map(translateTemplata(substitutions, newPerspectiveRegionT, _))))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => {
        LambdaCitizenNameI(LambdaCitizenTemplateNameI(codeLocation))
      }
      case other => vimpl(other)
    }
  }

  def translateInterfaceName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: IInterfaceNameT):
  IInterfaceNameI = {
    name match {
      case InterfaceNameT(InterfaceTemplateNameT(humanName), templateArgs) => {
        interner.intern(InterfaceNameI(
          interner.intern(InterfaceTemplateNameI(humanName)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _))))
      }
      case other => vimpl(other)
    }
  }

  def translateInterfaceTemplateName(
    name: IInterfaceTemplateNameT):
  IInterfaceTemplateNameI = {
    name match {
      case InterfaceTemplateNameT(humanName) => name
      case other => vimpl(other)
    }
  }

  def translateName(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    perspectiveRegionT: IdT[RegionPlaceholderNameT],
    name: INameT):
  INameI = {
    name match {
      case v : IVarNameT => translateVarName(v)
      case KindPlaceholderTemplateNameT(index, _) => vwat()
      case KindPlaceholderNameT(inner) => vwat()
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        interner.intern(StructNameI(
          interner.intern(StructTemplateNameI(humanName)),
          templateArgs.map(translateTemplata(substitutions, perspectiveRegionT, _))))
      }
      case ForwarderFunctionTemplateNameT(inner, index) => {
        interner.intern(ForwarderFunctionTemplateNameI(
          // We dont translate these, as these are what uniquely identify generics, and we need that
          // information later to map this back to its originating generic.
          // See DMPOGN for a more detailed explanation. This oddity is really tricky.
          vimpl(inner),
          index))
      }
      case AnonymousSubstructConstructorTemplateNameT(substructTemplateName) => {
        interner.intern(AnonymousSubstructConstructorTemplateNameI(
          translateName(substitutions, perspectiveRegionT, substructTemplateName) match {
            case x : ICitizenTemplateNameI => x
            case other => vwat(other)
          }))
      }
      case FunctionTemplateNameT(humanName, codeLoc) => FunctionTemplateNameI(humanName, codeLoc)
      case StructTemplateNameT(humanName) => StructTemplateNameI(humanName)
      case LambdaCitizenTemplateNameT(codeLoc) => LambdaCitizenTemplateNameI(codeLoc)
      case AnonymousSubstructTemplateNameT(interface) => {
        interner.intern(AnonymousSubstructTemplateNameI(
          translateInterfaceTemplateName(interface)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => {
        LambdaCitizenNameI(LambdaCitizenTemplateNameI(codeLocation))
      }
      case InterfaceTemplateNameT(humanNamee) => InterfaceTemplateNameI(humanNamee)
      //      case FreeTemplateNameT(codeLoc) => name
      case f : IFunctionNameT => translateFunctionName(substitutions, perspectiveRegionT, f)
      case other => vimpl(other)
    }
  }

  def translateImplDefinition(
    substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI]],
    implId: IdT[IImplNameT],
    implDefinition: EdgeT):
  Unit = {
    if (monouts.impls.contains(implId)) {
      return
    }

    val citizen =
      translateCitizen(
        substitutions,
        vimpl(),
        implDefinition.subCitizen,
        translateBoundArgsForCallee(
          substitutions,
          vimpl(),
          hinputs.getInstantiationBoundArgs(implDefinition.subCitizen.id)))
    val superInterface =
      translateInterfaceFullName(
        substitutions,
        vimpl(),
        implDefinition.superInterface,
        translateBoundArgsForCallee(
          substitutions,
          vimpl(),
          hinputs.getInstantiationBoundArgs(implDefinition.superInterface)))
    monouts.impls.put(implId, (citizen, superInterface, denizenBoundToDenizenCallerSuppliedThing, this))

    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverride.get(vimpl(superInterface)))
      .put(implId, mutable.HashMap())
    vassertSome(monouts.interfaceToImpls.get(vimpl(superInterface))).add(implId)


    vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(vimpl(superInterface)))
      .foreach({ case (abstractFuncPrototype, virtualIndex) =>
        Instantiator.translateOverride(
          opts, interner, keywords, hinputs, monouts, implId, abstractFuncPrototype)
      })
  }
}
