package dev.vale.instantiating

import dev.vale.options.GlobalOptions
import dev.vale.{Accumulator, Collector, Interner, Keywords, StrI, vassert, vassertOne, vassertSome, vcurious, vfail, vimpl, vpass, vregion, vwat}
import dev.vale.postparsing.{IRuneS, ITemplataType, IntegerTemplataType, RegionTemplataType}
import dev.vale.typing.TemplataCompiler.{getTopLevelDenizenId, substituteTemplatasInKind}
import dev.vale.typing.{Hinputs, InstantiationBoundArguments, TemplataCompiler}
import dev.vale.typing.ast.{EdgeT, _}
import dev.vale.typing.env._
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplata.{expectIntegerTemplata, expectKind, expectKindTemplata, expectMutabilityTemplata, expectRegionTemplata, expectVariabilityTemplata}
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.Map
import scala.collection.mutable

case class DenizenBoundToDenizenCallerBoundArg(
  funcBoundToCallerSuppliedBoundArgFunc: Map[IdT[FunctionBoundNameT], PrototypeT],
  implBoundToCallerSuppliedBoundArgImpl: Map[IdT[ImplBoundNameT], IdT[IImplNameT]])

class InstantiatedOutputs() {
  val functions: mutable.HashMap[IdT[IFunctionNameT], FunctionDefinitionT] =
    mutable.HashMap[IdT[IFunctionNameT], FunctionDefinitionT]()
  val structs: mutable.HashMap[IdT[IStructNameT], StructDefinitionT] = mutable.HashMap()
  val interfacesWithoutMethods: mutable.HashMap[IdT[IInterfaceNameT], InterfaceDefinitionT] = mutable.HashMap()

  // We can get some recursion if we have a self-referential struct like:
  //   struct Node<T> { value T; next Opt<Node<T>>; }
  // So we need these to short-circuit that nonsense.
  val startedStructs: mutable.HashMap[IdT[IStructNameT], (MutabilityT, DenizenBoundToDenizenCallerBoundArg)] = mutable.HashMap()
  val startedInterfaces: mutable.HashMap[IdT[IInterfaceNameT], (MutabilityT, DenizenBoundToDenizenCallerBoundArg)] = mutable.HashMap()

  //  val immKindToDestructor: mutable.HashMap[KindT, PrototypeT] =
  //    mutable.HashMap[KindT, PrototypeT]()

  // We already know from the hinputs that Some<T> implements Opt<T>.
  // In this map, we'll know that Some<int> implements Opt<int>, Some<bool> implements Opt<bool>, etc.
  val interfaceToImpls: mutable.HashMap[IdT[IInterfaceNameT], mutable.HashSet[IdT[IImplNameT]]] =
  mutable.HashMap()
  val interfaceToAbstractFuncToVirtualIndex: mutable.HashMap[IdT[IInterfaceNameT], mutable.HashMap[PrototypeT, Int]] =
    mutable.HashMap()
  val impls:
    mutable.HashMap[
      IdT[IImplNameT],
      (ICitizenTT, IdT[IInterfaceNameT], DenizenBoundToDenizenCallerBoundArg, Instantiator)] =
    mutable.HashMap()
  // We already know from the hinputs that Opt<T has drop> has func drop(T).
  // In this map, we'll know that Opt<int> has func drop(int).
  val abstractFuncToInstantiatorAndSuppliedPrototypes: mutable.HashMap[IdT[IFunctionNameT], (Instantiator, InstantiationBoundArguments)] =
  mutable.HashMap()
  // This map collects all overrides for every impl. We'll use it to assemble vtables soon.
  val interfaceToImplToAbstractPrototypeToOverride:
    mutable.HashMap[IdT[IInterfaceNameT], mutable.HashMap[IdT[IImplNameT], mutable.HashMap[PrototypeT, OverrideT]]] =
    mutable.HashMap()

  // These are new impls and abstract funcs we discover for interfaces.
  // As we discover a new impl or a new abstract func, we'll later need to stamp a lot more overrides either way.
  val newImpls: mutable.Queue[(IdT[IImplNameT], InstantiationBoundArguments)] = mutable.Queue()
  // The int is a virtual index
  val newAbstractFuncs: mutable.Queue[(PrototypeT, Int, IdT[IInterfaceNameT], InstantiationBoundArguments)] = mutable.Queue()
  val newFunctions: mutable.Queue[(PrototypeT, InstantiationBoundArguments, Option[DenizenBoundToDenizenCallerBoundArg])] = mutable.Queue()

  def addMethodToVTable(
    implId: IdT[IImplNameT],
    superInterfaceId: IdT[IInterfaceNameT],
    abstractFuncPrototype: PrototypeT,
    overrride: OverrideT
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
  def translate(opts: GlobalOptions, interner: Interner, keywords: Keywords, originalHinputs: Hinputs): Hinputs = {
    val Hinputs(
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

        val instantiator =
          new Instantiator(
            opts,
            interner,
            keywords,
            originalHinputs,
            monouts,
            exportTemplateId,
            exportId,
            vimpl(), //Map(exportId -> assemblePlaceholderMap(hinputs, exportId)),
            DenizenBoundToDenizenCallerBoundArg(Map(), Map()))
        KindExportT(
          range,
          instantiator.translateKind(tyype),
          exportId,
          exportedName)
      })

    val functionExports =
      functionExportsT.map({ case FunctionExportT(range, prototypeT, exportPlaceholderedId, exportedName) =>
  //      val packageName = IdT(exportId.packageCoord, Vector(), interner.intern(PackageTopLevelNameT()))
  //      val exportName =
  //        packageName.addStep(
  //          interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)), )))

        val exportTemplateId = TemplataCompiler.getExportTemplate(exportPlaceholderedId)

        val IdT(packageCoord, initSteps, ExportNameT(exportTemplate, PlaceholderTemplata(_, RegionTemplataType()))) = exportPlaceholderedId
        // Someday we'll have exports that do immutable things... not yet though.
        val region = RegionTemplata(true)
        val exportId = IdT(packageCoord, initSteps, interner.intern(ExportNameT(exportTemplate, region)))

        val exportTemplateName = TemplataCompiler.getExportTemplate(exportId)
        val instantiator =
          new Instantiator(
            opts,
            interner,
            keywords,
            originalHinputs,
            monouts,
            exportTemplateName,
            exportId,
            Map(exportTemplateId -> assemblePlaceholderMap(originalHinputs, exportId)),
            DenizenBoundToDenizenCallerBoundArg(Map(), Map()))
        Collector.all(exportId, { case PlaceholderTemplata(_, _) => vwat() })
        val prototype = instantiator.translatePrototype(prototypeT)
        Collector.all(prototype, { case PlaceholderTemplata(_, _) => vwat() })
        FunctionExportT(range, prototype, exportId, exportedName)
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
        val (newFuncName, instantiationBoundArgs, maybeDenizenBoundToDenizenCallerSuppliedThing) =
          monouts.newFunctions.dequeue()
        Instantiator.translateFunction(
          opts, interner, keywords, originalHinputs, monouts, newFuncName, instantiationBoundArgs,
          maybeDenizenBoundToDenizenCallerSuppliedThing)
        true
      } else if (monouts.newImpls.nonEmpty) {
        val (implFullName, instantiationBoundsForUnsubstitutedImpl) = monouts.newImpls.dequeue()
        Instantiator.translateImpl(
          opts, interner, keywords, originalHinputs, monouts, implFullName, instantiationBoundsForUnsubstitutedImpl)
        true
      } else if (monouts.newAbstractFuncs.nonEmpty) {
        val (abstractFunc, virtualIndex, interfaceFullName, instantiationBoundArgs) = monouts.newAbstractFuncs.dequeue()
        Instantiator.translateAbstractFunc(
          opts, interner, keywords, originalHinputs, monouts, interfaceFullName, abstractFunc, virtualIndex, instantiationBoundArgs)
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
        interface ->
          InterfaceEdgeBlueprint(
            interface,
            abstractFuncPrototypes.toVector)
      }).toMap

    val interfaces =
      monouts.interfacesWithoutMethods.values.map(interface => {
        val InterfaceDefinitionT(templateName, instantiatedInterface, ref, attributes, weakable, mutability, _, _, _) = interface
        InterfaceDefinitionT(
          templateName, instantiatedInterface, ref, attributes, weakable, mutability, Map(), Map(),
          vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interface.ref.id)).toVector)
      })

    val interfaceToSubCitizenToEdge =
      monouts.interfaceToImpls.map({ case (interface, impls) =>
        interface ->
          impls.map(implFullName => {
            val (subCitizen, parentInterface, _, implInstantiator) = vassertSome(monouts.impls.get(implFullName))
            vassert(parentInterface == interface)
            val abstractFuncToVirtualIndex =
              vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interface))
            val abstractFuncPrototypeToOverridePrototype =
              abstractFuncToVirtualIndex.map({ case (abstractFuncPrototype, virtualIndex) =>
                val overrride =
                  vassertSome(
                    vassertSome(
                      vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverride.get(interface))
                        .get(implFullName))
                      .get(abstractFuncPrototype))

                vassert(
                  abstractFuncPrototype.id.localName.parameters(virtualIndex).kind !=
                    overrride.overridePrototype.id.localName.parameters(virtualIndex).kind)

                abstractFuncPrototype.id -> overrride
              })
            val edge =
              EdgeT(
                implFullName,
                subCitizen,
                interface,
                Map(),
                Map(),
                abstractFuncPrototypeToOverridePrototype.toMap)
            subCitizen.id -> edge
          }).toMap
      }).toMap

    val resultHinputs =
      Hinputs(
        interfaces.toVector,
        monouts.structs.values.toVector,
        monouts.functions.values.toVector,
        //      monouts.immKindToDestructor.toMap,
        interfaceEdgeBlueprints,
        interfaceToSubCitizenToEdge,
        Map(),
        kindExports,
        functionExports,
        kindExternsT,
        functionExternsT)

    if (opts.sanityCheck) {
      Collector.all(resultHinputs, {
        case BorrowT => vfail()
      })
    }

    resultHinputs
  }

  def translateInterfaceDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    interfaceId: IdT[IInterfaceNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  Unit = {
    val interfaceTemplate = TemplataCompiler.getInterfaceTemplate(interfaceId)

    val interfaceDefT =
      vassertOne(hinputs.interfaces.filter(_.templateName == interfaceTemplate))

    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        interfaceTemplate,
        interfaceId,
        Map(interfaceTemplate -> assemblePlaceholderMap(hinputs, interfaceId)),
        DenizenBoundToDenizenCallerBoundArg(
          assembleCalleeDenizenFunctionBounds(
            interfaceDefT.runeToFunctionBound,
            instantiationBoundArgs.runeToFunctionBoundArg),
          assembleCalleeDenizenImplBounds(
            interfaceDefT.runeToImplBound,
            instantiationBoundArgs.runeToImplBoundArg)))
    instantiator.translateInterfaceDefinition(interfaceId, interfaceDefT)
  }

  def assembleCalleeDenizenFunctionBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[FunctionBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeT]
  ): Map[IdT[FunctionBoundNameT], PrototypeT] = {
    calleeRuneToSuppliedPrototype.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def assembleCalleeDenizenImplBounds(
    // This is from the receiver's perspective, they have some runes for their required functions.
    calleeRuneToReceiverBoundT: Map[IRuneS, IdT[ImplBoundNameT]],
    // This is a map from the receiver's rune to the bound that the caller is supplying.
    calleeRuneToSuppliedImpl: Map[IRuneS, IdT[IImplNameT]]
  ): Map[IdT[ImplBoundNameT], IdT[IImplNameT]] = {
    calleeRuneToSuppliedImpl.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(calleeRuneToReceiverBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def translateStructDefinition(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    structId: IdT[IStructNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  Unit = {
    if (opts.sanityCheck) {
      vassert(Collector.all(structId, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    val structTemplate = TemplataCompiler.getStructTemplate(structId)

    val structDefT = findStruct(hinputs, structId)

    val topLevelDenizenFullName =
      getTopLevelDenizenId(structId)
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArg(
        assembleCalleeDenizenFunctionBounds(
          structDefT.runeToFunctionBound, instantiationBoundArgs.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          structDefT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg))
    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        structTemplate,
        structId,
        Map(
          topLevelDenizenTemplateFullName ->
            assemblePlaceholderMap(
              hinputs,
              // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
              // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
              // This struct might not be the top level denizen, such as if it's a lambda.
              topLevelDenizenFullName)),
        denizenBoundToDenizenCallerSuppliedThing)

    instantiator.translateStructDefinition(structId, structDefT)
  }

  private def findStruct(hinputs: Hinputs, structId: IdT[IStructNameT]) = {
    vassertOne(
      hinputs.structs
        .filter(structT => {
          TemplataCompiler.getSuperTemplate(structT.instantiatedCitizen.id) ==
            TemplataCompiler.getSuperTemplate(structId)
        }))
  }

  private def findInterface(hinputs: Hinputs, interfaceId: IdT[IInterfaceNameT]) = {
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
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    interfaceId: IdT[IInterfaceNameT],
    abstractFunc: PrototypeT,
    virtualIndex: Int,
    instantiationBoundArgs: InstantiationBoundArguments):
  Unit = {
    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(abstractFunc.id)

    val funcT =
      vassertOne(
        hinputs.functions.filter(func => {
          TemplataCompiler.getFunctionTemplate(func.header.id) == funcTemplateNameT
        }))

    val abstractFuncInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        funcTemplateNameT,
        abstractFunc.id,
        Map(
          funcTemplateNameT ->
            assemblePlaceholderMap(hinputs, abstractFunc.id)),
        DenizenBoundToDenizenCallerBoundArg(
          assembleCalleeDenizenFunctionBounds(
            funcT.runeToFuncBound, instantiationBoundArgs.runeToFunctionBoundArg),
          assembleCalleeDenizenImplBounds(
            funcT.runeToImplBound, instantiationBoundArgs.runeToImplBoundArg)))

    vassert(!monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.contains(abstractFunc.id))
    monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.put(abstractFunc.id, (abstractFuncInstantiator, instantiationBoundArgs))

    val abstractFuncs = vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interfaceId))
    vassert(!abstractFuncs.contains(abstractFunc))
    abstractFuncs.put(abstractFunc, virtualIndex)

    vassertSome(monouts.interfaceToImpls.get(interfaceId)).foreach(impl => {
      translateOverride(opts, interner, keywords, hinputs, monouts, impl, abstractFunc)
    })
  }

  def translateOverride(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    implId: IdT[IImplNameT],
    abstractFuncPrototype: PrototypeT):
  Unit = {
    //    val superInterfaceFullName: FullNameT[IInterfaceNameT],

    val implTemplateFullName = TemplataCompiler.getImplTemplate(implId)
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

    val abstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(abstractFuncPrototype.id)
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
      vassertSome(monouts.abstractFuncToInstantiatorAndSuppliedPrototypes.get(abstractFuncPrototype.id))
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

    val dispatcherInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        TemplataCompiler.getFunctionTemplate(dispatcherFullNameT),
        dispatcherFullNameT,
        Map(dispatcherTemplateId -> dispatcherPlaceholderFullNameToSuppliedTemplata.toMap),
        DenizenBoundToDenizenCallerBoundArg(
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
      DenizenBoundToDenizenCallerBoundArg(
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

    val caseInstantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        dispatcherCaseFullNameT,
        dispatcherCaseFullNameT,
        Map(
          dispatcherTemplateId -> dispatcherPlaceholderFullNameToSuppliedTemplataMap,
          dispatcherFullNameT -> dispatcherCasePlaceholderFullNameToSuppliedTemplataMap),
        caseDenizenBoundToDenizenCallerSuppliedThing)


    // right here we're calling it from the perspective of the abstract function
    // we need to call it from the perspective of the abstract dispatcher function's case.
    // we might need a sub-instantiator if that makes sense...

    // we need to make a instantiator that thinks in terms of impl overrides.

    val overridePrototype = caseInstantiator.translatePrototype(overridePrototypeT)

    val superInterfaceFullName = vassertSome(monouts.impls.get(implId))._2

    val overrride =
      OverrideT(
        dispatcherFullNameT, Vector(), Vector(), Map(), Map(), Map(), dispatcherCaseFullNameT, overridePrototype)
    monouts.addMethodToVTable(implId, superInterfaceFullName, abstractFuncPrototype, overrride)
  }

  def translateImpl(
    opts: GlobalOptions,
    interner: Interner,
    keywords: Keywords,
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    implId: IdT[IImplNameT],
    instantiationBoundsForUnsubstitutedImpl: InstantiationBoundArguments):
  Unit = {
    val implTemplateFullName = TemplataCompiler.getImplTemplate(implId)
    val implDefinition =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => {
            TemplataCompiler.getImplTemplate(edge.edgeId) == implTemplateFullName
          }))


    val subCitizenT = implDefinition.subCitizen
    val subCitizenM =
      implId.localName match {
        case ImplNameT(template, templateArgs, subCitizen) => subCitizen
        case AnonymousSubstructImplNameT(template, templateArgs, subCitizen) => subCitizen
        case other => vimpl(other)
      }

    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItself =
      DenizenBoundToDenizenCallerBoundArg(
        assembleCalleeDenizenFunctionBounds(
          implDefinition.runeToFuncBound, instantiationBoundsForUnsubstitutedImpl.runeToFunctionBoundArg),
        assembleCalleeDenizenImplBounds(
          implDefinition.runeToImplBound, instantiationBoundsForUnsubstitutedImpl.runeToImplBoundArg))
    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams =
      Vector(denizenBoundToDenizenCallerSuppliedThingFromDenizenItself) ++
        hoistBoundsFromParameter(hinputs, monouts, subCitizenT, subCitizenM)

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArg(
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.funcBoundToCallerSuppliedBoundArgFunc)
          .reduceOption(_ ++ _).getOrElse(Map()),
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.implBoundToCallerSuppliedBoundArgImpl)
          .reduceOption(_ ++ _).getOrElse(Map()))

    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        implTemplateFullName,
        implId,
        Map(implTemplateFullName -> assemblePlaceholderMap(hinputs, implId)),
        denizenBoundToDenizenCallerSuppliedThing)
    instantiator.translateImplDefinition(implId, implDefinition)


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
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    desiredPrototype: PrototypeT,
    suppliedBoundArgs: InstantiationBoundArguments,
    // This is only Some if this is a lambda. This will contain the prototypes supplied to the top level denizen by its
    // own caller, see LCNBAFA.
    maybeDenizenBoundToDenizenCallerSuppliedThing: Option[DenizenBoundToDenizenCallerBoundArg]):
  FunctionDefinitionT = {
    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(desiredPrototype.id)

    val desiredFuncSuperTemplateName = TemplataCompiler.getSuperTemplate(desiredPrototype.id)
    val funcT =
      vassertOne(
        hinputs.functions
          .filter(funcT => TemplataCompiler.getSuperTemplate(funcT.header.id) == desiredFuncSuperTemplateName))


    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItself =
      maybeDenizenBoundToDenizenCallerSuppliedThing.getOrElse({
        DenizenBoundToDenizenCallerBoundArg(
          // This is a top level denizen, and someone's calling it. Assemble the bounds!
          assembleCalleeDenizenFunctionBounds(funcT.runeToFuncBound, suppliedBoundArgs.runeToFunctionBoundArg),
          // This is a top level denizen, and someone's calling it. Assemble the bounds!
          assembleCalleeDenizenImplBounds(funcT.runeToImplBound, suppliedBoundArgs.runeToImplBoundArg))
      })
    val argsM = desiredPrototype.id.localName.parameters.map(_.kind)
    val paramsT = funcT.header.params.map(_.tyype.kind)
    val denizenBoundToDenizenCallerSuppliedThingFromParams =
      paramsT.zip(argsM).flatMap({ case (a, x) =>
        hoistBoundsFromParameter(hinputs, monouts, a, x)
      })

    val denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams =
      Vector(denizenBoundToDenizenCallerSuppliedThingFromDenizenItself) ++
        denizenBoundToDenizenCallerSuppliedThingFromParams

    val denizenBoundToDenizenCallerSuppliedThing =
      DenizenBoundToDenizenCallerBoundArg(
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.funcBoundToCallerSuppliedBoundArgFunc)
          .reduceOption(_ ++ _).getOrElse(Map()),
        denizenBoundToDenizenCallerSuppliedThingFromDenizenItselfAndParams
          .map(_.implBoundToCallerSuppliedBoundArgImpl)
          .reduceOption(_ ++ _).getOrElse(Map()))


    val topLevelDenizenFullName =
      getTopLevelDenizenId(desiredPrototype.id)
    val topLevelDenizenTemplateFullName =
      TemplataCompiler.getTemplate(topLevelDenizenFullName)
    // One would imagine we'd get structFullName.last.templateArgs here, because that's the struct
    // we're about to monomorphize. However, only the top level denizen has placeholders, see LHPCTLD.
    val topLevelDenizenPlaceholderIndexToTemplata =
    topLevelDenizenFullName.localName.templateArgs


    val instantiator =
      new Instantiator(
        opts,
        interner,
        keywords,
        hinputs,
        monouts,
        funcTemplateNameT,
        desiredPrototype.id,
        Map(
          topLevelDenizenTemplateFullName ->
          assemblePlaceholderMap(hinputs, topLevelDenizenFullName)),
        denizenBoundToDenizenCallerSuppliedThing)

    desiredPrototype.id match {
      case IdT(_,Vector(),FunctionNameT(FunctionTemplateNameT(StrI("as"),_),_, _)) => {
        vpass()
      }
      case _ => false
    }

    val monomorphizedFuncT = instantiator.translateFunction(funcT)

    vassert(desiredPrototype.returnType == monomorphizedFuncT.header.returnType)

    monomorphizedFuncT
  }

  // This isn't just for parameters, it's for impl subcitizens, and someday for cases too.
  // See NBIFP
  private def hoistBoundsFromParameter(
    hinputs: Hinputs,
    monouts: InstantiatedOutputs,
    paramT: KindT,
    paramM: KindT):
  Option[DenizenBoundToDenizenCallerBoundArg] = {
    (paramT, paramM) match {
      case (StructTT(structFullNameT), StructTT(structFullNameM)) => {
        val calleeRuneToBoundArgT = hinputs.getInstantiationBoundArgs(structFullNameT)
        val (_, structDenizenBoundToDenizenCallerSuppliedThing) = vassertSome(monouts.startedStructs.get(structFullNameM))
        val structT = findStruct(hinputs, structFullNameT)
        val denizenBoundToDenizenCallerSuppliedThing =
          hoistBoundsFromParameterInner(
            structDenizenBoundToDenizenCallerSuppliedThing, calleeRuneToBoundArgT, structT.runeToFunctionBound, structT.runeToImplBound)
        Some(denizenBoundToDenizenCallerSuppliedThing)
      }
      case (InterfaceTT(interfaceFullNameT), InterfaceTT(interfaceFullNameM)) => {
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
    parameterDenizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArg,
    calleeRuneToBoundArgT: InstantiationBoundArguments,
    calleeRuneToCalleeFunctionBoundT: Map[IRuneS, IdT[FunctionBoundNameT]],
    calleeRuneToCalleeImplBoundT: Map[IRuneS, IdT[ImplBoundNameT]]):
  DenizenBoundToDenizenCallerBoundArg = {
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
      DenizenBoundToDenizenCallerBoundArg(
        callerSuppliedBoundToInstantiatedFunction,
        callerSuppliedBoundToInstantiatedImpl)
    denizenBoundToDenizenCallerSuppliedThing
  }

  def assemblePlaceholderMap(hinputs: Hinputs, id: IdT[IInstantiationNameT]):
  Map[IdT[IPlaceholderNameT], ITemplata[ITemplataType]] = {
    val containersPlaceholderMap =
      // This might be a lambda's name. If it is, its name has an init step that's the parent
      // function's name, and we want its mappings too.
      (id.initNonPackageFullName() match {
        case Some(IdT(packageCoord, initSteps, parentLocalName : IInstantiationNameT)) => {
          assemblePlaceholderMap(hinputs, IdT(packageCoord, initSteps, parentLocalName))
        }
        case _ => Map[IdT[IPlaceholderNameT], ITemplata[ITemplataType]]()
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
        case (CoordTemplata(CoordT(placeholderOwnership, PlaceholderTemplata(regionPlaceholderId, RegionTemplataType()), KindPlaceholderT(kindPlaceholderId))), c @ CoordTemplata(_)) => {
          vassert(placeholderOwnership == OwnT || placeholderOwnership == ShareT)
          // We might need to do something with placeholderRegion here, but I think we can just
          // assume it correctly matches up with the coord's region. The typing phase should have
          // made sure it matches up nicely.
          List(
            (regionPlaceholderId -> c.coord.region),
            (kindPlaceholderId -> c))
        }
        case (KindTemplata(KindPlaceholderT(placeholderId)), kindTemplata) => {
          List((placeholderId -> kindTemplata))
        }
        case (PlaceholderTemplata(placeholderId, tyype), templata) => {
          List((placeholderId -> templata))
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
  hinputs: Hinputs,
  monouts: InstantiatedOutputs,
  denizenTemplateName: IdT[ITemplateNameT],
  denizenName: IdT[IInstantiationNameT],

  // This IdT might be the top level denizen and not necessarily *this* denizen, see LHPCTLD.
  substitutions: Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplata[ITemplataType]]],

  val denizenBoundToDenizenCallerSuppliedThing: DenizenBoundToDenizenCallerBoundArg) {
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

  def translateStructMember(member: IStructMemberT): IStructMemberT = {
    member match {
      case NormalStructMemberT(name, variability, tyype) => {
        NormalStructMemberT(
          translateVarName(name),
          variability,
          tyype match {
            case ReferenceMemberTypeT((unsubstitutedCoord)) => {
              ReferenceMemberTypeT(translateCoord(unsubstitutedCoord))
            }
            case AddressMemberTypeT((unsubstitutedCoord)) => {
              AddressMemberTypeT(translateCoord(unsubstitutedCoord))
            }
          })
      }
      case VariadicStructMemberT(name, tyype) => {
        vimpl()
      }
    }
  }

  // This is run at the call site, from the caller's perspective
  def translatePrototype(
    desiredPrototypeUnsubstituted: PrototypeT):
  PrototypeT = {
    val PrototypeT(desiredPrototypeFullNameUnsubstituted, desiredPrototypeReturnTypeUnsubstituted) = desiredPrototypeUnsubstituted

    val runeToBoundArgsForCall =
      translateBoundArgsForCallee(
        hinputs.getInstantiationBoundArgs(desiredPrototypeUnsubstituted.id))

    val desiredPrototype =
      PrototypeT(
        translateFunctionFullName(desiredPrototypeFullNameUnsubstituted),
        translateCoord(desiredPrototypeReturnTypeUnsubstituted))

    desiredPrototypeUnsubstituted.id match {
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

        monouts.newFunctions.enqueue(
          (
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
    // This contains a map from rune to a prototype, specifically the prototype that we
    // (the *template* caller) is supplying to the *template* callee. This prototype might
    // be a placeholder, phrased in terms of our (the *template* caller's) placeholders
    instantiationBoundArgsForCallUnsubstituted: InstantiationBoundArguments):
  InstantiationBoundArguments = {
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
              translatePrototype(suppliedPrototypeUnsubstituted)
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
                  hinputs.getInstantiationBoundArgs(suppliedImplUnsubstituted))
              translateImplFullName(suppliedImplUnsubstituted, runeToBoundArgsForCall)
            }
          })
      })
    // And now we have a map from the callee's rune to the *instantiated* callee's impls.

    InstantiationBoundArguments(runeToSuppliedPrototypeForCall, runeToSuppliedImplForCall)
  }

  def translateStructDefinition(
    newId: IdT[IStructNameT],
    structDefT: StructDefinitionT):
  Unit = {
    val StructDefinitionT(templateName, instantiatedCitizen, attributes, weakable, mutabilityT, members, isClosure, _, _) = structDefT

    if (opts.sanityCheck) {
      vassert(Collector.all(newId, { case KindPlaceholderNameT(_) => }).isEmpty)
    }

    val mutability = expectMutabilityTemplata(translateTemplata(mutabilityT)).mutability

    if (monouts.startedStructs.contains(newId)) {
      return
    }
    monouts.startedStructs.put(newId, (mutability, this.denizenBoundToDenizenCallerSuppliedThing))

    val result =
      StructDefinitionT(
        templateName,
        interner.intern(StructTT(newId)),
        attributes,
        weakable,
        MutabilityTemplata(mutability),
        members.map(translateStructMember),
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
    newId: IdT[IInterfaceNameT],
    interfaceDefT: InterfaceDefinitionT):
  Unit = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutabilityT, _, _, internalMethods) = interfaceDefT

    val mutability = expectMutabilityTemplata(translateTemplata(mutabilityT)).mutability

    if (monouts.startedInterfaces.contains(newId)) {
      return
    }
    monouts.startedInterfaces.put(newId, (mutability, this.denizenBoundToDenizenCallerSuppliedThing))

    val newInterfaceTT = interner.intern(InterfaceTT(newId))

    val result =
      InterfaceDefinitionT(
        templateName,
        newInterfaceTT,
        newInterfaceTT,
        attributes,
        weakable,
        MutabilityTemplata(mutability),
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

  def translateFunctionHeader(header: FunctionHeaderT): FunctionHeaderT = {
    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = header

    val newFullName = translateFunctionFullName(fullName)

    val result =
      FunctionHeaderT(
        newFullName,
        attributes,
        params.map(translateParameter),
        translateCoord(returnType),
        maybeOriginFunctionTemplata)

    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(result.fullName, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.attributes, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.params, { case PlaceholderNameT(_) => }).isEmpty)
    //      vassert(Collector.all(result.returnType, { case PlaceholderNameT(_) => }).isEmpty)
    //    }

    result
  }

  def translateFunction(
    functionT: FunctionDefinitionT):
  FunctionDefinitionT = {
    val FunctionDefinitionT(headerT, _, _, bodyT) = functionT

    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = headerT

    val newFullName = translateFunctionFullName(fullName)

    monouts.functions.get(newFullName) match {
      case Some(func) => return func
      case None =>
    }

    val newHeader = translateFunctionHeader(headerT)

    val result = FunctionDefinitionT(newHeader, Map(), Map(), translateRefExpr(bodyT))
    monouts.functions.put(result.header.id, result)
    result
  }

  def translateLocalVariable(
    variable: ILocalVariableT):
  ILocalVariableT = {
    variable match {
      case r @ ReferenceLocalVariableT(_, _, _) => translateReferenceLocalVariable(r)
      case AddressibleLocalVariableT(id, variability, coord) => {
        AddressibleLocalVariableT(
          translateVarFullName(id),
          variability,
          translateCoord(coord))
      }
    }
  }

  def translateReferenceLocalVariable(
    variable: ReferenceLocalVariableT):
  ReferenceLocalVariableT = {
    val ReferenceLocalVariableT(id, variability, reference) = variable
    ReferenceLocalVariableT(
      translateVarFullName(id),
      variability,
      translateCoord(reference))
  }

  def translateAddrExpr(
    expr: AddressExpressionTE):
  AddressExpressionTE = {
    expr match {
      case LocalLookupTE(range, localVariable) => {
        LocalLookupTE(range, translateLocalVariable(localVariable))
      }
      case ReferenceMemberLookupTE(range, structExpr, memberName, memberCoord, variability) => {
        ReferenceMemberLookupTE(
          range,
          translateRefExpr(structExpr),
          translateVarFullName(memberName),
          translateCoord(memberCoord),
          variability)
      }
      case StaticSizedArrayLookupTE(range, arrayExpr, arrayType, indexExpr, variability) => {
        StaticSizedArrayLookupTE(
          range,
          translateRefExpr(arrayExpr),
          translateStaticSizedArray(arrayType),
          translateRefExpr(indexExpr),
          variability)
      }
      case AddressMemberLookupTE(range, structExpr, memberName, resultType2, variability) => {
        AddressMemberLookupTE(
          range,
          translateRefExpr(structExpr),
          translateVarFullName(memberName),
          translateCoord(resultType2),
          variability)
      }
      case RuntimeSizedArrayLookupTE(range, arrayExpr, arrayType, indexExpr, variability) => {
        RuntimeSizedArrayLookupTE(
          range,
          translateRefExpr(arrayExpr),
          translateRuntimeSizedArray(arrayType),
          translateRefExpr(indexExpr),
          variability)
      }
      case other => vimpl(other)
    }
  }

  def translateExpr(
    expr: ExpressionT):
  ExpressionT = {
    expr match {
      case r : ReferenceExpressionTE => translateRefExpr(r)
      case a : AddressExpressionTE => translateAddrExpr(a)
    }
  }

  def translateRefExpr(
    expr: ReferenceExpressionTE):
  ReferenceExpressionTE = {
    val resultRefExpr =
      expr match {
        case LetNormalTE(variable, inner) => LetNormalTE(translateLocalVariable(variable), translateRefExpr(inner))
        case BlockTE(inner) => BlockTE(translateRefExpr(inner))
        case ReturnTE(inner) => ReturnTE(translateRefExpr(inner))
        case ConsecutorTE(inners) => ConsecutorTE(inners.map(translateRefExpr))
        case ConstantIntTE(value, bits, region) => {
          ConstantIntTE(
            ITemplata.expectIntegerTemplata(translateTemplata(value)),
            bits,
            ITemplata.expectRegion(translateTemplata(region)))
        }
        case ConstantStrTE(value, region) => ConstantStrTE(value, ITemplata.expectRegion(translateTemplata(region)))
        case ConstantBoolTE(value, region) => ConstantBoolTE(value, ITemplata.expectRegion(translateTemplata(region)))
        case ConstantFloatTE(value, region) => ConstantFloatTE(value, ITemplata.expectRegion(translateTemplata(region)))
        case UnletTE(variable) => UnletTE(translateLocalVariable(variable))
        case DiscardTE(expr) => DiscardTE(translateRefExpr(expr))
        case VoidLiteralTE(region) => {
          VoidLiteralTE(ITemplata.expectRegion(translateTemplata(region)))
        }
        case FunctionCallTE(prototypeT, args) => {
          val prototype = translatePrototype(prototypeT)
          FunctionCallTE(
            prototype,
            args.map(translateRefExpr))
        }
        case InterfaceFunctionCallTE(superFunctionPrototypeT, virtualParamIndex, resultReference, args) => {
          val superFunctionPrototype = translatePrototype(superFunctionPrototypeT)
          val result =
            InterfaceFunctionCallTE(
              superFunctionPrototype,
              virtualParamIndex,
              translateCoord(resultReference),
              args.map(translateRefExpr))
          val interfaceFullName =
            superFunctionPrototype.paramTypes(virtualParamIndex).kind.expectInterface().id
          //        val interfaceFullName =
          //          translateInterfaceFullName(
          //            interfaceFullNameT,
          //            translateBoundArgsForCallee(
          //              hinputs.getInstantiationBounds(callee.toPrototype.fullName)))

          val instantiationBoundArgs =
            translateBoundArgsForCallee(
              // but this is literally calling itself from where its defined
              // perhaps we want the thing that originally called
              hinputs.getInstantiationBoundArgs(superFunctionPrototypeT.id))

          monouts.newAbstractFuncs.enqueue(
            (superFunctionPrototype, virtualParamIndex, interfaceFullName, instantiationBoundArgs))

          result
        }
        case ArgLookupTE(paramIndex, reference) => ArgLookupTE(paramIndex, translateCoord(reference))
        case SoftLoadTE(originalInner, originalTargetOwnership) => {
          val inner = translateAddrExpr(originalInner)
          val targetOwnership =
            (originalTargetOwnership, inner.result.coord.ownership) match {
              case (a, b) if a == b => a
              case (BorrowT, ShareT) => ShareT
              case (BorrowT, WeakT) => WeakT
              case (BorrowT, OwnT) => BorrowT
              case (WeakT, ShareT) => ShareT
              case (WeakT, OwnT) => WeakT
              case (WeakT, BorrowT) => WeakT
              case other => vwat(other)
            }
          SoftLoadTE(inner, vimpl(targetOwnership))
        }
        case ExternFunctionCallTE(prototype2, args) => {
          ExternFunctionCallTE(
            translatePrototype(prototype2),
            args.map(translateRefExpr))
        }
        case ConstructTE(structTT, resultReference, args) => {
          val coord = translateCoord(resultReference)

          //          val freePrototype = translatePrototype(freePrototypeT)
          //          // They might disagree on the ownership, and thats fine.
          //          // That free prototype is only going to take an owning or a share reference, and we'll only
          //          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototype)
          //          }

          ConstructTE(
            translateStruct(
              structTT,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(structTT.id))),
            coord,
            args.map(translateExpr))
        }
        case DestroyTE(expr, structTT, destinationReferenceVariables) => {
          DestroyTE(
            translateRefExpr(expr),
            translateStruct(
              structTT,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(structTT.id))),
            destinationReferenceVariables.map(translateReferenceLocalVariable))
        }
        case MutateTE(destinationExpr, sourceExpr) => {
          MutateTE(
            translateAddrExpr(destinationExpr),
            translateRefExpr(sourceExpr))
        }
        case u @ UpcastTE(innerExprUnsubstituted, targetSuperKind, untranslatedImplFullName) => {
          val implFullName =
            translateImplFullName(
              untranslatedImplFullName,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(untranslatedImplFullName)))
          //          val freePrototype = translatePrototype(freePrototypeT)
          val coord = translateCoord(u.result.coord)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          UpcastTE(
            translateRefExpr(innerExprUnsubstituted),
            translateSuperKind(targetSuperKind),
            implFullName)//,
          //            freePrototype)
        }
        case IfTE(condition, thenCall, elseCall) => {
          IfTE(
            translateRefExpr(condition),
            translateRefExpr(thenCall),
            translateRefExpr(elseCall))
        }
        case IsSameInstanceTE(left, right) => {
          IsSameInstanceTE(
            translateRefExpr(left),
            translateRefExpr(right))
        }
        case StaticArrayFromValuesTE(elements, resultReference, arrayType) => {

          //          val freePrototype = translatePrototype(freePrototypeT)
          val coord = translateCoord(resultReference)
          // They might disagree on the ownership, and thats fine.
          // That free prototype is only going to take an owning or a share reference, and we'll only
          // use it if we have a shared reference so it's all good.
          //          vassert(coord.kind == vassertSome(freePrototype.fullName.last.parameters.headOption).kind)
          //          if (coord.ownership == ShareT) {
          //            monouts.immKindToDestructor.put(coord.kind, freePrototypeT)
          //          }

          StaticArrayFromValuesTE(
            elements.map(translateRefExpr),
            translateCoord(resultReference),
            arrayType)
        }
        case DeferTE(innerExpr, deferredExpr) => {
          DeferTE(
            translateRefExpr(innerExpr),
            translateRefExpr(deferredExpr))
        }
        case LetAndLendTE(variable, sourceExprT, targetOwnership) => {
          val sourceExpr = translateRefExpr(sourceExprT)

          val resultOwnership =
            (targetOwnership, sourceExpr.result.coord.ownership) match {
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

          LetAndLendTE(
            translateLocalVariable(variable),
            sourceExpr,
            resultOwnership)
        }
        case BorrowToWeakTE(innerExpr) => {
          BorrowToWeakTE(translateRefExpr(innerExpr))
        }
        case WhileTE(BlockTE(inner)) => {
          WhileTE(BlockTE(translateRefExpr(inner)))
        }
        case BreakTE(region) => BreakTE(ITemplata.expectRegion(translateTemplata(region)))
        case LockWeakTE(innerExpr, resultOptBorrowType, someConstructor, noneConstructor, someImplUntranslatedFullName, noneImplUntranslatedFullName) => {
          LockWeakTE(
            translateRefExpr(innerExpr),
            translateCoord(resultOptBorrowType),
            translatePrototype(someConstructor),
            translatePrototype(noneConstructor),
            translateImplFullName(
              someImplUntranslatedFullName,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(someImplUntranslatedFullName))),
            translateImplFullName(
              noneImplUntranslatedFullName,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(noneImplUntranslatedFullName))))
        }
        case DestroyStaticSizedArrayIntoFunctionTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          DestroyStaticSizedArrayIntoFunctionTE(
            translateRefExpr(arrayExpr),
            translateStaticSizedArray(arrayType),
            translateRefExpr(consumer),
            translatePrototype(consumerMethod))
        }
        case NewImmRuntimeSizedArrayTE(arrayType, region, sizeExpr, generator, generatorMethod) => {
          //          val freePrototype = translatePrototype(freePrototypeT)

          val result =
            NewImmRuntimeSizedArrayTE(
              translateRuntimeSizedArray(arrayType),
              ITemplata.expectRegion(translateTemplata(region)),
              translateRefExpr(sizeExpr),
              translateRefExpr(generator),
              translatePrototype(generatorMethod))

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
            StaticArrayFromCallableTE(
              translateStaticSizedArray(arrayType),
              ITemplata.expectRegion(translateTemplata(region)),
              translateRefExpr(generator),
              translatePrototype(generatorMethod))

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
          RuntimeSizedArrayCapacityTE(translateRefExpr(arrayExpr))
        }
        case PushRuntimeSizedArrayTE(arrayExpr, newElementExpr) => {
          PushRuntimeSizedArrayTE(
            translateRefExpr(arrayExpr),
            translateRefExpr(newElementExpr))
        }
        case PopRuntimeSizedArrayTE(arrayExpr) => {
          PopRuntimeSizedArrayTE(translateRefExpr(arrayExpr))
        }
        case ArrayLengthTE(arrayExpr) => {
          ArrayLengthTE(translateRefExpr(arrayExpr))
        }
        case DestroyImmRuntimeSizedArrayTE(arrayExpr, arrayType, consumer, consumerMethod) => {
          DestroyImmRuntimeSizedArrayTE(
            translateRefExpr(arrayExpr),
            translateRuntimeSizedArray(arrayType),
            translateRefExpr(consumer),
            translatePrototype(consumerMethod))
          //            translatePrototype(freePrototype))
        }
        case DestroyMutRuntimeSizedArrayTE(arrayExpr) => {
          DestroyMutRuntimeSizedArrayTE(translateRefExpr(arrayExpr))
        }
        case NewMutRuntimeSizedArrayTE(arrayType, region, capacityExpr) => {
          NewMutRuntimeSizedArrayTE(
            translateRuntimeSizedArray(arrayType),
            ITemplata.expectRegion(translateTemplata(region)),
            translateRefExpr(capacityExpr))
        }
        case TupleTE(elements, resultReference) => {
          TupleTE(
            elements.map(translateRefExpr),
            translateCoord(resultReference))
        }
        case AsSubtypeTE(sourceExpr, targetSubtype, resultResultType, okConstructor, errConstructor, implFullNameT, okResultImplFullNameT, errResultImplFullNameT) => {
          AsSubtypeTE(
            translateRefExpr(sourceExpr),
            translateCoord(targetSubtype),
            translateCoord(resultResultType),
            translatePrototype(okConstructor),
            translatePrototype(errConstructor),
            translateImplFullName(
              implFullNameT,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(implFullNameT))),
            translateImplFullName(
              okResultImplFullNameT,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(okResultImplFullNameT))),
            translateImplFullName(
              errResultImplFullNameT,
              translateBoundArgsForCallee(
                hinputs.getInstantiationBoundArgs(errResultImplFullNameT))))
        }
        case other => vimpl(other)
      }
    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(resultRefExpr, { case PlaceholderNameT(_) => }).isEmpty)
    //    }
    resultRefExpr
  }

  def translateVarFullName(
    id: IdT[IVarNameT]):
  IdT[IVarNameT] = {
    val IdT(module, steps, last) = id
    val result =
      IdT(
        module,
        steps.map(translateName),
        translateVarName(last))
    result
  }

  def translateFunctionFullName(
    fullNameT: IdT[IFunctionNameT]):
  IdT[IFunctionNameT] = {
    val IdT(module, steps, last) = fullNameT
    val fullName =
      IdT(
        module,
        steps.map(translateName),
        translateFunctionName(last))
    //    if (opts.sanityCheck) {
    //      vassert(Collector.all(fullName, { case PlaceholderNameT(_) => }).isEmpty)
    //    }
    fullName
  }

  def translateStructFullName(
    fullNameT: IdT[IStructNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  IdT[IStructNameT] = {
    val IdT(module, steps, lastT) = fullNameT

    val fullName =
      IdT(
        module,
        steps.map(translateName),
        translateStructName(lastT))


    Instantiator.translateStructDefinition(
      opts, interner, keywords, hinputs, monouts, fullName, instantiationBoundArgs)

    return fullName
  }

  def translateInterfaceFullName(
    fullNameT: IdT[IInterfaceNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  IdT[IInterfaceNameT] = {
    val IdT(module, steps, last) = fullNameT
    val newFullName =
      IdT(
        module,
        steps.map(translateName),
        translateInterfaceName(last))


    Instantiator.translateInterfaceDefinition(
      opts, interner, keywords, hinputs, monouts, newFullName, instantiationBoundArgs)

    newFullName
  }

  def translateCitizenName(t: ICitizenNameT): ICitizenNameT = {
    t match {
      case s : IStructNameT => translateStructName(s)
      case i : IInterfaceNameT => translateInterfaceName(i)
    }
  }

  def translateCitizenFullName(
    id: IdT[ICitizenNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  IdT[ICitizenNameT] = {
    id match {
      case IdT(module, steps, last : IStructNameT) => {
        translateStructFullName(IdT(module, steps, last), instantiationBoundArgs)
      }
      case IdT(module, steps, last : IInterfaceNameT) => {
        translateInterfaceFullName(IdT(module, steps, last), instantiationBoundArgs)
      }
      case other => vimpl(other)
    }
  }

  def translateImplFullName(
    fullNameT: IdT[IImplNameT],
    instantiationBoundArgs: InstantiationBoundArguments):
  IdT[IImplNameT] = {
    val IdT(module, steps, last) = fullNameT
    val fullName =
      IdT(
        module,
        steps.map(translateName),
        translateImplName(last, instantiationBoundArgs))


    fullNameT match {
      case IdT(packageCoord, initSteps, name@ImplBoundNameT(_, _)) => {
        val implBoundName = IdT(packageCoord, initSteps, name)
        val result = vassertSome(denizenBoundToDenizenCallerSuppliedThing.implBoundToCallerSuppliedBoundArgImpl.get(implBoundName))
        //        if (opts.sanityCheck) {
        //          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        //        }
        result
      }
      case IdT(_, _, _) => {
        monouts.newImpls.enqueue((fullName, instantiationBoundArgs))
        fullName
      }
    }
  }

  def translateFullName(
    id: IdT[INameT]):
  IdT[INameT] = {
    vimpl()
  }

  def translateCoord(
    coord: CoordT):
  CoordT = {
    val CoordT(outerOwnership, outerRegion, kind) = coord
    kind match {
      case KindPlaceholderT(placeholderFullName) => {
        // Let's get the index'th placeholder from the top level denizen.
        // If we're compiling a function or a struct, it might actually be a lambda function or lambda struct.
        // In these cases, the topLevelDenizenPlaceholderIndexToTemplata actually came from the containing function,
        // see LHPCTLD.

        vassertSome(vassertSome(substitutions.get(placeholderFullName.initFullName(interner))).get(placeholderFullName)) match {
          case CoordTemplata(CoordT(innerOwnership, innerRegion, kind)) => {
            val combinedOwnership =
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
            vassert(innerRegion == translateTemplata(outerRegion))
            CoordT(vimpl(combinedOwnership), innerRegion, kind)
          }
          case KindTemplata(kind) => {
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
        val kind = translateKind(other)
        val mutability = getMutability(kind)
        val newOwnership =
          ((outerOwnership, mutability) match {
            case (_, ImmutableT) => ShareT
            case (other, MutableT) => other
          }) match {
            case BorrowT => {
              vimpl()
            }
            case other => other
          }
        val newRegion = expectRegionTemplata(translateTemplata(outerRegion))
        CoordT(newOwnership, newRegion, translateKind(other))
      }
    }
  }

  def getMutability(t: KindT): MutabilityT = {
    t match {
      case IntT(_) | BoolT() | StrT() | NeverT(_) | FloatT() | VoidT() => ImmutableT
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

  def translateCitizen(citizen: ICitizenTT, instantiationBoundArgs: InstantiationBoundArguments): ICitizenTT = {
    citizen match {
      case s @ StructTT(_) => translateStruct(s, instantiationBoundArgs)
      case s @ InterfaceTT(_) => translateInterface(s, instantiationBoundArgs)
    }
  }

  def translateStruct(struct: StructTT, instantiationBoundArgs: InstantiationBoundArguments): StructTT = {
    val StructTT(fullName) = struct

    val desiredStruct = interner.intern(StructTT(translateStructFullName(fullName, instantiationBoundArgs)))

    desiredStruct
  }

  def translateInterface(interface: InterfaceTT, instantiationBoundArgs: InstantiationBoundArguments): InterfaceTT = {
    val InterfaceTT(fullName) = interface

    val desiredInterface = interner.intern(InterfaceTT(translateInterfaceFullName(fullName, instantiationBoundArgs)))

    desiredInterface
  }

  def translateSuperKind(kind: ISuperKindTT): ISuperKindTT = {
    kind match {
      case i @ InterfaceTT(_) => {
        translateInterface(
          i,
          translateBoundArgsForCallee(
            hinputs.getInstantiationBoundArgs(i.id)))
      }
      case p @ KindPlaceholderT(_) => {
        translatePlaceholder(p) match {
          case s : ISuperKindTT => s
          case other => vwat(other)
        }
      }
    }
  }

  def translatePlaceholder(t: KindPlaceholderT): KindT = {
    val newSubstitutingTemplata =
      vassertSome(
        vassertSome(substitutions.get(t.id.initFullName(interner)))
        .get(t.id))
    ITemplata.expectKindTemplata(newSubstitutingTemplata).kind
  }

  def translateStaticSizedArray(ssaTT: StaticSizedArrayTT): StaticSizedArrayTT = {
    val StaticSizedArrayTT(
    IdT(
    packageCoord,
    initSteps,
    StaticSizedArrayNameT(template, size, variability, RawArrayNameT(mutability, elementType, ssaRegion)))) = ssaTT

    interner.intern(StaticSizedArrayTT(
      IdT(
        packageCoord,
        initSteps,
        interner.intern(StaticSizedArrayNameT(
          template,
          expectIntegerTemplata(translateTemplata(size)),
          expectVariabilityTemplata(translateTemplata(variability)),
          interner.intern(RawArrayNameT(
            expectMutabilityTemplata(translateTemplata(mutability)),
            translateCoord(elementType),
            expectRegionTemplata(translateTemplata(ssaRegion)))))))))
  }

  def translateRuntimeSizedArray(ssaTT: RuntimeSizedArrayTT): RuntimeSizedArrayTT = {
    val RuntimeSizedArrayTT(
    IdT(
    packageCoord,
    initSteps,
    RuntimeSizedArrayNameT(template, RawArrayNameT(mutability, elementType, region)))) = ssaTT

    interner.intern(RuntimeSizedArrayTT(
      IdT(
        packageCoord,
        initSteps,
        interner.intern(RuntimeSizedArrayNameT(
          template,
          interner.intern(RawArrayNameT(
            expectMutabilityTemplata(translateTemplata(mutability)),
            translateCoord(elementType),
            expectRegionTemplata(translateTemplata(region)))))))))
  }

  def translateKind(kind: KindT): KindT = {
    kind match {
      case IntT(bits) => IntT(bits)
      case BoolT() => BoolT()
      case FloatT() => FloatT()
      case VoidT() => VoidT()
      case StrT() => StrT()
      case NeverT(fromBreak) => NeverT(fromBreak)
      case p @ KindPlaceholderT(_) => translatePlaceholder(p)
      case s @ StructTT(_) => {
        translateStruct(
          s, translateBoundArgsForCallee(hinputs.getInstantiationBoundArgs(s.id)))
      }
      case s @ InterfaceTT(_) => {
        translateInterface(
          s, translateBoundArgsForCallee(hinputs.getInstantiationBoundArgs(s.id)))
      }
      case a @ contentsStaticSizedArrayTT(_, _, _, _, _) => translateStaticSizedArray(a)
      case a @ contentsRuntimeSizedArrayTT(_, _, _) => translateRuntimeSizedArray(a)
      case other => vimpl(other)
    }
  }

  def translateParameter(
    param: ParameterT):
  ParameterT = {
    val ParameterT(name, virtuality, tyype) = param
    ParameterT(
      translateVarName(name),
      virtuality,
      translateCoord(tyype))
  }

  def translateTemplata(
    templata: ITemplata[ITemplataType]):
  ITemplata[ITemplataType] = {
    val result =
      templata match {
        case PlaceholderTemplata(n, _) => {
          vassertSome(vassertSome(substitutions.get(n.initFullName(interner))).get(n))
        }
        case IntegerTemplata(value) => IntegerTemplata(value)
        case BooleanTemplata(value) => BooleanTemplata(value)
        case StringTemplata(value) => StringTemplata(value)
        case CoordTemplata(coord) => CoordTemplata(translateCoord(coord))
        case MutabilityTemplata(mutability) => MutabilityTemplata(mutability)
        case VariabilityTemplata(variability) => VariabilityTemplata(variability)
        case KindTemplata(kind) => KindTemplata(translateKind(kind))
        case RegionTemplata(mutable) => RegionTemplata(mutable)
        case other => vimpl(other)
      }
    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case KindPlaceholderNameT(_) => }).isEmpty)
    }
    result
  }

  def translateVarName(
    name: IVarNameT):
  IVarNameT = {
    name match {
      case TypingPassFunctionResultVarNameT() => name
      case CodeVarNameT(_) => name
      case ClosureParamNameT() => name
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
    name: IFunctionNameT):
  IFunctionNameT = {
    name match {
      case FunctionNameT(FunctionTemplateNameT(humanName, codeLoc), templateArgs, params) => {
        interner.intern(FunctionNameT(
          interner.intern(FunctionTemplateNameT(humanName, codeLoc)),
          templateArgs.map(translateTemplata),
          params.map(translateCoord)))
      }
      case ForwarderFunctionNameT(ForwarderFunctionTemplateNameT(innerTemplate, index), inner) => {
        interner.intern(ForwarderFunctionNameT(
          interner.intern(ForwarderFunctionTemplateNameT(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            innerTemplate,
            index)),
          translateFunctionName(inner)))
      }
      case ExternFunctionNameT(humanName, parameters) => {
        interner.intern(ExternFunctionNameT(humanName, parameters.map(translateCoord)))
      }
      case FunctionBoundNameT(FunctionBoundTemplateNameT(humanName, codeLocation), templateArgs, params) => {
        interner.intern(FunctionBoundNameT(
          interner.intern(FunctionBoundTemplateNameT(humanName, codeLocation)),
          templateArgs.map(translateTemplata),
          params.map(translateCoord)))
      }
      case AnonymousSubstructConstructorNameT(template, templateArgs, params) => {
        interner.intern(AnonymousSubstructConstructorNameT(
          translateName(template) match {
            case x @ AnonymousSubstructConstructorTemplateNameT(_) => x
            case other => vwat(other)
          },
          templateArgs.map(translateTemplata),
          params.map(translateCoord)))
      }
      case LambdaCallFunctionNameT(LambdaCallFunctionTemplateNameT(codeLocation, paramTypesForGeneric), templateArgs, paramTypes) => {
        interner.intern(LambdaCallFunctionNameT(
          interner.intern(LambdaCallFunctionTemplateNameT(
            codeLocation,
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            paramTypesForGeneric)),
          templateArgs.map(translateTemplata),
          paramTypes.map(translateCoord)))
      }
      case other => vimpl(other)
    }
  }

  def translateImplName(
    name: IImplNameT,
    instantiationBoundArgs: InstantiationBoundArguments):
  IImplNameT = {
    name match {
      case ImplNameT(ImplTemplateNameT(codeLocationS), templateArgs, subCitizen) => {
        interner.intern(ImplNameT(
          interner.intern(ImplTemplateNameT(codeLocationS)),
          templateArgs.map(translateTemplata),
          translateCitizen(
            subCitizen,
            hinputs.getInstantiationBoundArgs(subCitizen.id))))
      }
      case ImplBoundNameT(ImplBoundTemplateNameT(codeLocationS), templateArgs) => {
        interner.intern(ImplBoundNameT(
          interner.intern(ImplBoundTemplateNameT(codeLocationS)),
          templateArgs.map(translateTemplata)))
      }
      case AnonymousSubstructImplNameT(AnonymousSubstructImplTemplateNameT(interface), templateArgs, subCitizen) => {
        interner.intern(AnonymousSubstructImplNameT(
          interner.intern(AnonymousSubstructImplTemplateNameT(
            // We dont translate these, as these are what uniquely identify generics, and we need that
            // information later to map this back to its originating generic.
            // See DMPOGN for a more detailed explanation. This oddity is really tricky.
            interface)),
          templateArgs.map(translateTemplata),
          translateCitizen(
            subCitizen,
            hinputs.getInstantiationBoundArgs(subCitizen.id))))
      }
    }
  }

  def translateStructName(
    name: IStructNameT):
  IStructNameT = {
    name match {
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        interner.intern(StructNameT(
          interner.intern(StructTemplateNameT(humanName)),
          templateArgs.map(translateTemplata)))
      }
      case AnonymousSubstructNameT(AnonymousSubstructTemplateNameT(interface), templateArgs) => {
        interner.intern(AnonymousSubstructNameT(
          interner.intern(AnonymousSubstructTemplateNameT(
            translateInterfaceTemplateName(interface))),
          templateArgs.map(translateTemplata)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => name
      case other => vimpl(other)
    }
  }

  def translateInterfaceName(
    name: IInterfaceNameT):
  IInterfaceNameT = {
    name match {
      case InterfaceNameT(InterfaceTemplateNameT(humanName), templateArgs) => {
        interner.intern(InterfaceNameT(
          interner.intern(InterfaceTemplateNameT(humanName)),
          templateArgs.map(translateTemplata)))
      }
      case other => vimpl(other)
    }
  }

  def translateInterfaceTemplateName(
    name: IInterfaceTemplateNameT):
  IInterfaceTemplateNameT = {
    name match {
      case InterfaceTemplateNameT(humanName) => name
      case other => vimpl(other)
    }
  }

  def translateName(
    name: INameT):
  INameT = {
    name match {
      case v : IVarNameT => translateVarName(v)
      case KindPlaceholderTemplateNameT(index, _) => vwat()
      case KindPlaceholderNameT(inner) => vwat()
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        interner.intern(StructNameT(
          interner.intern(StructTemplateNameT(humanName)),
          templateArgs.map(translateTemplata)))
      }
      case ForwarderFunctionTemplateNameT(inner, index) => {
        interner.intern(ForwarderFunctionTemplateNameT(
          // We dont translate these, as these are what uniquely identify generics, and we need that
          // information later to map this back to its originating generic.
          // See DMPOGN for a more detailed explanation. This oddity is really tricky.
          inner,
          index))
      }
      case AnonymousSubstructConstructorTemplateNameT(substructTemplateName) => {
        interner.intern(AnonymousSubstructConstructorTemplateNameT(
          translateName(substructTemplateName) match {
            case x : ICitizenTemplateNameT => x
            case other => vwat(other)
          }))
      }
      case FunctionTemplateNameT(humanName, codeLoc) => name
      case StructTemplateNameT(humanName) => name
      case LambdaCitizenTemplateNameT(codeLoc) => name
      case AnonymousSubstructTemplateNameT(interface) => {
        interner.intern(AnonymousSubstructTemplateNameT(
          translateInterfaceTemplateName(interface)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => name
      case InterfaceTemplateNameT(humanNamee) => name
      //      case FreeTemplateNameT(codeLoc) => name
      case f : IFunctionNameT => translateFunctionName(f)
      case other => vimpl(other)
    }
  }

  def translateImplDefinition(
    implId: IdT[IImplNameT],
    implDefinition: EdgeT):
  Unit = {
    if (monouts.impls.contains(implId)) {
      return
    }

    val citizen =
      translateCitizen(
        implDefinition.subCitizen,
        translateBoundArgsForCallee(hinputs.getInstantiationBoundArgs(implDefinition.subCitizen.id)))
    val superInterface =
      translateInterfaceFullName(
        implDefinition.superInterface,
        translateBoundArgsForCallee(hinputs.getInstantiationBoundArgs(implDefinition.superInterface)))
    monouts.impls.put(implId, (citizen, superInterface, denizenBoundToDenizenCallerSuppliedThing, this))

    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverride.get(superInterface))
      .put(implId, mutable.HashMap())
    vassertSome(monouts.interfaceToImpls.get(superInterface)).add(implId)


    vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(superInterface))
      .foreach({ case (abstractFuncPrototype, virtualIndex) =>
        Instantiator.translateOverride(
          opts, interner, keywords, hinputs, monouts, implId, abstractFuncPrototype)
      })
  }
}
