package dev.vale.monomorphizing

import dev.vale.options.GlobalOptions
import dev.vale.{Accumulator, Collector, Interner, vassert, vassertOne, vassertSome, vfail, vimpl, vwat}
import dev.vale.postparsing.{IRuneS, ITemplataType, IntegerTemplataType}
import dev.vale.typing.{Hinputs, TemplataCompiler}
import dev.vale.typing.ast.{EdgeT, _}
import dev.vale.typing.env._
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplata.{expectIntegerTemplata, expectKind, expectMutabilityTemplata, expectVariabilityTemplata}
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.Map
import scala.collection.mutable

class MonomorphizedOutputs() {
  val functions: mutable.HashMap[FullNameT[IFunctionNameT], FunctionT] =
    mutable.HashMap[FullNameT[IFunctionNameT], FunctionT]()
  val structs: mutable.HashMap[FullNameT[IStructNameT], StructDefinitionT] =
    mutable.HashMap[FullNameT[IStructNameT], StructDefinitionT]()
  val interfacesWithoutMethods: mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceDefinitionT] =
    mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceDefinitionT]()

  // We can get some recursion if we have a self-referential struct like:
  //   struct Node<T> { value T; next Opt<Node<T>>; }
  // So we need these to short-circuit that nonsense.
  val startedStructs: mutable.HashSet[FullNameT[IStructNameT]] = mutable.HashSet()
  val startedInterfaces: mutable.HashSet[FullNameT[IInterfaceNameT]] = mutable.HashSet()

  val immKindToDestructor: mutable.HashMap[KindT, PrototypeT] =
    mutable.HashMap[KindT, PrototypeT]()

  // We already know from the hinputs that Some<T> implements Opt<T>.
  // In this map, we'll know that Some<int> implements Opt<int>, Some<bool> implements Opt<bool>, etc.
  val interfaceToImpls: mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashSet[FullNameT[IImplNameT]]] =
    mutable.HashMap()
  val interfaceToAbstractFuncToVirtualIndex: mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[PrototypeT, Int]] =
    mutable.HashMap()
  val impls:
    mutable.HashMap[
      FullNameT[IImplNameT],
      (FullNameT[ICitizenNameT], FullNameT[IInterfaceNameT], Map[FullNameT[FunctionBoundNameT], PrototypeT])] =
    mutable.HashMap()
  // We already know from the hinputs that Opt<T has drop> has func drop(T).
  // In this map, we'll know that Opt<int> has func drop(int).
  val abstractFuncToMonomorphizer: mutable.HashMap[FullNameT[IFunctionNameT], DenizenMonomorphizer] =
    mutable.HashMap()
  // This map collects all overrides for every impl. We'll use it to assemble vtables soon.
  val interfaceToImplToAbstractPrototypeToOverridePrototype:
    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[FullNameT[IImplNameT], mutable.HashMap[PrototypeT, PrototypeT]]] =
    mutable.HashMap()

  // These are new impls and abstract funcs we discover for interfaces.
  // As we discover a new impl or a new abstract func, we'll later need to stamp a lot more overrides either way.
  val newImpls: mutable.Queue[(FullNameT[IImplNameT], Map[IRuneS, PrototypeT])] = mutable.Queue()
  // The int is a virtual index
  val newAbstractFuncs: mutable.Queue[(PrototypeT, Int, FullNameT[IInterfaceNameT], Map[IRuneS, PrototypeT])] = mutable.Queue()
  val newFunctions: mutable.Queue[(FullNameT[IFunctionNameT], Map[IRuneS, PrototypeT])] = mutable.Queue()

  def addMethodToVTable(
    implFullName: FullNameT[IImplNameT],
    superInterfaceFullName: FullNameT[IInterfaceNameT],
    abstractFuncPrototype: PrototypeT,
    overridePrototype: PrototypeT
  ) = {
    val map =
      interfaceToImplToAbstractPrototypeToOverridePrototype
        .getOrElseUpdate(superInterfaceFullName, mutable.HashMap())
        .getOrElseUpdate(implFullName, mutable.HashMap())
    vassert(!map.contains(abstractFuncPrototype))
    map.put(abstractFuncPrototype, overridePrototype)
  }
}

object Monomorphizer {
  def translate(opts: GlobalOptions, interner: Interner, hinputs: Hinputs): Hinputs = {
    val Hinputs(
      interfacesT,
      structsT,
      functionsT,
      oldImmKindToDestructorT,
      interfaceToEdgeBlueprintsT,
      interfaceToSubCitizenToEdgeT,
      instantiationNameToFunctionBoundToRuneT,
      kindExportsT,
      functionExportsT,
      kindExternsT,
      functionExternsT) = hinputs

    val monouts = new MonomorphizedOutputs()

    kindExportsT.foreach({ case KindExportT(range, tyype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName =
        packageName.addStep(interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)))))
      val exportTemplateName = TemplataCompiler.getExportTemplate(exportName)
      val monomorphizer =
        new DenizenMonomorphizer(
          opts, interner, hinputs, monouts, exportTemplateName, exportName, Array(), Map())
      KindExportT(
        range,
        monomorphizer.translateKind(tyype),
        packageCoordinate,
        exportedName)
    })

    functionExportsT.foreach({ case FunctionExportT(range, prototype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName =
        packageName.addStep(
          interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)))))
      val exportTemplateName = TemplataCompiler.getExportTemplate(exportName)
      val monomorphizer =
        new DenizenMonomorphizer(
          opts, interner, hinputs, monouts, exportTemplateName, exportName, Array(), Map())
      FunctionExportT(
        range,
        monomorphizer.translatePrototype(prototype),
        packageCoordinate,
        exportedName)
    })

    while ({
      // We make structs and interfaces eagerly as we come across them
      // if (monouts.newStructs.nonEmpty) {
      //   val newStructName = monouts.newStructs.dequeue()
      //   DenizenMonomorphizer.translateStructDefinition(opts, interner, hinputs, monouts, newStructName)
      //   true
      // } else if (monouts.newInterfaces.nonEmpty) {
      //   val (newInterfaceName, calleeRuneToSuppliedPrototype) = monouts.newInterfaces.dequeue()
      //   DenizenMonomorphizer.translateInterfaceDefinition(
      //     opts, interner, hinputs, monouts, newInterfaceName, calleeRuneToSuppliedPrototype)
      //   true
      // } else
      if (monouts.newFunctions.nonEmpty) {
        val (newFuncName, runeToSuppliedFunction) = monouts.newFunctions.dequeue()
        DenizenMonomorphizer.translateFunction(
          opts, interner, hinputs, monouts, newFuncName, runeToSuppliedFunction)
        true
      } else if (monouts.newImpls.nonEmpty) {
        val (implFullName, runeToSuppliedFunctionForUnsubstitutedImpl) = monouts.newImpls.dequeue()
        DenizenMonomorphizer.translateImpl(
          opts, interner, hinputs, monouts, implFullName, runeToSuppliedFunctionForUnsubstitutedImpl)
        true
      } else if (monouts.newAbstractFuncs.nonEmpty) {
        val (abstractFunc, virtualIndex, interfaceFullName, bounds) = monouts.newAbstractFuncs.dequeue()
        DenizenMonomorphizer.translateAbstractFunc(
          opts, interner, hinputs, monouts, interfaceFullName, abstractFunc, virtualIndex, bounds)
        true
      } else {
        false
      }
    }) {}

//    interfaceToEdgeBlueprints.foreach({ case (interfacePlaceholderedFullName, edge) =>
//      val monomorphizer = new DenizenMonomorphizer(interner, monouts, interfacePlaceholderedFullName)
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
        val InterfaceDefinitionT(templateName, instantiatedInterface, ref, attributes, weakable, mutability, _, _) = interface
        InterfaceDefinitionT(
          templateName, instantiatedInterface, ref, attributes, weakable, mutability, Map(),
          vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interface.ref.fullName)).toVector)
      })

    val interfaceToSubCitizenToEdge =
      monouts.interfaceToImpls.map({ case (interface, impls) =>
        interface ->
        impls.map(impl => {
          val (subCitizen, parentInterface, _) = vassertSome(monouts.impls.get(impl))
          vassert(parentInterface == interface)
          val abstractFuncToVirtualIndex =
            vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interface))
          val abstractFuncPrototypeToOverridePrototype =
            abstractFuncToVirtualIndex.map({ case (abstractFuncPrototype, virtualIndex) =>
              val overridePrototype =
                vassertSome(
                  vassertSome(
                    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.get(interface))
                      .get(impl))
                    .get(abstractFuncPrototype))

              vassert(
                abstractFuncPrototype.fullName.last.parameters(virtualIndex).kind !=
                overridePrototype.fullName.last.parameters(virtualIndex).kind)

              abstractFuncPrototype.fullName -> overridePrototype
            })
          val edge = EdgeT(impl, subCitizen, interface, Map(), abstractFuncPrototypeToOverridePrototype.toMap)
          subCitizen -> edge
        }).toMap
      }).toMap

    Hinputs(
      interfaces.toVector,
      monouts.structs.values.toVector,
      monouts.functions.values.toVector,
      monouts.immKindToDestructor.toMap,
      interfaceEdgeBlueprints,
      interfaceToSubCitizenToEdge,
      Map(),
      kindExportsT,
      functionExportsT,
      kindExternsT,
      functionExternsT)
  }
}

object DenizenMonomorphizer {
  def translateInterfaceDefinition(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    interfaceFullName: FullNameT[IInterfaceNameT],
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeT]):
  Unit = {
    val interfaceTemplate = TemplataCompiler.getInterfaceTemplate(interfaceFullName)

    val interfaceDefT =
      vassertOne(hinputs.interfaces.filter(_.templateName == interfaceTemplate))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        interfaceTemplate,
        interfaceFullName,
        interfaceFullName.last.templateArgs.toArray,
        assembleCalleeDenizenBounds(
          interfaceDefT.runeToFunctionBound,
          calleeRuneToSuppliedPrototype))
    monomorphizer.translateInterfaceDefinition(interfaceFullName, interfaceDefT)
  }

  def assembleCalleeDenizenBounds(
    runeToFunctionBoundT: Map[IRuneS, FullNameT[FunctionBoundNameT]],
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeT]
  ): Map[FullNameT[FunctionBoundNameT], PrototypeT] = {
    calleeRuneToSuppliedPrototype.map({ case (calleeRune, suppliedFunctionT) =>
      vassertSome(runeToFunctionBoundT.get(calleeRune)) -> suppliedFunctionT
    })
  }

  def translateStructDefinition(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    structFullName: FullNameT[IStructNameT],
    runeToSuppliedFunction: Map[IRuneS, PrototypeT]):
  Unit = {
    if (opts.sanityCheck) {
      vassert(Collector.all(structFullName, { case PlaceholderNameT(_) => }).isEmpty)
    }

    val structTemplate = TemplataCompiler.getStructTemplate(structFullName)

    val structDefT =
      vassertOne(hinputs.structs.filter(_.templateName == structTemplate))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        structTemplate,
        structFullName,
        structFullName.last.templateArgs.toArray,
        assembleCalleeDenizenBounds(
          structDefT.runeToFunctionBound, runeToSuppliedFunction))

    monomorphizer.translateStructDefinition(structFullName, structDefT)
  }

  def translateAbstractFunc(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    interfaceFullName: FullNameT[IInterfaceNameT],
    abstractFunc: PrototypeT,
    virtualIndex: Int,
    abstractFunctionRuneToSuppliedFunction: Map[IRuneS, PrototypeT]):
  Unit = {
    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(abstractFunc.fullName)

    val funcT =
      vassertOne(
        hinputs.functions.filter(func => {
          TemplataCompiler.getFunctionTemplate(func.header.fullName) == funcTemplateNameT
        }))

    val abstractFuncMonomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        funcTemplateNameT,
        abstractFunc.fullName,
        abstractFunc.fullName.last.templateArgs.toArray,
        assembleCalleeDenizenBounds(
          funcT.runeToFuncBound, abstractFunctionRuneToSuppliedFunction))

    vassert(!monouts.abstractFuncToMonomorphizer.contains(abstractFunc.fullName))
    monouts.abstractFuncToMonomorphizer.put(abstractFunc.fullName, abstractFuncMonomorphizer)

    val abstractFuncs = vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(interfaceFullName))
    vassert(!abstractFuncs.contains(abstractFunc))
    abstractFuncs.put(abstractFunc, virtualIndex)

    vassertSome(monouts.interfaceToImpls.get(interfaceFullName)).foreach(impl => {
      translateOverride(opts, interner, hinputs, monouts, impl, abstractFunc)
    })
  }

  def translateOverride(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    implFullName: FullNameT[IImplNameT],
    abstractFuncPrototype: PrototypeT):
  Unit = {
//    val superInterfaceFullName: FullNameT[IInterfaceNameT],

    val implDefinitionT =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => TemplataCompiler.getImplTemplate(edge.edgeFullName) == TemplataCompiler.getImplTemplate(implFullName)))

    val superInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(implDefinitionT.interface)
    val superInterfaceDefinitionT = hinputs.lookupInterfaceByTemplateFullName(superInterfaceTemplateFullName)
    val superInterfacePlaceholderedName = superInterfaceDefinitionT.instantiatedInterface

    val abstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(abstractFuncPrototype.fullName)
    val abstractFuncPlaceholderedNameT =
      vassertSome(
        hinputs.functions
          .find(func => TemplataCompiler.getFunctionTemplate(func.header.fullName) == abstractFuncTemplateName))
        .header.fullName

    val edgeT =
      vassertSome(
        vassertSome(hinputs.interfaceToSubCitizenToEdge.get(superInterfacePlaceholderedName.fullName))
          .get(implDefinitionT.struct))

    val overridePrototypeT =
      vassertSome(edgeT.abstractFuncToOverrideFunc.get(abstractFuncPlaceholderedNameT))

    val abstractFunctionMonomorphizer =
      vassertSome(monouts.abstractFuncToMonomorphizer.get(abstractFuncPrototype.fullName))

    val overridePrototype = abstractFunctionMonomorphizer.translatePrototype(overridePrototypeT)

    val superInterfaceFullName = vassertSome(monouts.impls.get(implFullName))._2

    monouts.addMethodToVTable(implFullName, superInterfaceFullName, abstractFuncPrototype, overridePrototype)
  }

  def translateImpl(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    implFullName: FullNameT[IImplNameT],
    runeToSuppliedFunctionForUnsubstitutedImpl: Map[IRuneS, PrototypeT]):
  Unit = {
    val implTemplateFullName = TemplataCompiler.getImplTemplate(implFullName)
    val implDefinition =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => TemplataCompiler.getImplTemplate(edge.edgeFullName) == implTemplateFullName))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        implTemplateFullName,
        implFullName,
        implFullName.last.templateArgs.toArray,
        assembleCalleeDenizenBounds(
          implDefinition.runeToFuncBound, runeToSuppliedFunctionForUnsubstitutedImpl))
    val (subCitizen, superInterface) =
      monomorphizer.translateImplDefinition(implFullName, implDefinition)


    vassertSome(monouts.interfaceToAbstractFuncToVirtualIndex.get(superInterface))
      .foreach({ case (abstractFuncPrototype, virtualIndex) =>
        translateOverride(opts, interner, hinputs, monouts, implFullName, abstractFuncPrototype)
      })


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
    //        DenizenMonomorphizer.translateFunction(
    //          opts, interner, hinputs, monouts, overridePrototype.fullName,
    //          translateBoundsForCallee(
    //            hinputs.getInstantiationBounds(overridePrototype.fullName)))
    //
    //      monouts.addMethodToVTable(implFullName, superInterfaceFullName, abstractFunc, funcT)
    //    })

  }

  def translateFunction(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    desiredFuncFullName: FullNameT[IFunctionNameT],
    runeToSuppliedPrototype: Map[IRuneS, PrototypeT]):
  FunctionT = {
    val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(desiredFuncFullName)
    val funcT =
      vassertOne(
        hinputs.functions.filter(func => {
          TemplataCompiler.getFunctionTemplate(func.header.fullName) == funcTemplateNameT
        }))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        TemplataCompiler.getFunctionTemplate(funcT.header.fullName),
        desiredFuncFullName,
        desiredFuncFullName.last.templateArgs.toArray,
        assembleCalleeDenizenBounds(funcT.runeToFuncBound, runeToSuppliedPrototype))

    val monomorphizedFuncT = monomorphizer.translateFunction(funcT)

    if (opts.sanityCheck) {
      vassert(Collector.all(monomorphizedFuncT, { case PlaceholderNameT(_) => }).isEmpty)
    }

    monomorphizedFuncT
  }
}

class DenizenMonomorphizer(
  opts: GlobalOptions,
  interner: Interner,
  hinputs: Hinputs,
  monouts: MonomorphizedOutputs,
  denizenTemplateName: FullNameT[ITemplateNameT],
  denizenName: FullNameT[IInstantiationNameT],
  placeholderIndexToTemplata: Array[ITemplata[ITemplataType]],
  denizenFunctionBoundToDenizenCallerSuppliedPrototype: Map[FullNameT[FunctionBoundNameT], PrototypeT]) {
//  selfFunctionBoundToRuneUnsubstituted: Map[PrototypeT, IRuneS],
//  denizenRuneToDenizenCallerPrototype: Map[IRuneS, PrototypeT]) {

  // This is just here to get scala to include these fields so i can see them in the debugger
  vassert(TemplataCompiler.getTemplate(denizenName) == denizenTemplateName)

  if (opts.sanityCheck) {
    denizenFunctionBoundToDenizenCallerSuppliedPrototype.foreach({
      case (denizenFunctionBound, denizenCallerSuppliedPrototype) => {
        vassert(Collector.all(denizenCallerSuppliedPrototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
      }
    })
  }

  def translateStructMember(member: IStructMemberT): IStructMemberT = {
    member match {
      case NormalStructMemberT(name, variability, tyype) => {
        NormalStructMemberT(
          translateVarName(name),
          variability,
          tyype match {
            case ReferenceMemberTypeT(UnsubstitutedCoordT(unsubstitutedCoord)) => {
              ReferenceMemberTypeT(UnsubstitutedCoordT(translateCoord(unsubstitutedCoord)))
            }
            case AddressMemberTypeT(UnsubstitutedCoordT(unsubstitutedCoord)) => {
              AddressMemberTypeT(UnsubstitutedCoordT(translateCoord(unsubstitutedCoord)))
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

    val runeToSuppliedPrototypeForCall =
      translateBoundsForCallee(
        hinputs.getInstantiationBounds(desiredPrototypeUnsubstituted.fullName))

    val desiredPrototype =
      PrototypeT(
        translateFullFunctionName(desiredPrototypeFullNameUnsubstituted),
        translateCoord(desiredPrototypeReturnTypeUnsubstituted))

    desiredPrototypeUnsubstituted.fullName match {
      case FullNameT(packageCoord, initSteps, name @ FunctionBoundNameT(_, _, _)) => {
        val funcBoundName = FullNameT(packageCoord, initSteps, name)
        val result = vassertSome(denizenFunctionBoundToDenizenCallerSuppliedPrototype.get(funcBoundName))
        if (opts.sanityCheck) {
          vassert(Collector.all(result, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        }
        result
      }
      case FullNameT(_, _, ExternFunctionNameT(_, _)) => {
        if (opts.sanityCheck) {
          vassert(Collector.all(desiredPrototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        }

        desiredPrototype
      }
      case _ => {
        monouts.newFunctions.enqueue((desiredPrototype.fullName, runeToSuppliedPrototypeForCall))
        desiredPrototype
      }
    }
  }

  private def translateBoundsForCallee(
    // This is a map from rune to a prototype, specifically the prototype that we
    // (the *template* caller) is supplying to the *template* callee. This prototype might
    // be a placeholder, phrased in terms of our (the *template* caller's) placeholders
    runeToSuppliedPrototypeForCallUnsubstituted: Map[IRuneS, PrototypeT]):
  Map[IRuneS, PrototypeT] = {
    // For any that are placeholders themselves, let's translate those into actual prototypes.
    runeToSuppliedPrototypeForCallUnsubstituted.map({ case (rune, suppliedPrototypeUnsubstituted) =>
      rune ->
        (suppliedPrototypeUnsubstituted.fullName match {
          case FullNameT(packageCoord, initSteps, name @ FunctionBoundNameT(_, _, _)) => {
            vassertSome(
              denizenFunctionBoundToDenizenCallerSuppliedPrototype.get(
                FullNameT(packageCoord, initSteps, name)))
          }
          case _ => {
            translatePrototype(suppliedPrototypeUnsubstituted)
          }
        })
    })
    // And now we have a map from the callee's rune to the *instantiated* callee's prototypes.
  }

  def translateStructDefinition(
    newFullName: FullNameT[IStructNameT],
    structDefT: StructDefinitionT):
  Unit = {
    val StructDefinitionT(templateName, instantiatedCitizen, attributes, weakable, mutability, members, isClosure, _) = structDefT

    if (opts.sanityCheck) {
      vassert(Collector.all(newFullName, { case PlaceholderNameT(_) => }).isEmpty)
    }

    if (monouts.startedStructs.contains(newFullName)) {
      return
    }
    monouts.startedStructs.add(newFullName)

    val result =
      StructDefinitionT(
        templateName,
        interner.intern(StructTT(newFullName)),
        attributes,
        weakable,
        mutability,
        members.map(translateStructMember),
        isClosure,
        Map())

    vassert(result.instantiatedCitizen.fullName == newFullName)

    monouts.structs.put(result.instantiatedCitizen.fullName, result)

    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case PlaceholderNameT(_) => }).isEmpty)
    }
    result
  }

  def translateInterfaceDefinition(
    newFullName: FullNameT[IInterfaceNameT],
    interfaceDefT: InterfaceDefinitionT):
  Unit = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutability, _, internalMethods) = interfaceDefT

    if (monouts.startedInterfaces.contains(newFullName)) {
      return
    }
    monouts.startedInterfaces.add(newFullName)

    val newInterfaceTT = interner.intern(InterfaceTT(newFullName))

    val result =
      InterfaceDefinitionT(
        templateName,
        newInterfaceTT,
        newInterfaceTT,
        attributes,
        weakable,
        mutability,
        Map(),
        Vector())

    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case PlaceholderNameT(_) => }).isEmpty)
    }

    vassert(!monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.contains(newFullName))
    monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.put(newFullName, mutable.HashMap())

    monouts.interfacesWithoutMethods.put(newFullName, result)

    vassert(!monouts.interfaceToAbstractFuncToVirtualIndex.contains(newFullName))
    monouts.interfaceToAbstractFuncToVirtualIndex.put(newFullName, mutable.HashMap())

    vassert(!monouts.interfaceToImpls.contains(newFullName))
    monouts.interfaceToImpls.put(newFullName, mutable.HashSet())

    vassert(result.instantiatedCitizen.fullName == newFullName)
  }

  def translateFunctionHeader(header: FunctionHeaderT): FunctionHeaderT = {
    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = header

    val newFullName = translateFullFunctionName(fullName)

    FunctionHeaderT(
      newFullName,
      attributes,
      params.map(translateParameter),
      translateCoord(returnType),
      maybeOriginFunctionTemplata)
  }

  def translateFunction(
    functionT: FunctionT):
  FunctionT = {
    val FunctionT(headerT, _, bodyT) = functionT

    val FunctionHeaderT(fullName, attributes, params, returnType, maybeOriginFunctionTemplata) = headerT

    val newFullName = translateFullFunctionName(fullName)

    monouts.functions.get(newFullName) match {
      case Some(func) => return func
      case None =>
    }

    val newHeader = translateFunctionHeader(headerT)

    val result = FunctionT(newHeader, Map(), translateRefExpr(bodyT))
    monouts.functions.put(result.header.fullName, result)
    result
  }

  def translateLocalVariable(
    variable: ILocalVariableT):
  ILocalVariableT = {
    variable match {
      case r @ ReferenceLocalVariableT(_, _, _) => translateReferenceLocalVariable(r)
      case AddressibleLocalVariableT(id, variability, reference) => {
        AddressibleLocalVariableT(
          translateFullVarName(id),
          variability,
          translateCoord(reference))
      }
    }
  }

  def translateReferenceLocalVariable(
    variable: ReferenceLocalVariableT):
  ReferenceLocalVariableT = {
    val ReferenceLocalVariableT(id, variability, reference) = variable
    ReferenceLocalVariableT(
      translateFullVarName(id),
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
      case ReferenceMemberLookupTE(range, structExpr, memberName, memberReference, variability) => {
        ReferenceMemberLookupTE(
          range,
          translateRefExpr(structExpr),
          translateFullVarName(memberName),
          translateCoord(memberReference),
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
          translateFullVarName(memberName),
          translateCoord(resultType2),
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
    expr match {
      case LetNormalTE(variable, inner) => LetNormalTE(translateLocalVariable(variable), translateRefExpr(inner))
      case BlockTE(inner) => BlockTE(translateRefExpr(inner))
      case ReturnTE(inner) => ReturnTE(translateRefExpr(inner))
      case ConsecutorTE(inners) => ConsecutorTE(inners.map(translateRefExpr))
      case ConstantIntTE(value, bits) => {
        ConstantIntTE(ITemplata.expectIntegerTemplata(translateTemplata(value)), bits)
      }
      case ConstantStrTE(value) => ConstantStrTE(value)
      case ConstantBoolTE(value) => ConstantBoolTE(value)
      case ConstantFloatTE(value) => ConstantFloatTE(value)
      case UnletTE(variable) => UnletTE(translateLocalVariable(variable))
      case DiscardTE(expr) => DiscardTE(translateRefExpr(expr))
      case VoidLiteralTE() => VoidLiteralTE()
      case FunctionCallTE(callable, args) => {
        FunctionCallTE(
          translatePrototype(callable),
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
          superFunctionPrototype.paramTypes(virtualParamIndex).kind.expectInterface().fullName
//        val interfaceFullName =
//          translateInterfaceFullName(
//            interfaceFullNameT,
//            translateBoundsForCallee(
//              hinputs.getInstantiationBounds(callee.toPrototype.fullName)))

        val calleeRuneToSuppliedPrototype =
          translateBoundsForCallee(
            // but this is literally calling itself from where its defined
            // perhaps we want the thing that originally called
            hinputs.getInstantiationBounds(superFunctionPrototypeT.fullName))

        monouts.newAbstractFuncs.enqueue(
          (superFunctionPrototype, virtualParamIndex, interfaceFullName, calleeRuneToSuppliedPrototype))

        result
      }
      case ArgLookupTE(paramIndex, reference) => ArgLookupTE(paramIndex, translateCoord(reference))
      case SoftLoadTE(originalInner, originalTargetOwnership) => {
        val inner = translateAddrExpr(originalInner)
        val targetOwnership =
          (originalTargetOwnership, inner.result.reference.ownership) match {
            case (a, b) if a == b => a
            case (BorrowT, ShareT) => ShareT
            case (BorrowT, OwnT) => BorrowT
            case other => vwat(other)
          }
        SoftLoadTE(inner, targetOwnership)
      }
      case ExternFunctionCallTE(prototype2, args) => {
        ExternFunctionCallTE(
          translatePrototype(prototype2),
          args.map(translateRefExpr))
      }
      case ConstructTE(structTT, resultReference, args, freePrototype) => {
        val free = translatePrototype(freePrototype)

        val coord = translateCoord(resultReference)
        vassert(coord == vassertSome(free.fullName.last.parameters.headOption))
        if (coord.ownership == ShareT) {
          monouts.immKindToDestructor.put(coord.kind, freePrototype)
        }
        ConstructTE(
          translateStruct(
            structTT,
            translateBoundsForCallee(
              hinputs.getInstantiationBounds(structTT.fullName))),
          coord,
          args.map(translateExpr),
          free)
      }
      case DestroyTE(expr, structTT, destinationReferenceVariables) => {
        DestroyTE(
          translateRefExpr(expr),
          translateStruct(
            structTT,
            translateBoundsForCallee(
              hinputs.getInstantiationBounds(structTT.fullName))),
          destinationReferenceVariables.map(translateReferenceLocalVariable))
      }
      case MutateTE(destinationExpr, sourceExpr) => {
        MutateTE(
          translateAddrExpr(destinationExpr),
          translateRefExpr(sourceExpr))
      }
      case u @ UpcastTE(innerExprUnsubstituted, targetSuperKind, untranslatedImplFullName, interfaceFreePrototype) => {
        val implFullName =
          translateImplFullName(
            untranslatedImplFullName,
            translateBoundsForCallee(
              hinputs.getInstantiationBounds(untranslatedImplFullName)))
//
//        val runeToFunctionBound =
//          runeToFunctionBoundUnsubstituted.map({ case (rune, PrototypeTemplata(declarationRange, prototype)) =>
//            // We're resolving some function bounds, and function bounds have no function bounds
//            // themselves, so we supply Map() here.
//            (rune -> PrototypeTemplata(declarationRange, translatePrototype(prototype)))
//          })

        val free = translatePrototype(interfaceFreePrototype)
        val coord = translateCoord(u.result.reference)
        vassert(coord == vassertSome(free.fullName.last.parameters.headOption))
        if (coord.ownership == ShareT) {
          monouts.immKindToDestructor.put(coord.kind, interfaceFreePrototype)
        }

        UpcastTE(
          translateRefExpr(innerExprUnsubstituted),
          translateSuperKind(targetSuperKind),
          implFullName,
          free)
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
      case LetAndLendTE(variable, expr, targetOwnership) => {
        LetAndLendTE(
          translateLocalVariable(variable),
          translateRefExpr(expr),
          targetOwnership)
      }
      case BorrowToWeakTE(innerExpr) => {
        BorrowToWeakTE(translateRefExpr(innerExpr))
      }
      case WhileTE(BlockTE(inner)) => {
        WhileTE(BlockTE(translateRefExpr(inner)))
      }
      case BreakTE() => BreakTE()
      case other => vimpl(other)
    }
  }

  def translateFullVarName(
    fullName: FullNameT[IVarNameT]):
  FullNameT[IVarNameT] = {
    val FullNameT(module, steps, last) = fullName
    FullNameT(
      module,
      steps.map(translateName),
      translateVarName(last))
  }

  def translateFullFunctionName(
    fullName: FullNameT[IFunctionNameT]):
  FullNameT[IFunctionNameT] = {
    val FullNameT(module, steps, last) = fullName
    FullNameT(
      module,
      steps.map(translateName),
      translateFunctionName(last))
  }

  def translateStructFullName(
    fullNameT: FullNameT[IStructNameT],
    runeToSuppliedFunction: Map[IRuneS, PrototypeT]):
  FullNameT[IStructNameT] = {
    val FullNameT(module, steps, lastT) = fullNameT

    val fullName =
      FullNameT(
        module,
        steps.map(translateName),
        translateStructName(lastT))

    DenizenMonomorphizer.translateStructDefinition(
      opts, interner, hinputs, monouts, fullName, runeToSuppliedFunction)

    return fullName
  }

  def translateInterfaceFullName(
    fullName: FullNameT[IInterfaceNameT],
    calleeRuneToSuppliedPrototype: Map[IRuneS, PrototypeT]):
  FullNameT[IInterfaceNameT] = {
    val FullNameT(module, steps, last) = fullName
    val newFullName =
      FullNameT(
        module,
        steps.map(translateName),
        translateInterfaceName(last))

    DenizenMonomorphizer.translateInterfaceDefinition(
      opts, interner, hinputs, monouts, newFullName, calleeRuneToSuppliedPrototype)

    newFullName
  }

  def translateCitizenName(t: ICitizenNameT): ICitizenNameT = {
    t match {
      case s : IStructNameT => translateStructName(s)
      case i : IInterfaceNameT => translateInterfaceName(i)
    }
  }

  def translateCitizenFullName(
    fullName: FullNameT[ICitizenNameT],
    runeToSuppliedFunction: Map[IRuneS, PrototypeT]):
  FullNameT[ICitizenNameT] = {
    fullName match {
      case FullNameT(module, steps, last : IStructNameT) => {
        translateStructFullName(FullNameT(module, steps, last), runeToSuppliedFunction)
      }
      case FullNameT(module, steps, last : IInterfaceNameT) => {
        translateInterfaceFullName(FullNameT(module, steps, last), runeToSuppliedFunction)
      }
      case other => vimpl(other)
    }
  }

  def translateImplFullName(
    fullNameT: FullNameT[IImplNameT],
    runeToSuppliedFunctionForUnsubstitutedImpl: Map[IRuneS, PrototypeT]):
  FullNameT[IImplNameT] = {
    val FullNameT(module, steps, last) = fullNameT
    val fullName =
      FullNameT(
        module,
        steps.map(translateName),
        translateImplName(last))

    monouts.newImpls.enqueue((fullName, runeToSuppliedFunctionForUnsubstitutedImpl))

    fullName
  }

  def translateFullName(
    fullName: FullNameT[INameT]):
  FullNameT[INameT] = {
    vimpl()
  }

  def translateCoord(
    coord: CoordT):
  CoordT = {
    val CoordT(ownership, kind) = coord
    kind match {
      case PlaceholderT(FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index)))) => {
        vassert(index >= 0)
        vassert(index < placeholderIndexToTemplata.length)
        placeholderIndexToTemplata(index) match {
          case CoordTemplata(CoordT(innerOwnership, kind)) => {
            val combinedOwnership =
              (ownership, innerOwnership) match {
                case (OwnT, OwnT) => OwnT
                case (OwnT, BorrowT) => BorrowT
                case (BorrowT, OwnT) => BorrowT
                case (BorrowT, BorrowT) => BorrowT
                case (BorrowT, ShareT) => ShareT
                case (ShareT, ShareT) => ShareT
                case (OwnT, ShareT) => ShareT
                case other => vwat(other)
              }
            CoordT(combinedOwnership, kind)
          }
          case KindTemplata(kind) => CoordT(ownership, kind)
        }
      }
      case other => CoordT(ownership, translateKind(other))
    }
  }

  def translateStruct(struct: StructTT, runeToSuppliedFunction: Map[IRuneS, PrototypeT]): StructTT = {
    val StructTT(fullName) = struct

    val desiredStruct = interner.intern(StructTT(translateStructFullName(fullName, runeToSuppliedFunction)))

    desiredStruct
  }

  def translateInterface(interface: InterfaceTT, runeToSuppliedFunction: Map[IRuneS, PrototypeT]): InterfaceTT = {
    val InterfaceTT(fullName) = interface

    val desiredInterface = interner.intern(InterfaceTT(translateInterfaceFullName(fullName, runeToSuppliedFunction)))

    desiredInterface
  }

  def translateSuperKind(kind: ISuperKindTT): ISuperKindTT = {
    kind match {
      case i @ InterfaceTT(_) => {
        translateInterface(
          i,
          translateBoundsForCallee(
            hinputs.getInstantiationBounds(i.fullName)))
      }
      case p @ PlaceholderT(_) => {
        translatePlaceholder(p) match {
          case s : ISuperKindTT => s
          case other => vwat(other)
        }
      }
    }
  }

  def translatePlaceholder(t: PlaceholderT): KindT = {
    val PlaceholderT(FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index)))) = t

    vassert(index >= 0)
    vassert(index < placeholderIndexToTemplata.length)
    ITemplata.expectKindTemplata(placeholderIndexToTemplata(index)).kind
  }

  def translateStaticSizedArray(ssaTT: StaticSizedArrayTT): StaticSizedArrayTT = {
    val StaticSizedArrayTT(size, mutability, variability, elementType) = ssaTT
    interner.intern(StaticSizedArrayTT(
      expectIntegerTemplata(translateTemplata(size)),
      expectMutabilityTemplata(translateTemplata(mutability)),
      expectVariabilityTemplata(translateTemplata(variability)),
      translateCoord(elementType)))
  }

  def translateRuntimeSizedArray(ssaTT: RuntimeSizedArrayTT): RuntimeSizedArrayTT = {
    val RuntimeSizedArrayTT(mutability, elementType) = ssaTT
    interner.intern(RuntimeSizedArrayTT(
      expectMutabilityTemplata(translateTemplata(mutability)),
      translateCoord(elementType)))
  }

  def translateKind(kind: KindT): KindT = {
    kind match {
      case IntT(bits) => IntT(bits)
      case BoolT() => BoolT()
      case FloatT() => FloatT()
      case VoidT() => VoidT()
      case StrT() => StrT()
      case NeverT(fromBreak) => NeverT(fromBreak)
      case p @ PlaceholderT(_) => translatePlaceholder(p)
      case s @ StructTT(_) => {
        translateStruct(
          s, translateBoundsForCallee(hinputs.getInstantiationBounds(s.fullName)))
      }
      case s @ InterfaceTT(_) => {
        translateInterface(
          s, translateBoundsForCallee(hinputs.getInstantiationBounds(s.fullName)))
      }
      case a @ StaticSizedArrayTT(_, _, _, _) => translateStaticSizedArray(a)
      case a @ RuntimeSizedArrayTT(_, _) => translateRuntimeSizedArray(a)
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
        case PlaceholderTemplata(FullNameT(_, _, PlaceholderNameT(PlaceholderTemplateNameT(index))), _) =>  {
          vassert(index >= 0)
          vassert(index < placeholderIndexToTemplata.length)
          placeholderIndexToTemplata(index)
        }
        case IntegerTemplata(value) => IntegerTemplata(value)
        case BooleanTemplata(value) => BooleanTemplata(value)
        case StringTemplata(value) => StringTemplata(value)
        case CoordTemplata(coord) => CoordTemplata(translateCoord(coord))
        case MutabilityTemplata(mutability) => MutabilityTemplata(mutability)
        case VariabilityTemplata(variability) => VariabilityTemplata(variability)
        case KindTemplata(kind) => KindTemplata(translateKind(kind))
        case other => vimpl(other)
      }
    if (opts.sanityCheck) {
      vassert(Collector.all(result, { case PlaceholderNameT(_) => }).isEmpty)
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
      case ExternFunctionNameT(humanName, parameters) => {
        interner.intern(ExternFunctionNameT(humanName, parameters.map(translateCoord)))
      }
      case FreeNameT(FreeTemplateNameT(codeLoc), templateArgs, coord) => {
        interner.intern(FreeNameT(
          interner.intern(FreeTemplateNameT(codeLoc)),
          templateArgs.map(translateTemplata),
          translateCoord(coord)))
      }
      case FunctionBoundNameT(FunctionBoundTemplateNameT(humanName, codeLocation), templateArgs, params) => {
        interner.intern(FunctionBoundNameT(
          interner.intern(FunctionBoundTemplateNameT(humanName, codeLocation)),
          templateArgs.map(translateTemplata),
          params.map(translateCoord)))
      }
      case other => vimpl(other)
    }
  }

  def translateImplName(
    name: IImplNameT):
  IImplNameT = {
    name match {
      case ImplDeclareNameT(ImplTemplateDeclareNameT(codeLocationS), templateArgs) => {
        interner.intern(ImplDeclareNameT(
          interner.intern(ImplTemplateDeclareNameT(codeLocationS)),
          templateArgs.map(translateTemplata)))
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
      case PlaceholderTemplateNameT(index) => vwat()
      case PlaceholderNameT(inner) => vwat()
      case StructNameT(StructTemplateNameT(humanName), templateArgs) => {
        interner.intern(StructNameT(
          interner.intern(StructTemplateNameT(humanName)),
          templateArgs.map(translateTemplata)))
      }
//      case FunctionNameT(FunctionTemplateNameT(humanName, codeLoc), templateArgs, params) => {
//        interner.intern(FunctionNameT(
//          interner.intern(FunctionTemplateNameT(humanName, codeLoc)),
//          templateArgs.map(translateTemplata),
//          params.map(translateCoord)))
//      }
      case FunctionTemplateNameT(humanName, codeLoc) => name
      case StructTemplateNameT(humanName) => name
      case LambdaCitizenTemplateNameT(codeLoc) => name
      case AnonymousSubstructTemplateNameT(interface) => {
        interner.intern(AnonymousSubstructTemplateNameT(
          translateInterfaceTemplateName(interface)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => name
      case InterfaceTemplateNameT(humanNamee) => name
      case f : IFunctionNameT => translateFunctionName(f)
      case other => vimpl(other)
    }
  }

  def translateImplDefinition(
    implFullName: FullNameT[IImplNameT],
    implDefinition: EdgeT):
  (FullNameT[ICitizenNameT], FullNameT[IInterfaceNameT]) = {
    vassert(!monouts.impls.contains(implFullName))

    val citizen =
      translateCitizenFullName(
        implDefinition.struct,
        translateBoundsForCallee(hinputs.getInstantiationBounds(implDefinition.struct)))
    val interface =
      translateInterfaceFullName(
        implDefinition.interface,
        translateBoundsForCallee(hinputs.getInstantiationBounds(implDefinition.interface)))
    monouts.impls.put(implFullName, (citizen, interface, denizenFunctionBoundToDenizenCallerSuppliedPrototype))

    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.get(interface))
      .put(implFullName, mutable.HashMap())
    vassertSome(monouts.interfaceToImpls.get(interface)).add(implFullName)

    (citizen, interface)
  }
}
