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

import scala.collection.mutable

class MonomorphizedOutputs() {
  val functions: mutable.HashMap[FullNameT[IFunctionNameT], FunctionT] =
    mutable.HashMap[FullNameT[IFunctionNameT], FunctionT]()
  val structs: mutable.HashMap[FullNameT[IStructNameT], StructDefinitionT] =
    mutable.HashMap[FullNameT[IStructNameT], StructDefinitionT]()
  val interfaces: mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceDefinitionT] =
    mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceDefinitionT]()
  val edges: mutable.HashMap[FullNameT[IImplNameT], EdgeT] =
    mutable.HashMap[FullNameT[IImplNameT], EdgeT]()
  val interfaceToEdgeBlueprints: mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceEdgeBlueprint] =
    mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceEdgeBlueprint]()
//  val interfaceToSubCitizenToEdge: mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[FullNameT[ICitizenNameT], EdgeT]] =
//    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[FullNameT[ICitizenNameT], EdgeT]]()
  val immKindToDestructor: mutable.HashMap[KindT, PrototypeT] =
    mutable.HashMap[KindT, PrototypeT]()

  // We already know from the hinputs that Some<T> implements Opt<T>.
  // In this map, we'll know that Some<int> implements Opt<int>, Some<bool> implements Opt<bool>, etc.
  val interfaceToImpls: mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashSet[FullNameT[IImplNameT]]] =
    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashSet[FullNameT[IImplNameT]]]()
  val impls: mutable.HashMap[FullNameT[IImplNameT], (FullNameT[ICitizenNameT], FullNameT[IInterfaceNameT])] =
    mutable.HashMap[FullNameT[IImplNameT], (FullNameT[ICitizenNameT], FullNameT[IInterfaceNameT])]()
  // We already know from the hinputs that Opt<T has drop> has func drop(T).
  // In this map, we'll know that Opt<int> has func drop(int).
  val interfaceToAbstractFuncToBounds: mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[PrototypeT, Map[IRuneS, PrototypeTemplata]]] =
    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[PrototypeT, Map[IRuneS, PrototypeTemplata]]]()
  // This map collects all overrides for every impl. We'll use it to assemble vtables soon.
  val interfaceToImplToAbstractPrototypeToOverridePrototype:
    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[FullNameT[IImplNameT], mutable.HashMap[PrototypeT, PrototypeT]]] =
    mutable.HashMap[FullNameT[IInterfaceNameT], mutable.HashMap[FullNameT[IImplNameT], mutable.HashMap[PrototypeT, PrototypeT]]]()

  // These are new impls and abstract funcs we discover for interfaces.
  // As we discover a new impl or a new abstract func, we'll later need to stamp a lot more overrides either way.
  val newImpls: mutable.Queue[(FullNameT[IImplNameT], Map[IRuneS, PrototypeTemplata])] = mutable.Queue()
  val newAbstractFuncs: mutable.Queue[(PrototypeT, FullNameT[IInterfaceNameT])] = mutable.Queue()
  val newInterfaces: mutable.Queue[(FullNameT[IInterfaceNameT], Map[IRuneS, PrototypeTemplata])] = mutable.Queue()
  val newFunctions: mutable.Queue[(FullNameT[IFunctionNameT], Map[IRuneS, PrototypeTemplata])] = mutable.Queue()

  def addMethodToVTable(implFullName: FullNameT[IImplNameT], superInterfaceFullName: FullNameT[IInterfaceNameT], interfaceAbstractFunc: PrototypeT, funcT: FunctionT) = {
    val map =
      interfaceToImplToAbstractPrototypeToOverridePrototype
        .getOrElseUpdate(superInterfaceFullName, mutable.HashMap())
        .getOrElseUpdate(implFullName, mutable.HashMap())
    vassert(!map.contains(interfaceAbstractFunc))
    map.put(interfaceAbstractFunc, funcT.header.toPrototype)
  }
}

object Monomorphizer {
  def translate(opts: GlobalOptions, interner: Interner, hinputs: Hinputs): Hinputs = {
    val Hinputs(
      interfaces,
      structs,
      functions,
      oldImmKindToDestructor,
      interfaceToEdgeBlueprints,
      interfaceToSubCitizenToEdge,
      instantiationNameToFunctionBoundToRune,
      kindExports,
      functionExports,
      kindExterns,
      functionExterns) = hinputs

    val monouts = new MonomorphizedOutputs()

    kindExports.foreach({ case KindExportT(range, tyype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName =
        packageName.addStep(interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)))))
      val exportTemplateName = TemplataCompiler.getExportTemplate(exportName)
      val monomorphizer =
        new DenizenMonomorphizer(
          opts, interner, hinputs, monouts, exportTemplateName, exportName, Array(), Map(), Map())
      KindExportT(
        range,
        monomorphizer.translateKind(tyype),
        packageCoordinate,
        exportedName)
    })

    functionExports.foreach({ case FunctionExportT(range, prototype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName =
        packageName.addStep(
          interner.intern(ExportNameT(interner.intern(ExportTemplateNameT(range.begin)))))
      val exportTemplateName = TemplataCompiler.getExportTemplate(exportName)
      val monomorphizer =
        new DenizenMonomorphizer(
          opts, interner, hinputs, monouts, exportTemplateName, exportName, Array(), Map(), Map())
      FunctionExportT(
        range,
        monomorphizer.translatePrototype(prototype),
        packageCoordinate,
        exportedName)
    })

    while ({
      // We make structs inline
      // if (monouts.newStructs.nonEmpty) {
      //   val newStructName = monouts.newStructs.dequeue()
      //   DenizenMonomorphizer.translateStructDefinition(opts, interner, hinputs, monouts, newStructName)
      //   true
      // } else
      if (monouts.newInterfaces.nonEmpty) {
        val (newInterfaceName, calleeRuneToSuppliedPrototype) = monouts.newInterfaces.dequeue()
        DenizenMonomorphizer.translateInterfaceDefinition(
          opts, interner, hinputs, monouts, newInterfaceName, calleeRuneToSuppliedPrototype)
        true
      } else if (monouts.newFunctions.nonEmpty) {
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
        val (interfaceAbstractFunc, interfaceFullName) = monouts.newAbstractFuncs.dequeue()
        DenizenMonomorphizer.translateAbstractFunc(
          opts, interner, hinputs, monouts, interfaceFullName, interfaceAbstractFunc)
        true
      } else {
        false
      }
    }) {}

//    interfaceToEdgeBlueprints.foreach({ case (interfacePlaceholderedFullName, edge) =>
//      val monomorphizer = new DenizenMonomorphizer(interner, monouts, interfacePlaceholderedFullName)
//
//    })

    Hinputs(
      monouts.interfaces.values.toVector,
      monouts.structs.values.toVector,
      monouts.functions.values.toVector,
      monouts.immKindToDestructor.toMap,
      monouts.interfaceToEdgeBlueprints.toMap,
      monouts.edges.groupBy(_._2.interface).mapValues(_.groupBy(_._2.struct).mapValues(x => vassertOne(x.values))),
      Map(),
      kindExports,
      functionExports,
      kindExterns,
      functionExterns)
  }
}

object DenizenMonomorphizer {
  def translateInterfaceDefinition(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    interfaceFullName: FullNameT[IInterfaceNameT],
    calleeRuneToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
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
        interfaceDefT.functionBoundToRune,
        calleeRuneToSuppliedFunction)
    monomorphizer.translateInterfaceDefinition(interfaceFullName, interfaceDefT)
  }

  def translateStructDefinition(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    structFullName: FullNameT[IStructNameT],
    runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
  Unit = {
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
        structDefT.functionBoundToRune,
        runeToSuppliedFunction)

    val newStructDef =
      monomorphizer.translateStructDefinition(structFullName, structDefT)

    vassert(newStructDef.instantiatedCitizen.fullName == structFullName)
  }

  def translateAbstractFunc(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    interfaceFullName: FullNameT[IInterfaceNameT],
    interfaceAbstractFunc: PrototypeT) = {
    val implToAbstractPrototypeToOverridePrototype =
      vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.get(interfaceFullName))

    implToAbstractPrototypeToOverridePrototype.foreach({
      case (implFullName, abstractPrototypeToOverridePrototype) => {
        val implTemplateFullName = TemplataCompiler.getImplTemplate(implFullName)

        val (subCitizenFullName, superInterfaceFullName) = vassertSome(monouts.impls.get(implFullName))

        val implDefinition =
          vassertOne(
            hinputs.interfaceToSubCitizenToEdge
              .flatMap(_._2.values)
              .filter(edge => TemplataCompiler.getImplTemplate(edge.edgeFullName) == implTemplateFullName))

        val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenFullName)
        val subCitizenDefinition = hinputs.lookupCitizenByTemplateFullName(subCitizenTemplateFullName)
        val subCitizenPlaceholderedName = subCitizenDefinition.instantiatedCitizen
        val superInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(superInterfaceFullName)
        val superInterfaceDefinition = hinputs.lookupInterfaceByTemplateFullName(superInterfaceTemplateFullName)
        val superInterfacePlaceholderedName = superInterfaceDefinition.instantiatedInterface

        val interfaceAbstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(interfaceAbstractFunc.fullName)

        val edge =
          vassertSome(
            vassertSome(hinputs.interfaceToSubCitizenToEdge.get(superInterfacePlaceholderedName.fullName))
              .get(subCitizenPlaceholderedName.fullName))

        val overridePrototype =
          vassertSome(edge.abstractFuncTemplateToOverrideFunc.get(interfaceAbstractFuncTemplateName))

        val bounds =
          vassertSome(
            vassertSome(monouts.interfaceToAbstractFuncToBounds.get(superInterfaceFullName))
              .get(interfaceAbstractFunc))

        val funcT =
          DenizenMonomorphizer.translateFunction(
            opts, interner, hinputs, monouts, overridePrototype.fullName, bounds)

        monouts.addMethodToVTable(implFullName, superInterfaceFullName, interfaceAbstractFunc, funcT)

        abstractPrototypeToOverridePrototype.put(interfaceAbstractFunc, overridePrototype)
      }
    })
  }

  def translateImpl(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    implFullName: FullNameT[IImplNameT],
    runeToSuppliedFunctionForUnsubstitutedImpl: Map[IRuneS, PrototypeTemplata]):
  Unit = {
    val implTemplateFullName = TemplataCompiler.getImplTemplate(implFullName)
    val implDefinition =
      vassertOne(
        hinputs.interfaceToSubCitizenToEdge
          .flatMap(_._2.values)
          .filter(edge => TemplataCompiler.getImplTemplate(edge.edgeFullName) == implTemplateFullName))
    //        val EdgeT(edgeTemplateFullName, placeholderedStructFullName, placeholderedInterfaceFullName, methods) = edge
    //        val desiredEdgeName = interner.intern(StructTT(translateStructFullName(fullName)))
    //        val desiredStructTemplate = TemplataCompiler.getStructTemplate(desiredStruct.fullName)
    //        val edgeFullName = translateFullName(edgeTemplateFullName)

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        implTemplateFullName,
        implFullName,
        implFullName.last.templateArgs.toArray,
        implDefinition.functionBoundToRune,
        runeToSuppliedFunctionForUnsubstitutedImpl)
    monomorphizer.translateImplDefinition(implFullName, implDefinition)
  }

  def translateFunction(
    opts: GlobalOptions,
    interner: Interner,
    hinputs: Hinputs,
    monouts: MonomorphizedOutputs,
    desiredFuncFullName: FullNameT[IFunctionNameT],
    runeToSuppliedPrototype: Map[IRuneS, PrototypeTemplata]):
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
        funcT.functionBoundToRune,
        runeToSuppliedPrototype)

    val monomorphizedFuncT = monomorphizer.translateFunction(funcT)

    if (opts.sanityCheck) {
      vassert(Collector.all(desiredFuncFullName, { case PlaceholderTemplateNameT(_) => }).isEmpty)
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
  selfFunctionBoundToRuneUnsubstituted: Map[PrototypeT, IRuneS],
  denizenRuneToDenizenCallerPrototype: Map[IRuneS, PrototypeTemplata]) {

  // This is just here to get scala to include these fields so i can see them in the debugger
  vassert(TemplataCompiler.getTemplate(denizenName) == denizenTemplateName)

  if (opts.sanityCheck) {
    vassert(Collector.all(denizenRuneToDenizenCallerPrototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
  }

  def translateStructMember(member: StructMemberT): StructMemberT = {
    val StructMemberT(name, variability, tyype) = member
    StructMemberT(
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

  // This is run at the call site, from the caller's perspective
  def translatePrototype(
    desiredPrototypeUnsubstituted: PrototypeT):
  PrototypeT = {
    val PrototypeT(desiredPrototypeFullNameUnsubstituted, desiredPrototypeReturnTypeUnsubstituted) = desiredPrototypeUnsubstituted

    // We need a map from rune to the *instantiated* function to call.

    // This is a map from rune to a prototype, specifically the prototype that we
    // (the *template* caller) is supplying to the *template* callee. This prototype might
    // be a placeholder, phrased in terms of our (the *template* caller's) placeholders
    val runeToSuppliedPrototypeForCallUnsubstituted =
      hinputs.getInstantiationBounds(desiredPrototypeUnsubstituted.fullName)
    
strt here // make sure everywhere else calling getInstantiationBounds is doing the below translation

    // For any that are placeholders themselves, let's translate those into actual prototypes.
    val runeToSuppliedPrototypeForCall =
      assembleRuneToPrototypeForCall(runeToSuppliedPrototypeForCallUnsubstituted)

    val desiredPrototype =
      PrototypeT(
        translateFullFunctionName(desiredPrototypeFullNameUnsubstituted),
        translateCoord(desiredPrototypeReturnTypeUnsubstituted))

    desiredPrototype.fullName.last match {
      case FunctionBoundNameT(_, _, _) => {
        // We're calling one of our function bounds.
        // First, figure out the rune in our own env corresponding to this prototype.
        val rune = vassertSome(selfFunctionBoundToRuneUnsubstituted.get(desiredPrototypeUnsubstituted))

        // Now we want to call the function bound referred to by this rune.
        // This is something supplied by our own caller.
        val funcToCall = vassertSome(denizenRuneToDenizenCallerPrototype.get(rune))

        if (opts.sanityCheck) {
          vassert(Collector.all(funcToCall.prototype, { case PlaceholderTemplateNameT(_) => }).isEmpty)
        }

        funcToCall.prototype
      }
      case ExternFunctionNameT(_, _) => {
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

  private def assembleRuneToPrototypeForCall(runeToSuppliedPrototypeForCallUnsubstituted: Map[IRuneS, PrototypeTemplata]) = {
    runeToSuppliedPrototypeForCallUnsubstituted.map({ case (rune, pt@PrototypeTemplata(range, suppliedPrototypeUnsubstituted)) =>
      rune ->
        (suppliedPrototypeUnsubstituted.fullName.last match {
          case FunctionBoundNameT(_, _, _) => {
            val runeForBound = vassertSome(selfFunctionBoundToRuneUnsubstituted.get(suppliedPrototypeUnsubstituted))
            val callerFuncBound = vassertSome(denizenRuneToDenizenCallerPrototype.get(runeForBound))
            callerFuncBound
          }
          case _ => pt
        })
    })
  }

  def translateStructDefinition(
    newFullName: FullNameT[IStructNameT],
    structDefT: StructDefinitionT):
  StructDefinitionT = {
    val StructDefinitionT(templateName, instantiatedCitizen, attributes, weakable, mutability, members, isClosure, fbtr) = structDefT
    vassert(fbtr == selfFunctionBoundToRuneUnsubstituted)

    monouts.structs.get(newFullName) match {
      case Some(struct) => return struct
      case None =>
    }

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

    monouts.structs.put(result.instantiatedCitizen.fullName, result)
    result
  }

  def translateInterfaceDefinition(
    newFullName: FullNameT[IInterfaceNameT],
    interfaceDefT: InterfaceDefinitionT):
  Unit = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutability, fbtr, internalMethods) = interfaceDefT
    vassert(fbtr == selfFunctionBoundToRuneUnsubstituted)

    monouts.interfaces.get(newFullName) match {
      case Some(interface) => return interface
      case None =>
    }

    val newInterfaceTT = interner.intern(InterfaceTT(newFullName))

    val result =
      InterfaceDefinitionT(
        templateName,
        newInterfaceTT,
        ref,
        attributes,
        weakable,
        mutability,
        Map(),
        internalMethods.map(header => {
          translatePrototype(header.toPrototype)
          translateFunctionHeader(header)
        }))

    val edgeBlueprint =
      vassertSome(hinputs.interfaceToEdgeBlueprints.get(instantiatedCitizen.fullName))
    monouts.interfaceToEdgeBlueprints.put(
      newFullName,
      InterfaceEdgeBlueprint(
        newFullName,
        edgeBlueprint.superFamilyRootHeaders.map(header => {
          translatePrototype(header.toPrototype)
          translateFunctionHeader(header)
        })))
    vassert(!monouts.interfaceToAbstractFuncToBounds.contains(newFullName))
    monouts.interfaceToAbstractFuncToBounds.put(newFullName, mutable.HashMap())

    monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.put(newFullName, mutable.HashMap())

    monouts.interfaces.put(result.instantiatedInterface.fullName, result)

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
    val FunctionT(headerT, fbtr, bodyT) = functionT
    if (opts.sanityCheck) {
      vassert(fbtr == selfFunctionBoundToRuneUnsubstituted)
    }

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
      case InterfaceFunctionCallTE(superFunctionHeader, resultReference, args) => {
        val callee = translateFunctionHeader(superFunctionHeader)
        val result =
          InterfaceFunctionCallTE(
            callee,
            translateCoord(resultReference),
            args.map(translateRefExpr))
        val interfaceFullNameT = vassertSome(superFunctionHeader.getAbstractInterface).fullName
        val interfaceFullName =
          translateInterfaceFullName(
            interfaceFullNameT, hinputs.getInstantiationBounds(superFunctionHeader.toPrototype.fullName))


        // This is a map from rune to a prototype, specifically the prototype that we
        // (the *template* caller) is supplying to the *template* callee. This prototype might
        // be a placeholder, phrased in terms of our (the *template* caller's) placeholders
        val runeToSuppliedPrototypeForCallUnsubstituted =
          hinputs.getInstantiationBounds(superFunctionHeader.toPrototype.fullName)

        // For any that are placeholders themselves, let's translate those into actual prototypes.
        val runeToPrototypeForCall =
          assembleRuneToPrototypeForCall(runeToSuppliedPrototypeForCallUnsubstituted)

        monouts.newAbstractFuncs.enqueue((superFunctionHeader.toPrototype, interfaceFullName))

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
          translateStruct(structTT, hinputs.getInstantiationBounds(structTT.fullName)),
          coord,
          args.map(translateExpr),
          free)
      }
      case DestroyTE(expr, structTT, destinationReferenceVariables) => {
        DestroyTE(
          translateRefExpr(expr),
          translateStruct(structTT, hinputs.getInstantiationBounds(structTT.fullName)),
          destinationReferenceVariables.map(translateReferenceLocalVariable))
      }
      case MutateTE(destinationExpr, sourceExpr) => {
        MutateTE(
          translateAddrExpr(destinationExpr),
          translateRefExpr(sourceExpr))
      }
      case u @ UpcastTE(innerExprUnsubstituted, targetSuperKind, untranslatedImplFullName, interfaceFreePrototype) => {
        val implFullName = translateImplFullName(untranslatedImplFullName, hinputs.getInstantiationBounds(untranslatedImplFullName))
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
    runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
  FullNameT[IStructNameT] = {
    val FullNameT(module, steps, lastT) = fullNameT

    val fullName =
      FullNameT(
        module,
        steps.map(translateName),
        translateStructName(lastT))

    DenizenMonomorphizer.translateStructDefinition(
      opts, interner, hinputs, monouts, fullNameT,
      runeToSuppliedFunction)

    return fullName
  }

  def translateInterfaceFullName(
    fullName: FullNameT[IInterfaceNameT],
    calleeRuneToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
  FullNameT[IInterfaceNameT] = {
    val FullNameT(module, steps, last) = fullName
    val newFullName =
      FullNameT(
        module,
        steps.map(translateName),
        translateInterfaceName(last))
    monouts.newInterfaces.enqueue((newFullName, calleeRuneToSuppliedFunction))
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
    runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
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
    runeToSuppliedFunctionForUnsubstitutedImpl: Map[IRuneS, PrototypeTemplata]):
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

  def translateStruct(struct: StructTT, runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]): StructTT = {
    val StructTT(fullName) = struct

    val desiredStruct = interner.intern(StructTT(translateStructFullName(fullName, runeToSuppliedFunction)))

    desiredStruct
  }

  def translateInterface(interface: InterfaceTT, runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]): InterfaceTT = {
    val InterfaceTT(fullName) = interface

    val desiredInterface = interner.intern(InterfaceTT(translateInterfaceFullName(fullName, runeToSuppliedFunction)))

    desiredInterface
  }

  def translateSuperKind(kind: ISuperKindTT): ISuperKindTT = {
    kind match {
      case i @ InterfaceTT(_) => translateInterface(i, hinputs.getInstantiationBounds(i.fullName))
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
      case s @ StructTT(_) => translateStruct(s, hinputs.getInstantiationBounds(s.fullName))
      case s @ InterfaceTT(_) => translateInterface(s, hinputs.getInstantiationBounds(s.fullName))
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
  Unit = {
    vassert(!monouts.impls.contains(implFullName))

    val citizen =
      translateCitizenFullName(
        implDefinition.struct, hinputs.getInstantiationBounds(implDefinition.struct))
    val interface =
      translateInterfaceFullName(
        implDefinition.interface, hinputs.getInstantiationBounds(implDefinition.interface))
    monouts.impls.put(implFullName, (citizen, interface))

    vassertSome(monouts.interfaceToImplToAbstractPrototypeToOverridePrototype.get(interface))
      .put(implFullName, mutable.HashMap())

    val (subCitizenFullName, superInterfaceFullName) = vassertSome(monouts.impls.get(implFullName))
    val subCitizenTemplateFullName = TemplataCompiler.getCitizenTemplate(subCitizenFullName)
    val subCitizenDefinition = hinputs.lookupCitizenByTemplateFullName(subCitizenTemplateFullName)
    val subCitizenPlaceholderedName = subCitizenDefinition.instantiatedCitizen
    val superInterfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(superInterfaceFullName)
    val superInterfaceDefinition = hinputs.lookupInterfaceByTemplateFullName(superInterfaceTemplateFullName)
    val superInterfacePlaceholderedName = superInterfaceDefinition.instantiatedInterface

    val interfaceAbstractFuncToBounds = vassertSome(monouts.interfaceToAbstractFuncToBounds.get(superInterfaceFullName))
    interfaceAbstractFuncToBounds.foreach({ case (interfaceAbstractFunc, bounds) =>
      val edge =
        vassertSome(
          vassertSome(hinputs.interfaceToSubCitizenToEdge.get(superInterfacePlaceholderedName.fullName))
            .get(subCitizenPlaceholderedName.fullName))
      val interfaceAbstractFuncTemplateName = TemplataCompiler.getFunctionTemplate(interfaceAbstractFunc.fullName)

      val overridePrototype =
        vassertSome(edge.abstractFuncTemplateToOverrideFunc.get(interfaceAbstractFuncTemplateName))

      val funcT =
        DenizenMonomorphizer.translateFunction(
          opts, interner, hinputs, monouts, overridePrototype.fullName, bounds)

      monouts.addMethodToVTable(implFullName, superInterfaceFullName, interfaceAbstractFunc, funcT)
    })
  }
}
