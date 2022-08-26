package dev.vale.monomorphizing

import dev.vale.options.GlobalOptions
import dev.vale.{Interner, vassert, vassertOne, vassertSome, vfail, vimpl, vwat}
import dev.vale.postparsing.{IRuneS, ITemplataType, IntegerTemplataType}
import dev.vale.typing.{Hinputs, TemplataCompiler}
import dev.vale.typing.ast._
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
  val interfaceToEdgeBlueprints: mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceEdgeBlueprint] =
    mutable.HashMap[FullNameT[IInterfaceNameT], InterfaceEdgeBlueprint]()
  val edges: mutable.HashMap[FullNameT[IStructNameT], mutable.HashMap[FullNameT[IInterfaceNameT], EdgeT]] =
    mutable.HashMap[FullNameT[IStructNameT], mutable.HashMap[FullNameT[IInterfaceNameT], EdgeT]]()

  val declaredStructs: mutable.HashSet[FullNameT[IStructNameT]] =
    mutable.HashSet[FullNameT[IStructNameT]]()
  val declaredInterfaces: mutable.HashSet[FullNameT[IInterfaceNameT]] =
    mutable.HashSet[FullNameT[IInterfaceNameT]]()
}

object Monomorphizer {
  def translate(opts: GlobalOptions, interner: Interner, hinputs: Hinputs): Hinputs = {
    val Hinputs(
      interfaces,
      structs,
      functions,
      interfaceToEdgeBlueprints,
      edges,
      kindExports,
      functionExports,
      kindExterns,
      functionExterns) = hinputs

    val monouts = new MonomorphizedOutputs()

    kindExports.foreach({ case KindExportT(range, tyype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName = packageName.addStep(ExportNameT(range.begin))
      val monomorphizer =
        new DenizenMonomorphizer(opts, interner, hinputs, monouts, exportName, exportName, Array(), Map(), Map())
      KindExportT(
        range,
        monomorphizer.translateKind(tyype),
        packageCoordinate,
        exportedName)
    })

    functionExports.foreach({ case FunctionExportT(range, prototype, packageCoordinate, exportedName) =>
      val packageName = FullNameT(packageCoordinate, Vector(), interner.intern(PackageTopLevelNameT()))
      val exportName = packageName.addStep(ExportNameT(range.begin))
      val monomorphizer =
        new DenizenMonomorphizer(
          opts, interner, hinputs, monouts, exportName, exportName, Array(), Map(), Map())
      FunctionExportT(
        range,
        monomorphizer.translatePrototype(prototype, Map()),
        packageCoordinate,
        exportedName)
    })

//    interfaceToEdgeBlueprints.foreach({ case (interfacePlaceholderedFullName, edge) =>
//      val monomorphizer = new DenizenMonomorphizer(interner, monouts, interfacePlaceholderedFullName)
//
//    })

    Hinputs(
      monouts.interfaces.values.toVector,
      monouts.structs.values.toVector,
      monouts.functions.values.toVector,
      monouts.interfaceToEdgeBlueprints.toMap,
      monouts.edges.values.flatMap(_.values).toVector,
      kindExports,
      functionExports,
      kindExterns,
      functionExterns)
  }
}

class DenizenMonomorphizer(
  opts: GlobalOptions,
  interner: Interner,
  hinputs: Hinputs,
  monouts: MonomorphizedOutputs,
  denizenTemplateName: FullNameT[ITemplateNameT],
  denizenName: FullNameT[INameT],
  placeholderIndexToTemplata: Array[ITemplata[ITemplataType]],
  callerRuneToSuppliedFunction: Map[IRuneS, PrototypeTemplata],
  selfFunctionBoundToRuneUnsubstituted: Map[PrototypeT, IRuneS]) {

  val selfFunctionBoundToRune =
    selfFunctionBoundToRuneUnsubstituted.map({ case (PrototypeT(fullName, returnType), rune) =>
      PrototypeT(translateFullFunctionName(fullName), translateCoord(returnType)) -> rune
    })

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

  def translatePrototype(
    prototype: PrototypeT,
    // Self is the caller of this call that's about to happen
    selfRuneToSuppliedFunction: Map[IRuneS, PrototypeTemplata]):
  PrototypeT = {
    val PrototypeT(fullName, returnType) = prototype

    val desiredPrototype =
      PrototypeT(
        translateFullFunctionName(fullName),
        translateCoord(returnType))

    if (desiredPrototype.fullName.steps.init == denizenTemplateName.steps) {
      // We're calling one of our function bounds.
      // First, figure out the rune in our own env corresponding to this prototype.
      val rune = vassertSome(selfFunctionBoundToRune.get(desiredPrototype))

      // Now we want to call the function bound referred to by this rune.
      // This is something supplied by our own caller.
      val funcToCall = vassertSome(callerRuneToSuppliedFunction.get(rune))
      funcToCall.prototype
    } else {
      desiredPrototype.fullName.last match {
        case ExternFunctionNameT(_, _) => {

        }
        case _ => {
          val funcTemplateNameT = TemplataCompiler.getFunctionTemplate(desiredPrototype.fullName)
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
              desiredPrototype.fullName,
              desiredPrototype.fullName.last.templateArgs.toArray,
              selfRuneToSuppliedFunction,
              funcT.functionBoundToRune)

          val monomorphizedFuncT =
            monomorphizer
              .translateFunction(funcT)
          vassert(monomorphizedFuncT.header.fullName == desiredPrototype.fullName)
        }
      }

      desiredPrototype
    }
  }

  def translateStructDefinition(
    structDefT: StructDefinitionT):
  StructDefinitionT = {
    val StructDefinitionT(templateName, instantiatedCitizen, attributes, weakable, mutability, members, isClosure) = structDefT

    // instantiate all edges

    val newFullName = translateStructFullName(instantiatedCitizen.fullName)

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
        isClosure)

    monouts.structs.put(result.instantiatedCitizen.fullName, result)
    result
  }

  def translateInterfaceDefinition(interfaceDefT: InterfaceDefinitionT): InterfaceDefinitionT = {
    val InterfaceDefinitionT(templateName, instantiatedCitizen, ref, attributes, weakable, mutability, internalMethods) = interfaceDefT

    val newFullName = translateInterfaceFullName(instantiatedCitizen.fullName)

    monouts.interfaces.get(newFullName) match {
      case Some(interface) => return interface
      case None =>
    }

    val result =
      InterfaceDefinitionT(
        templateName,
        interner.intern(InterfaceTT(newFullName)),
        ref,
        attributes,
        weakable,
        mutability,
        internalMethods.map(header => translateFunctionHeader(header)))

    monouts.interfaces.put(result.instantiatedInterface.fullName, result)
    result
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
      case FunctionCallTE(callable, runeToFunctionBoundUnsubstituted, args) => {
        val runeToFunctionBound =
          runeToFunctionBoundUnsubstituted.map({ case (rune, PrototypeTemplata(declarationRange, prototype)) =>
            // We're resolving some function bounds, and function bounds have no function bounds
            // themselves, so we supply Map() here.
            (rune -> PrototypeTemplata(declarationRange, translatePrototype(prototype, Map())))
          })

        FunctionCallTE(
          translatePrototype(callable, runeToFunctionBound),
          Map(),
          args.map(translateRefExpr))
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
          translatePrototype(prototype2, Map()),
          args.map(translateRefExpr))
      }
      case ConstructTE(structTT, resultReference, args) => {
        ConstructTE(
          translateStruct(structTT),
          translateCoord(resultReference),
          args.map(translateExpr))
      }
      case DestroyTE(expr, structTT, destinationReferenceVariables) => {
        DestroyTE(
          translateRefExpr(expr),
          translateStruct(structTT),
          destinationReferenceVariables.map(translateReferenceLocalVariable))
      }
      case MutateTE(destinationExpr, sourceExpr) => {
        MutateTE(
          translateAddrExpr(destinationExpr),
          translateRefExpr(sourceExpr))
      }
      case InterfaceFunctionCallTE(superFunctionHeader, resultReference, args) => {
        InterfaceFunctionCallTE(
          translateFunctionHeader(superFunctionHeader),
          translateCoord(resultReference),
          args.map(translateRefExpr))
      }
      case UpcastTE(innerExpr, targetSuperKind) => {
        UpcastTE(
          translateRefExpr(innerExpr),
          translateSuperKind(targetSuperKind))
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
      case ConstructTE(structTT, resultReference, args) => {
        ConstructTE(
          translateStruct(structTT),
          translateCoord(resultReference),
          args.map(translateExpr))
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
    fullName: FullNameT[IStructNameT]):
  FullNameT[IStructNameT] = {
    val FullNameT(module, steps, last) = fullName
    FullNameT(
      module,
      steps.map(translateName),
      translateStructName(last))
  }

  def translateInterfaceFullName(
    fullName: FullNameT[IInterfaceNameT]):
  FullNameT[IInterfaceNameT] = {
    val FullNameT(module, steps, last) = fullName
    FullNameT(
      module,
      steps.map(translateName),
      translateInterfaceName(last))
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
                case (OwnT, OwnT) => BorrowT
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

  def translateStruct(struct: StructTT): StructTT = {
    val StructTT(fullName) = struct

    val desiredStruct = interner.intern(StructTT(translateStructFullName(fullName)))
    val desiredStructTemplate = TemplataCompiler.getStructTemplate(desiredStruct.fullName)

    if (monouts.declaredStructs.contains(desiredStruct.fullName)) {
      return desiredStruct
    }
    monouts.declaredStructs.add(desiredStruct.fullName)

    val structDefT =
      vassertOne(
        hinputs.structs.filter(struct => struct.templateName == desiredStructTemplate))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        desiredStructTemplate,
        desiredStruct.fullName,
        desiredStruct.fullName.last.templateArgs.toArray,
        Map(),
        Map())

    val monomorphizedStructT =
      monomorphizer
        .translateStructDefinition(structDefT)
    vassert(monomorphizedStructT.instantiatedCitizen.fullName == desiredStruct.fullName)

    desiredStruct
  }

  def translateInterface(interface: InterfaceTT): InterfaceTT = {
    val InterfaceTT(fullName) = interface

    val desiredInterface = interner.intern(InterfaceTT(translateInterfaceFullName(fullName)))
    val desiredInterfaceTemplate = TemplataCompiler.getInterfaceTemplate(desiredInterface.fullName)

    if (monouts.declaredInterfaces.contains(desiredInterface.fullName)) {
      return desiredInterface
    }
    monouts.declaredInterfaces.add(desiredInterface.fullName)

    val interfaceDefT =
      vassertOne(
        hinputs.interfaces.filter(interface => interface.templateName == desiredInterfaceTemplate))

    val monomorphizer =
      new DenizenMonomorphizer(
        opts,
        interner,
        hinputs,
        monouts,
        desiredInterfaceTemplate,
        desiredInterface.fullName,
        desiredInterface.fullName.last.templateArgs.toArray,
        Map(),
        Map())

    val monomorphizedInterfaceT =
      monomorphizer
        .translateInterfaceDefinition(interfaceDefT)
    vassert(monomorphizedInterfaceT.instantiatedCitizen.fullName == desiredInterface.fullName)

    desiredInterface
  }

  def translateSuperKind(kind: ISuperKindTT): ISuperKindTT = {
    kind match {
      case i @ InterfaceTT(_) => translateInterface(i)
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

  def translateKind(kind: KindT): KindT = {
    kind match {
      case IntT(bits) => IntT(bits)
      case BoolT() => BoolT()
      case FloatT() => FloatT()
      case VoidT() => VoidT()
      case StrT() => StrT()
      case NeverT(fromBreak) => NeverT(fromBreak)
      case p @ PlaceholderT(_) => translatePlaceholder(p)
      case s @ StructTT(_) => translateStruct(s)
      case s @ InterfaceTT(_) => translateInterface(s)
      case a @ StaticSizedArrayTT(_, _, _, _) => translateStaticSizedArray(a)
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
      case other => vimpl(other)
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
      case FunctionNameT(FunctionTemplateNameT(humanName, codeLoc), templateArgs, params) => {
        interner.intern(FunctionNameT(
          interner.intern(FunctionTemplateNameT(humanName, codeLoc)),
          templateArgs.map(translateTemplata),
          params.map(translateCoord)))
      }
      case FunctionTemplateNameT(humanName, codeLoc) => name
      case StructTemplateNameT(humanName) => name
      case LambdaCitizenTemplateNameT(codeLoc) => name
      case AnonymousSubstructTemplateNameT(interface) => {
        interner.intern(AnonymousSubstructTemplateNameT(
          translateInterfaceTemplateName(interface)))
      }
      case LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation)) => LambdaCitizenNameT(LambdaCitizenTemplateNameT(codeLocation))
      case other => vimpl(other)
    }
  }
}
