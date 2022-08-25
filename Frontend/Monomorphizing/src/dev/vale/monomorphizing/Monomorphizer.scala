package dev.vale.monomorphizing

import dev.vale.options.GlobalOptions
import dev.vale.{Interner, vassert, vassertOne, vassertSome, vfail, vimpl, vwat}
import dev.vale.postparsing.{IRuneS, ITemplataType, IntegerTemplataType}
import dev.vale.typing.{Hinputs, TemplataCompiler}
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplata.expectKind
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

  def translateStruct(
    structDefT: StructDefinitionT):
  StructDefinitionT = {
    val StructDefinitionT(templateName, placeholderedCitizen, attributes, weakable, mutability, members, isClosure) = structDefT

    // instantiate all edges

    vimpl()
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

      desiredPrototype
    }
  }

  def translateInterface(structDefT: InterfaceDefinitionT): InterfaceDefinitionT = {
    val InterfaceDefinitionT(templateName, placeholderedCitizen, attributes, weakable, mutability, members, isClosure) = structDefT

    vimpl()
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

    val result =
      FunctionT(
        FunctionHeaderT(
          newFullName,
          attributes,
          params.map(translateParameter),
          translateCoord(returnType),
          maybeOriginFunctionTemplata),
        Map(),
        translateRefExpr(bodyT))
    monouts.functions.put(result.header.fullName, result)
    result
  }

  def translateLocalVariable(
    variable: ILocalVariableT):
  ILocalVariableT = {
    variable match {
      case ReferenceLocalVariableT(id, variability, reference) => {
        ReferenceLocalVariableT(
          translateFullVarName(id),
          variability,
          translateCoord(reference))
      }
      case AddressibleLocalVariableT(id, variability, reference) => {
        AddressibleLocalVariableT(
          translateFullVarName(id),
          variability,
          translateCoord(reference))
      }
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
      case ConstantBoolTE(value) => ConstantBoolTE(value)
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

  def translateKind(
    kind: KindT):
  KindT = {
    kind match {
      case IntT(bits) => IntT(bits)
      case BoolT() => BoolT()
      case VoidT() => VoidT()
      case PlaceholderT(FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index)))) => {
        vassert(index >= 0)
        vassert(index < placeholderIndexToTemplata.length)
        ITemplata.expectKindTemplata(placeholderIndexToTemplata(index)).kind
      }
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
      case other => vimpl(other)
    }
  }

  def translateVarName(
    name: IVarNameT):
  IVarNameT = {
    name match {
      case TypingPassFunctionResultVarNameT() => name
      case CodeVarNameT(_) => name
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
      case other => vimpl(other)
    }
  }
}
