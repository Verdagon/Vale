package dev.vale.typing.citizen

import dev.vale.highertyping.{FunctionA, InterfaceA, StructA}
import dev.vale.{Interner, Keywords, vassertOne, vcurious, vfail, vimpl, vwat, _}
import dev.vale.parsing.ast.{CallMacroP, DontCallMacroP}
import dev.vale.postparsing.rules.RuneUsage
import dev.vale.postparsing._
import dev.vale.typing.expression.CallCompiler
import dev.vale.highertyping._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast.{ICitizenAttributeT, SealedT}
import dev.vale.typing.{CompileErrorExceptionT, CompilerOutputs, ImmStructCantHaveVaryingMember, RangedInternalErrorT, TypingPassOptions, env}
import dev.vale.typing.{ast, _}
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler
import dev.vale.parsing.ast.DontCallMacroP
import dev.vale.typing.env.{CitizenEnvironment, FunctionEnvEntry, IEnvironment, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.names.{AnonymousSubstructImplNameT, CitizenNameT, CitizenTemplateNameT, CodeVarNameT, FreeTemplateNameT, FullNameT, FunctionTemplateNameT, ICitizenTemplateNameT, IInterfaceNameT, IInterfaceTemplateNameT, INameT, IStructNameT, IStructTemplateNameT, InterfaceNameT, InterfaceTemplateNameT, LambdaCitizenTemplateNameT, NameTranslator, RuneNameT, SelfNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.typing.ast._

import scala.collection.immutable.List

class StructCompilerCore(
  opts: TypingPassOptions,
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator,
  delegate: IStructCompilerDelegate) {

  def compileStruct(
    // The environment that the struct was defined in.
    structRunesEnv: CitizenEnvironment[IStructNameT, IStructTemplateNameT],
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    structA: StructA):
  Unit = {
    val templateArgs = structRunesEnv.fullName.last.templateArgs
    val templateFullNameT = structRunesEnv.templateName
    val templateNameT = templateFullNameT.last
    val placeholderedNameT = templateNameT.makeStructName(interner, templateArgs)
    val placeholderedFullNameT = templateFullNameT.copy(last = placeholderedNameT)
    val placeholderedStructTT = interner.intern(StructTT(placeholderedFullNameT))

    val attributesWithoutExportOrMacros =
      structA.attributes.filter({
        case ExportS(_) => false
        case MacroCallS(range, dontCall, macroName) => false
        case _ => true
      })

    val maybeExport =
      structA.attributes.collectFirst { case e@ExportS(_) => e }

    val mutability =
      structRunesEnv.lookupNearestWithImpreciseName(
        interner.intern(RuneNameS(structA.mutabilityRune.rune)),
        Set(TemplataLookupContext)).toList match {
        case List(m) => ITemplata.expectMutability(m)
        case _ => vwat()
      }

    val defaultCalledMacros =
      Vector(
        MacroCallS(structA.range, CallMacroP, keywords.DeriveStructDrop),
        MacroCallS(structA.range, CallMacroP, keywords.DeriveStructFree),
        MacroCallS(structA.range, CallMacroP, keywords.DeriveImplFree))
    val macrosToCall =
      structA.attributes.foldLeft(defaultCalledMacros)({
        case (macrosToCall, mc @ MacroCallS(range, CallMacroP, macroName)) => {
          if (macrosToCall.exists(_.macroName == macroName)) {
            throw CompileErrorExceptionT(RangedInternalErrorT(range :: parentRanges, "Calling macro twice: " + macroName))
          }
          macrosToCall :+ mc
        }
        case (macrosToCall, MacroCallS(_, DontCallMacroP, macroName)) => macrosToCall.filter(_.macroName != macroName)
        case (macrosToCall, _) => macrosToCall
      })

    val envEntriesFromMacros =
      macrosToCall.flatMap({ case MacroCallS(range, CallMacroP, macroName) =>
        val maacro =
          structRunesEnv.globalEnv.nameToStructDefinedMacro.get(macroName) match {
            case None => {
              throw CompileErrorExceptionT(RangedInternalErrorT(range :: parentRanges, "Macro not found: " + macroName))
            }
            case Some(m) => m
          }
        val newEntriesList =
          maacro.getStructChildEntries(macroName, placeholderedFullNameT, structA, mutability)
        val newEntries =
          newEntriesList.map({ case (entryName, value) =>
            vcurious(placeholderedFullNameT.steps.size + 1 == entryName.steps.size)
            val last = entryName.last
            last -> value
          })
        newEntries
      })

    val structInnerEnv =
      CitizenEnvironment(
        structRunesEnv.globalEnv,
        structRunesEnv,
        templateFullNameT,
        placeholderedFullNameT,
        TemplatasStore(placeholderedFullNameT, Map(), Map())
          .addEntries(interner, envEntriesFromMacros))

    val members = makeStructMembers(structInnerEnv, coutputs, structA.members)

    if (mutability == MutabilityTemplata(ImmutableT)) {
      members.zipWithIndex.foreach({ case (member, index) =>
        if (member.variability == VaryingT) {
          throw CompileErrorExceptionT(
            ImmStructCantHaveVaryingMember(
              structA.members(index).range :: parentRanges,
              structA.name,
              structA.members(index) match {
                case NormalStructMemberS(range, name, variability, typeRune) => name.str
                case VariadicStructMemberS(range, variability, typeRune) => "(unnamed)"
              }))
        }
      })
    }

    val structDefT =
      StructDefinitionT(
        templateFullNameT,
        placeholderedStructTT,
        translateCitizenAttributes(attributesWithoutExportOrMacros),
        structA.weakable,
        mutability,
        members,
        false)

    coutputs.add(structDefT);

    maybeExport match {
      case None =>
      case Some(exportPackageCoord) => {
        val exportedName =
          placeholderedFullNameT.last match {
            case StructNameT(StructTemplateNameT(humanName), _) => humanName
            case _ => vfail("Can't export something that doesn't have a human readable name!")
          }
        coutputs.addKindExport(
          structA.range,
          placeholderedStructTT,
          exportPackageCoord.packageCoordinate,
          exportedName)
      }
    }
  }

  def translateCitizenAttributes(attrs: Vector[ICitizenAttributeS]): Vector[ICitizenAttributeT] = {
    attrs.map({
      case SealedS => SealedT
      case MacroCallS(_, _, _) => vwat() // Should have been processed
      case x => vimpl(x.toString)
    })
  }


  // Takes a IEnvironment because we might be inside a:
  // struct<T> Thing<T> {
  //   t: T;
  // }
  // which means we need some way to know what T is.
  def compileInterface(
    interfaceRunesEnv: CitizenEnvironment[IInterfaceNameT, IInterfaceTemplateNameT],
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    interfaceA: InterfaceA):
  (InterfaceDefinitionT) = {
    val templateArgs = interfaceRunesEnv.fullName.last.templateArgs
    val templateFullNameT = interfaceRunesEnv.templateName
    val templateNameT = templateFullNameT.last
    val placeholderedNameT = templateNameT.makeInterfaceName(interner, templateArgs)
    val placeholderedFullNameT = templateFullNameT.copy(last = placeholderedNameT)
    val placeholderedInterfaceTT = interner.intern(InterfaceTT(placeholderedFullNameT))

    val attributesWithoutExportOrMacros =
      interfaceA.attributes.filter({
        case ExportS(_) => false
        case MacroCallS(range, dontCall, macroName) => false
        case _ => true
      })
    val maybeExport =
      interfaceA.attributes.collectFirst { case e@ExportS(_) => e }


    val mutability =
      interfaceRunesEnv.lookupNearestWithImpreciseName(

        interner.intern(RuneNameS(interfaceA.mutabilityRune.rune)),
        Set(TemplataLookupContext)).toList match {
        case List(MutabilityTemplata(m)) => m
        case _ => vwat()
      }

    val defaultCalledMacros =
      Vector(
        MacroCallS(interfaceA.range, CallMacroP, keywords.DeriveInterfaceDrop),
        MacroCallS(interfaceA.range, CallMacroP, keywords.DeriveInterfaceFree))
    val macrosToCall =
      interfaceA.attributes.foldLeft(defaultCalledMacros)({
        case (macrosToCall, mc @ MacroCallS(_, CallMacroP, _)) => macrosToCall :+ mc
        case (macrosToCall, MacroCallS(_, DontCallMacroP, macroName)) => macrosToCall.filter(_.macroName != macroName)
        case (macrosToCall, _) => macrosToCall
      })

    val envEntriesFromMacros =
      macrosToCall.flatMap({ case MacroCallS(range, CallMacroP, macroName) =>
        val maacro =
          interfaceRunesEnv.globalEnv.nameToInterfaceDefinedMacro.get(macroName) match {
            case None => {
              throw CompileErrorExceptionT(RangedInternalErrorT(range :: parentRanges, "Macro not found: " + macroName))
            }
            case Some(m) => m
          }
        val newEntriesList = maacro.getInterfaceChildEntries(placeholderedFullNameT, interfaceA, mutability)
        val newEntries =
          newEntriesList.map({ case (entryName, value) =>
            vcurious(placeholderedFullNameT.steps.size + 1 == entryName.steps.size)
            val last = entryName.last
            last -> value
          })
        newEntries
      })

    val interfaceInnerEnv =
      CitizenEnvironment(
        interfaceRunesEnv.globalEnv,
        interfaceRunesEnv,
        templateFullNameT,
        placeholderedFullNameT,
        TemplatasStore(placeholderedFullNameT, Map(), Map())
          .addEntries(interner, envEntriesFromMacros)
          .addEntries(
            interner,
            interfaceA.genericParameters.zip(interfaceRunesEnv.fullName.last.templateArgs)
              .map({ case (genericParam, templata) => (interner.intern(RuneNameT(genericParam.rune.rune)), TemplataEnvEntry(templata)) }))
          .addEntries(
            interner,
            Vector(interner.intern(SelfNameT()) -> TemplataEnvEntry(KindTemplata(placeholderedInterfaceTT)))))

    val internalMethods2 =
      interfaceA.internalMethods.map(internalMethod => {
//        if (internalMethod.isTemplate) {
          delegate.evaluateTemplatedFunctionFromNonCallForHeader(
            coutputs,
            parentRanges,
            FunctionTemplata(
              interfaceInnerEnv,
              internalMethod),
            true)
//        } else {
//          delegate.evaluateOrdinaryFunctionFromNonCallForHeader(
//            coutputs,
//            parentRanges,
//            FunctionTemplata(
//              interfaceInnerEnv,
//              internalMethod),
//            true)
//        }
      })

    val interfaceDef2 =
      InterfaceDefinitionT(
        templateFullNameT,
        placeholderedInterfaceTT,
        interner.intern(placeholderedInterfaceTT),
        translateCitizenAttributes(attributesWithoutExportOrMacros),
        interfaceA.weakable,
        mutability,
        internalMethods2)
    coutputs.add(interfaceDef2)

    maybeExport match {
      case None =>
      case Some(exportPackageCoord) => {
        val exportedName =
          placeholderedFullNameT.last match {
            case InterfaceNameT(InterfaceTemplateNameT(humanName), _) => humanName
            case _ => vfail("Can't export something that doesn't have a human readable name!")
          }
        coutputs.addKindExport(
          interfaceA.range,
          placeholderedInterfaceTT,
          exportPackageCoord.packageCoordinate,
          exportedName)
      }
    }

    (interfaceDef2)
  }

  private def makeStructMembers(env: IEnvironment, coutputs: CompilerOutputs, members: Vector[IStructMemberS]): (Vector[StructMemberT]) = {
    members.flatMap(makeStructMember(env, coutputs, _))
  }

  private def makeStructMember(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    member: IStructMemberS):
  Vector[StructMemberT] = {
    val typeTemplata =
      vassertOne(
        env.lookupNearestWithImpreciseName(
          interner.intern(RuneNameS(member.typeRune.rune)), Set(TemplataLookupContext)))
    val variabilityT = Conversions.evaluateVariability(member.variability)
    member match {
      case NormalStructMemberS(_, name, _, _) => {
        val CoordTemplata(coord) = typeTemplata
        Vector(
          StructMemberT(
            interner.intern(CodeVarNameT(name)),
            variabilityT,
            ReferenceMemberTypeT(UnsubstitutedCoordT(coord))))
      }
      case VariadicStructMemberS(_, _, _) => {
        val CoordListTemplata(coords) = typeTemplata
        coords.zipWithIndex.map({ case (coord, index) =>
          StructMemberT(interner.intern(CodeVarNameT(interner.intern(StrI(index.toString)))), variabilityT, ReferenceMemberTypeT(UnsubstitutedCoordT(coord)))
        })
      }
    }
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    name: IFunctionDeclarationNameS,
    functionA: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
    val isMutable =
      members.exists({ case StructMemberT(name, variability, tyype) =>
        if (variability == VaryingT) {
          true
        } else {
          tyype match {
            case AddressMemberTypeT(reference) => true
            case ReferenceMemberTypeT(UnsubstitutedCoordT(reference)) => {
              reference.ownership match {
                case OwnT | BorrowT | WeakT => true
                case ShareT => false
              }
            }
          }
        }
      })
    val mutability = if (isMutable) MutableT else ImmutableT

    val understructTemplateNameT = interner.intern(LambdaCitizenTemplateNameT(nameTranslator.translateCodeLocation(functionA.range.begin)))
    val understructTemplatedFullNameT = containingFunctionEnv.fullName.addStep(understructTemplateNameT)
    val understructInstantiatedNameT =
      understructTemplateNameT.makeStructName(interner, Vector())
    val understructInstantiatedFullNameT = containingFunctionEnv.fullName.addStep(understructInstantiatedNameT)

    val understructStructTT = interner.intern(StructTT(understructInstantiatedFullNameT))

    val freeFuncNameT =
      interner.intern(FreeTemplateNameT(functionA.range.begin))
    val dropFuncNameT =
      interner.intern(FunctionTemplateNameT(keywords.drop, functionA.range.begin))

    // We declare the function into the environment that we use to compile the
    // struct, so that those who use the struct can reach into its environment
    // and see the function and use it.
    // See CSFMSEO and SAFHE.
    val structInnerEnv =
      CitizenEnvironment(
        containingFunctionEnv.globalEnv,
        containingFunctionEnv,
        understructTemplatedFullNameT,
        understructTemplatedFullNameT,
        TemplatasStore(understructTemplatedFullNameT, Map(), Map())
          .addEntries(
            interner,
            Vector(
              interner.intern(FunctionTemplateNameT(keywords.underscoresCall, functionA.range.begin)) ->
                env.FunctionEnvEntry(functionA),
              dropFuncNameT ->
                FunctionEnvEntry(
                  containingFunctionEnv.globalEnv.structDropMacro.makeImplicitDropFunction(
                    interner.intern(FunctionNameS(keywords.drop, functionA.range.begin)), functionA.range)),
              understructInstantiatedNameT -> TemplataEnvEntry(KindTemplata(understructStructTT)),
              interner.intern(SelfNameT()) -> TemplataEnvEntry(KindTemplata(understructStructTT))) ++
              (if (mutability == ImmutableT) {
                Vector(
                  freeFuncNameT ->
                    FunctionEnvEntry(
                      containingFunctionEnv.globalEnv.structFreeMacro.makeImplicitFreeFunction(
                        interner.intern(FreeDeclarationNameS(functionA.range.begin)), functionA.range)))
              } else {
                Vector()
              })))
    // We return this from the function in case we want to eagerly compile it (which we do
    // if it's not a template).
    val functionTemplata = FunctionTemplata(structInnerEnv, functionA)

    coutputs.declareType(understructTemplatedFullNameT)
    coutputs.declareTypeOuterEnv(understructTemplatedFullNameT, structInnerEnv)
    coutputs.declareTypeInnerEnv(understructTemplatedFullNameT, structInnerEnv)
    coutputs.declareTypeMutability(understructTemplatedFullNameT, MutabilityTemplata(mutability))

    val closureStructDefinition =
      StructDefinitionT(
        understructTemplatedFullNameT,
        understructStructTT,
        Vector.empty, false, MutabilityTemplata(mutability), members, true);
    coutputs.add(closureStructDefinition)

    val closuredVarsStructRef = understructStructTT;

    if (mutability == ImmutableT) {
      // Adds the free function to the coutputs
      // Free is indeed ordinary because it just takes in the lambda struct. The lambda struct
      // isn't templated. The lambda call function might be, but the struct isnt.
      delegate.evaluateTemplatedFunctionFromNonCallForHeader(
        coutputs,
        parentRanges,
        structInnerEnv.lookupNearestWithName(freeFuncNameT, Set(ExpressionLookupContext)) match {
          case Some(ft@FunctionTemplata(_, _)) => ft
          case _ => throw CompileErrorExceptionT(RangedInternalErrorT(functionA.range :: parentRanges, "Couldn't find closure free function we just added!"))
        },
        true)
      // Adds the drop function to the coutputs
      // Drop is indeed ordinary because it just takes in the lambda struct. The lambda struct
      // isn't templated. The lambda call function might be, but the struct isnt.
      delegate.evaluateTemplatedFunctionFromNonCallForHeader(
        coutputs,
        parentRanges,
        structInnerEnv.lookupNearestWithName(dropFuncNameT, Set(ExpressionLookupContext)) match {
          case Some(ft@FunctionTemplata(_, _)) => ft
          case _ => throw CompileErrorExceptionT(RangedInternalErrorT(functionA.range :: parentRanges, "Couldn't find closure drop function we just added!"))
        },
        true)
    }

    (closuredVarsStructRef, mutability, functionTemplata)
  }
}
