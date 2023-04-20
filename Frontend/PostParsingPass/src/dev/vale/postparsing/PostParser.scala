package dev.vale.postparsing

//import dev.vale.astronomer.{Astronomer, AstroutsBox, Environment, IRuneS, ITemplataType}
import dev.vale.options.GlobalOptions
import dev.vale.postparsing.patterns.PatternScout
import dev.vale.postparsing.rules.{IRulexSR, LiteralSR, MutabilityLiteralSL, RuleScout, RuneUsage, TemplexScout}
import dev.vale._
import dev.vale.parsing._
import dev.vale.parsing.ast._
import PostParser.{determineDenizenType, evalRange}
import dev.vale.lexing.{FailedParse, RangeL}
import dev.vale.parsing.ParserCompilation
import dev.vale.parsing.ast.{AnonymousRunePT, BoolPT, CallPT, ExportAsP, ExportAttributeP, FileP, IAttributeP, IImpreciseNameP, ITemplexPT, ImplP, ImportP, InlinePT, IntPT, InterfaceP, InterpretedPT, IterableNameP, IterationOptionNameP, IteratorNameP, LocationPT, LookupNameP, MacroCallP, MutabilityP, MutabilityPT, NameOrRunePT, NameP, NormalStructMemberP, OwnershipPT, RulePUtils, SealedAttributeP, StaticSizedArrayPT, StructMembersP, StructMethodP, StructP, TopLevelExportAsP, TopLevelFunctionP, TopLevelImplP, TopLevelImportP, TopLevelInterfaceP, TopLevelStructP, TuplePT, VariadicStructMemberP, WeakableAttributeP}
//import dev.vale.postparsing.predictor.RuneTypeSolveError
import dev.vale.postparsing.rules._
import dev.vale.Err

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class CompileErrorExceptionS(err: ICompileErrorS) extends RuntimeException { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }

sealed trait ICompileErrorS { def range: RangeS }
case class UnknownRuleFunctionS(range: RangeS, name: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class BadRuneAttributeErrorS(range: RangeS, attr: IRuneAttributeP) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CantHaveMultipleMutabilitiesS(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class UnimplementedExpression(range: RangeS, expressionName: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CouldntFindVarToMutateS(range: RangeS, name: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CouldntFindRuneS(range: RangeS, name: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class StatementAfterReturnS(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class ForgotSetKeywordError(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class UnknownRegionError(range: RangeS, name: String) extends ICompileErrorS {
  vpass()
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
}
case class CantUseThatLocalName(range: RangeS, name: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class ExternHasBody(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class InitializingRuntimeSizedArrayRequiresSizeAndCallable(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class InitializingStaticSizedArrayRequiresSizeAndCallable(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CantOwnershipInterfaceInImpl(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CantOwnershipStructInImpl(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CantOverrideOwnershipped(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class VariableNameAlreadyExists(range: RangeS, name: IVarNameS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class InterfaceMethodNeedsSelf(range: RangeS) extends ICompileErrorS {
  vpass()
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
}
case class VirtualAndAbstractGoTogether(range: RangeS) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class CouldntSolveRulesS(range: RangeS, error: RuneTypeSolveError) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class RuneExplicitTypeConflictS(range: RangeS, rune: IRuneS, types: Vector[ITemplataType]) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class IdentifyingRunesIncompleteS(range: RangeS, error: IdentifiabilitySolveError) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
case class RangedInternalErrorS(range: RangeS, message: String) extends ICompileErrorS { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }

sealed trait IEnvironmentS {
  def file: FileCoordinate
  def name: INameS
  def allDeclaredRunes(): Set[IRuneS]
  def localDeclaredRunes(): Set[IRuneS]
}

// Someday we might split this into PackageEnvironment and CitizenEnvironment
case class EnvironmentS(
    file: FileCoordinate,
    parentEnv: Option[EnvironmentS],
    name: INameS,
    userDeclaredRunes: Set[IRuneS]
) extends IEnvironmentS {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def localDeclaredRunes(): Set[IRuneS] = {
    userDeclaredRunes
  }
  override def allDeclaredRunes(): Set[IRuneS] = {
    userDeclaredRunes ++ parentEnv.toVector.flatMap(_.allDeclaredRunes())
  }
}

case class FunctionEnvironmentS(
  file: FileCoordinate,
  name: IFunctionDeclarationNameS,
  parentEnv: Option[IEnvironmentS],
  // Contains all the identifying runes and otherwise declared runes from this function's rules.
  // These are important for knowing whether e.g. T is a type or a rune when we process all the runes.
  // See: Must Scan For Declared Runes First (MSFDRF)
  private val declaredRunes: Set[IRuneS],
  // So that when we run into a magic param, we can add this to the number of previous magic
  // params to get the final param index.
  numExplicitParams: Int,
  // Whether this is an abstract method inside defined inside an interface.
  // (Maybe we can instead determine this by looking at parentEnv?)
  isInterfaceInternalMethod: Boolean
) extends IEnvironmentS {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def localDeclaredRunes(): Set[IRuneS] = {
    declaredRunes
  }
  override def allDeclaredRunes(): Set[IRuneS] = {
    declaredRunes ++ parentEnv.toVector.flatMap(_.allDeclaredRunes()).toSet
  }
  def child(): FunctionEnvironmentS = {
    FunctionEnvironmentS(file, name, Some(this), Set(), numExplicitParams, false)
  }
}

case class StackFrame(
    file: FileCoordinate,
    name: IFunctionDeclarationNameS,
    parentEnv: FunctionEnvironmentS,
    maybeParent: Option[StackFrame],
    contextRegion: IRuneS,
    pureHeight: Int,
    locals: VariableDeclarations) {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  def ++(newVars: VariableDeclarations): StackFrame = {
    StackFrame(file, name, parentEnv, maybeParent, contextRegion, pureHeight, locals ++ newVars)
  }
  def allDeclarations: VariableDeclarations = {
    locals ++ maybeParent.map(_.allDeclarations).getOrElse(PostParser.noDeclarations)
  }
  def findVariable(name: IImpreciseNameS): Option[IVarNameS] = {
    locals.find(name) match {
      case Some(fullNameS) => Some(fullNameS)
      case None => {
        maybeParent match {
          case None => None
          case Some(parent) => parent.findVariable(name)
        }
      }
    }
  }
}

object PostParser {
//  val VIRTUAL_DROP_FUNCTION_NAME = "vdrop"
  // Interface's drop function simply calls vdrop.
  // A struct's vdrop function calls the struct's drop function.

  def noVariableUses = VariableUses(Vector.empty)
  def noDeclarations = VariableDeclarations(Vector.empty)

  def evalRange(file: FileCoordinate, range: RangeL): RangeS = {
    RangeS(evalPos(file, range.begin), evalPos(file, range.end))
  }

  def evalPos(file: FileCoordinate, pos: Int): CodeLocationS = {
    CodeLocationS(file, pos)
  }

  def translateImpreciseName(interner: Interner, file: FileCoordinate, name: IImpreciseNameP): IImpreciseNameS = {
    name match {
      case LookupNameP(name) => interner.intern(CodeNameS(name.str))
      case IterableNameP(range) => IterableNameS(PostParser.evalRange(file,  range))
      case IteratorNameP(range) => IteratorNameS(PostParser.evalRange(file,  range))
      case IterationOptionNameP(range) => IterationOptionNameS(PostParser.evalRange(file,  range))
    }
  }

  // Err is the missing rune
  def determineDenizenType(
    templateResultType: ITemplataType,
    identifyingRunesS: Vector[IRuneS],
    runeAToType: Map[IRuneS, ITemplataType]):
  Result[ITemplataType, IRuneS] = {
    val isTemplate = identifyingRunesS.nonEmpty

    val tyype =
      if (isTemplate) {
        TemplateTemplataType(
          identifyingRunesS.map(identifyingRuneA => {
            runeAToType.get(identifyingRuneA) match {
              case None => return Err(identifyingRuneA)
              case Some(x) => x
            }
          }),
          templateResultType)
      } else {
        templateResultType
      }
    Ok(tyype)
  }

  def getHumanName(interner: Interner, templex: ITemplexPT): IImpreciseNameS = {
    templex match {
      //      case NullablePT(_, inner) => getHumanName(inner)
      case InlinePT(_, inner) => getHumanName(interner, inner)
      //      case PermissionedPT(_, permission, inner) => getHumanName(inner)
      case InterpretedPT(_, ownership, region, inner) => getHumanName(interner, inner)
      case AnonymousRunePT(_) => vwat()
      case NameOrRunePT(NameP(_, name)) => interner.intern(CodeNameS(name))
      case CallPT(_, template, args) => getHumanName(interner, template)
      case StaticSizedArrayPT(_, mutability, variability, size, element) => vwat()
      case TuplePT(_, members) => vwat()
      case IntPT(_, value) => vwat()
      case BoolPT(_, value) => vwat()
      case OwnershipPT(_, ownership) => vwat()
      case MutabilityPT(_, mutability) => vwat()
      case LocationPT(_, location) => vwat()
    }
  }

//  def knownEndsInVoid(expr: IExpressionSE): Boolean = {
//    expr match {
//      case VoidSE(_) => true
//      case ReturnSE(_, _) => true
//      case DestructSE(_, _) => true
//      case IfSE(_, _, thenBody, elseBody) => knownEndsInVoid(thenBody) && knownEndsInVoid(elseBody)
//      case WhileSE(_, _) => true
//    }
//  }

//  def pruneTrailingVoids(exprs: Vector[IExpressionSE]): Vector[IExpressionSE] = {
//    if (exprs.size >= 2) {
//      exprs.last match {
//        case VoidSE(_) => {
//          exprs.init.last match {
//            case ReturnSE(_, _) => return exprs.init
//            case VoidSE(_) => return pruneTrailingVoids(exprs.init)
//            case
//          }
//        }
//        case _ =>
//      }
//    }
//  }

  def consecutive(exprs: Vector[IExpressionSE]): IExpressionSE = {
    if (exprs.isEmpty) {
      vcurious()
    } else if (exprs.size == 1) {
      exprs.head
    } else {
      ConsecutorSE(
        exprs.flatMap({
          case ConsecutorSE(exprs) => exprs
          case other => List(other)
        }))
    }
  }

  def scoutGenericParameter(
      templexScout: TemplexScout,
      env: IEnvironmentS,
      lidb: LocationInDenizenBuilder,
      runeToExplicitType: mutable.ArrayBuffer[(IRuneS, ITemplataType)],
      ruleBuilder: ArrayBuffer[IRulexSR],
      // This might seem a bit weird, because the region rune usually comes last and is usually
      // mentioned at the end of the header too. But indeed we need it for knowing the region to use
      // for generic params' default values.
      contextRegion: IRuneS,
      genericParamP: GenericParameterP,
      paramRuneS: RuneUsage):
  // Returns a possible implicit region generic param (see MNRFGC), and the translated original
  // generic param.
  GenericParameterS  = {
    val GenericParameterP(genericParamRangeP, _, maybeType, maybeCoordRegionP, originalAttributesP, maybeDefault) = genericParamP
    val genericParamRangeS = PostParser.evalRange(env.file, genericParamRangeP)
    val runeS = paramRuneS

    val typeS =
      maybeType match {
        case None => CoordTemplataType()
        case Some(typeP) => RuleScout.translateType(typeP.tyype)
      }
    runeToExplicitType += ((runeS.rune, typeS))

    val maybeExplicitCoordRegionS =
      maybeCoordRegionP match {
        case Some(RegionRunePT(rangeP, name)) => {
          val rangeS = PostParser.evalRange(env.file, rangeP)
          Some(RuneUsage(rangeS, CodeRuneS(vassertSome(name).str))) // impl isolates
        }
        case None => None
      }


    val genericParamTypeS =
      typeS match {
        case CoordTemplataType() => {
          val (immutableAttrs, remainingAttributesP) =
            U.extract[IRuneAttributeP, Unit](originalAttributesP, {
              case ImmutableRuneAttributeP(rangeP) => Unit
            })
          if (remainingAttributesP.nonEmpty) {
            val first = remainingAttributesP.head
            throw CompileErrorExceptionS(BadRuneAttributeErrorS(evalRange(env.file, first.range), first))
          }
          val mutable = immutableAttrs.isEmpty
          CoordGenericParameterTypeS(None, mutable)
        }
        case RegionTemplataType() => {
          val (mutabilityAttrsS, remainingAttributesP) =
            U.extract[IRuneAttributeP, IRegionMutabilityS](originalAttributesP, {
              case ImmutableRegionRuneAttributeP(_) => ImmutableRegionS
              case AdditiveRegionRuneAttributeP(_) => AdditiveRegionS
              case ReadWriteRegionRuneAttributeP(_) => ReadWriteRegionS
              case ReadOnlyRegionRuneAttributeP(_) => ReadOnlyRegionS
            })
          if (remainingAttributesP.nonEmpty) {
            val first = remainingAttributesP.head
            throw CompileErrorExceptionS(BadRuneAttributeErrorS(evalRange(env.file, first.range), first))
          }

          if (mutabilityAttrsS.size > 1) {
            throw CompileErrorExceptionS(CantHaveMultipleMutabilitiesS(evalRange(env.file, genericParamRangeP)))
          }
          val mutability = mutabilityAttrsS.headOption.getOrElse(ReadOnlyRegionS)
          RegionGenericParameterTypeS(mutability)
        }
        case other => {
          if (originalAttributesP.nonEmpty) {
            val first = originalAttributesP.head
            throw CompileErrorExceptionS(BadRuneAttributeErrorS(evalRange(env.file, first.range), first))
          }
          OtherGenericParameterTypeS(other)
        }
      }

    val defaultS =
      maybeDefault.map(defaultPT => {
        val uncategorizedRules = ArrayBuffer[IRulexSR]()
        val resultRune = templexScout.translateTemplex(env, lidb, uncategorizedRules, contextRegion, defaultPT)._2
        uncategorizedRules += EqualsSR(genericParamRangeS, runeS, resultRune)

        val rulesToLeaveInDefaultArgument = new Accumulator[IRulexSR]()
        uncategorizedRules.foreach({
          case r @ PackSR(_, _, _) => ruleBuilder += r // Hoist it up into regular rules
          case r @ LiteralSR(_, _, _) => rulesToLeaveInDefaultArgument.add(r)
          case r @ MaybeCoercingLookupSR(_, _, _, _) => rulesToLeaveInDefaultArgument.add(r)
          case r @ ResolveSR(_, _, _, _, _) => rulesToLeaveInDefaultArgument.add(r)
          case r @ EqualsSR(_, _, _) => ruleBuilder += r // Hoist it up into regular rules
          case r @ CallSiteFuncSR(_, _, _, _, _) => ruleBuilder += r // Hoist it up into regular rules
          case r @ DefinitionFuncSR(_, _, _, _, _) => ruleBuilder += r // Hoist it up into regular rules
          case other => vwat(other)
        })

        GenericParameterDefaultS(
          resultRune.rune, rulesToLeaveInDefaultArgument.buildArray().toVector)
      })

//    val (maybeImplicitRegionGenericParam, maybeCoordRegionS) =
//      (typeS, maybeExplicitCoordRegionS) match {
//        case (CoordTemplataType(), Some(explicitCoordRegionS)) => {
//          (None, Some(explicitCoordRegionS))
//        }
//        case (CoordTemplataType(), None) => {
////          val implicitRegionRune = ImplicitRegionRuneS(runeS.rune)
////          val implicitRegionGenericParam =
////            GenericParameterS(
////              genericParamRangeS, RuneUsage(genericParamRangeS, implicitRegionRune), RegionTemplataType(), None, Vector(), None)
//          (None, Some(RuneUsage(genericParamRangeS, implicitRegionRune)))
//
//        }
//        case _ => (None, None)
//      }

    val genericParamS =
      GenericParameterS(genericParamRangeS, runeS, genericParamTypeS, defaultS)
    genericParamS
  }
}

class PostParser(
    globalOptions: GlobalOptions,
    interner: Interner,
    keywords: Keywords) {
  val templexScout = new TemplexScout(interner, keywords)
  val ruleScout = new RuleScout(interner, keywords, templexScout)
  val functionScout = new FunctionScout(this, interner, keywords, templexScout, ruleScout)

  def scoutProgram(fileCoordinate: FileCoordinate, parsed: FileP): Result[ProgramS, ICompileErrorS] = {
    Profiler.frame(() => {
      try {
        val structsS = parsed.denizens.collect({ case TopLevelStructP(s) => s }).map(scoutStruct(fileCoordinate, _));
        val interfacesS = parsed.denizens.collect({ case TopLevelInterfaceP(i) => i }).map(scoutInterface(fileCoordinate, _));
        val implsS = parsed.denizens.collect({ case TopLevelImplP(i) => i }).map(scoutImpl(fileCoordinate, _))
        val functionsS =
          parsed.denizens
            .collect({ case TopLevelFunctionP(f) => f })
            .map(functionP => {
              val (functionS, variableUses) =
                functionScout.scoutFunction(fileCoordinate, functionP, FunctionNoParent())
              vassert(variableUses.uses.isEmpty)
              functionS
            })
        val exportsS = parsed.denizens.collect({ case TopLevelExportAsP(e) => e }).map(scoutExportAs(fileCoordinate, _))
        val importsS = parsed.denizens.collect({ case TopLevelImportP(e) => e }).map(scoutImport(fileCoordinate, _))
        val programS = ProgramS(structsS.toVector, interfacesS.toVector, implsS.toVector, functionsS.toVector, exportsS.toVector, importsS.toVector)
        Ok(programS)
      } catch {
        case CompileErrorExceptionS(err) => Err(err)
      }
    })
  }

  private def scoutImpl(file: FileCoordinate, impl0: ImplP): ImplS = {
    val ImplP(rangeP, maybeGenericParametersP, maybeTemplateRulesP, maybeStruct, interface, attributes) = impl0
    val rangeS = PostParser.evalRange(file, rangeP)

    interface match {
      case InterpretedPT(range, _, _, _) => {
        throw CompileErrorExceptionS(CantOwnershipInterfaceInImpl(PostParser.evalRange(file, range)))
      }
      case _ =>
    }

    maybeStruct match {
      case Some(InterpretedPT(range, _, _, _)) => {
        throw CompileErrorExceptionS(CantOwnershipStructInImpl(PostParser.evalRange(file, range)))
      }
      case _ =>
    }

    val templateRulesP = maybeTemplateRulesP.toVector.flatMap(_.rules)

    val codeLocation = rangeS.begin
    val implName = interner.intern(ImplDeclarationNameS(codeLocation))

    val userSpecifiedIdentifyingRunes =
      maybeGenericParametersP
        .toVector.flatMap(_.params)
        .map(_.name)
        .map({ case NameP(range, identifyingRuneName) => rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName)) })
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map({ case NameP(range, identifyingRuneName) => rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName)) })
    val userDeclaredRunes = userSpecifiedIdentifyingRunes ++ runesFromRules
    val userDeclaredRunesSet = userDeclaredRunes.map(_.rune).toSet

    val implEnv = postparsing.EnvironmentS(file, None, implName, userDeclaredRunes.map(_.rune).toSet)

    val lidb = new LocationInDenizenBuilder(Vector())
    val ruleBuilder = ArrayBuffer[IRulexSR]()
    val runeToExplicitType = mutable.ArrayBuffer[(IRuneS, ITemplataType)]()

    val (defaultRegionRuneRangeS, defaultRegionRuneS, maybeRegionGenericParam) =
    {
      val regionRange = RangeS(rangeS.end, rangeS.end)
      val rune = DenizenDefaultRegionRuneS(implName)
      runeToExplicitType += ((rune, RegionTemplataType()))
      val implicitRegionGenericParam =
        GenericParameterS(regionRange, RuneUsage(regionRange, rune), RegionGenericParameterTypeS(ReadWriteRegionS), None)
      (regionRange, rune, Some(implicitRegionGenericParam))
    }

    val genericParametersP =
      maybeGenericParametersP
        .toVector
        .flatMap(_.params)

    // We'll add the implicit runes to the end, see IRRAE.
    val userSpecifiedGenericParametersS =
      genericParametersP.zip(userSpecifiedIdentifyingRunes)
        .map({ case (g, r) =>
          PostParser.scoutGenericParameter(
            templexScout, implEnv, lidb.child(), runeToExplicitType, ruleBuilder, defaultRegionRuneS, g, r)
        })
//    val userSpecifiedRunesImplicitRegionRunesS = userSpecifiedRunesImplicitRegionRunesUnflattenedS.flatten

    ruleScout.translateRulexes(implEnv, lidb.child(), ruleBuilder, runeToExplicitType, defaultRegionRuneS, templateRulesP)

    val struct =
      maybeStruct match {
        case None => throw CompileErrorExceptionS(postparsing.RangedInternalErrorS(rangeS, "Impl needs struct!"))
        case Some(x) => x
      }

    val structRune =
      templexScout.translateMaybeTypeIntoRune(
        implEnv,
        lidb.child(),
        rangeS,
        ruleBuilder,
        defaultRegionRuneS,
        Some(struct))._2

    val interfaceRune =
      templexScout.translateMaybeTypeIntoRune(
        implEnv,
        lidb.child(),
        rangeS,
        ruleBuilder,
        defaultRegionRuneS,
        Some(interface))._2

    val subCitizenImpreciseName =
      struct match {
        case CallPT(_, NameOrRunePT(name), _) if !userDeclaredRunesSet.contains(CodeRuneS(name.str)) => interner.intern(CodeNameS(name.str))
        case NameOrRunePT(name) if !userDeclaredRunesSet.contains(CodeRuneS(name.str)) => interner.intern(CodeNameS(name.str))
        case _ => throw CompileErrorExceptionS(RangedInternalErrorS(PostParser.evalRange(file, struct.range), "Can't determine name of struct!"))
      }

    val superInterfaceImpreciseName =
      interface match {
        case CallPT(_, NameOrRunePT(name), _) if !userDeclaredRunesSet.contains(CodeRuneS(name.str)) => interner.intern(CodeNameS(name.str))
        case NameOrRunePT(name) if !userDeclaredRunesSet.contains(CodeRuneS(name.str)) => interner.intern(CodeNameS(name.str))
        case _ => throw CompileErrorExceptionS(RangedInternalErrorS(PostParser.evalRange(file, struct.range), "Can't determine name of struct!"))
      }

    val genericParametersS =
      userSpecifiedGenericParametersS
        //++ userSpecifiedRunesImplicitRegionRunesS

    val tyype = TemplateTemplataType(genericParametersS.map(_.tyype.tyype), KindTemplataType())

    ImplS(
      rangeS,
      implName,
      genericParametersS,
      ruleBuilder.toVector,
      runeToExplicitType.toMap,
      tyype,
      structRune,
      subCitizenImpreciseName,
      interfaceRune,
      superInterfaceImpreciseName)
  }

  private def scoutExportAs(file: FileCoordinate, exportAsP: ExportAsP): ExportAsS = {
    val ExportAsP(rangeP, templexP, exportedName) = exportAsP

    val rangeS = PostParser.evalRange(file, rangeP)
    val pos = rangeS.begin
    val exportName = interner.intern(ExportAsNameS(pos))
    val exportEnv = EnvironmentS(file, None, exportName, Set())

    val lidb = new LocationInDenizenBuilder(Vector())
    val ruleBuilder = ArrayBuffer[IRulexSR]()
    val runeToExplicitType = mutable.ArrayBuffer[(IRuneS, ITemplataType)]()

    val (defaultRegionRuneRangeS, defaultRegionRuneS, regionGenericParam) =
    {
      val regionRange = RangeS(rangeS.end, rangeS.end)
      val rune = DenizenDefaultRegionRuneS(exportName)
      runeToExplicitType += ((rune, RegionTemplataType()))
      val implicitRegionGenericParam =
        GenericParameterS(regionRange, RuneUsage(regionRange, rune), RegionGenericParameterTypeS(ReadWriteRegionS), None)
      (regionRange, rune, implicitRegionGenericParam)
    }

    val runeS = templexScout.translateTemplex(exportEnv, lidb, ruleBuilder, defaultRegionRuneS, templexP)._2

    postparsing.ExportAsS(rangeS, ruleBuilder.toVector, regionGenericParam, defaultRegionRuneS, exportName, runeS, exportedName.str)
  }

  private def scoutImport(file: FileCoordinate, importP: ImportP): ImportS = {
    val ImportP(range, moduleName, packageNames, importeeName) = importP

    val pos = PostParser.evalPos(file, range.begin)

    postparsing.ImportS(PostParser.evalRange(file, range), moduleName.str, packageNames.map(_.str), importeeName.str)
  }

  private def predictMutability(rangeS: RangeS, mutabilityRuneS: IRuneS, rulesS: Vector[IRulexSR]):
  Option[MutabilityP] = {
    val predictedMutabilities =
      rulesS.collect({
        case LiteralSR(_, runeS, MutabilityLiteralSL(mutability)) if runeS.rune == mutabilityRuneS => mutability
      })
    val predictedMutability =
      predictedMutabilities.size match {
        case 0 => None
        case 1 => Some(predictedMutabilities.head)
        case _ => throw CompileErrorExceptionS(RangedInternalErrorS(rangeS, "Too many mutabilities: " + predictedMutabilities.mkString("[", ", ", "]")))
      }
    predictedMutability
  }

  private def scoutStruct(file: FileCoordinate, head: StructP): StructS = {
    val StructP(rangeP, NameP(structNameRange, structHumanName), attributesP, mutabilityPT, maybeGenericParametersP, maybeTemplateRulesP, maybeDefaultRegionRuneP, bodyRangeP, StructMembersP(_, members)) = head

    val structRangeS = PostParser.evalRange(file, rangeP)
    val structName = interner.intern(postparsing.TopLevelStructDeclarationNameS(structHumanName, PostParser.evalRange(file, structNameRange)))
    val bodyRangeS = PostParser.evalRange(file, bodyRangeP)

    val lidb = new LocationInDenizenBuilder(Vector())

    val genericParametersP =
      maybeGenericParametersP
        .toVector
        .flatMap(_.params)

    val userSpecifiedIdentifyingRunes =
      genericParametersP
        .map({ case GenericParameterP(_, NameP(range, identifyingRuneName), _, _, _, _) =>
          rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName))
        })

    val templateRulesP = maybeTemplateRulesP.toVector.flatMap(_.rules)

    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map({ case NameP(range, identifyingRuneName) => rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName)) })
    val userDeclaredRunes = userSpecifiedIdentifyingRunes ++ runesFromRules
    val structEnv = postparsing.EnvironmentS(file, None, structName, userDeclaredRunes.map(_.rune).toSet)

    val headerRuleBuilder = ArrayBuffer[IRulexSR]()
    val headerRuneToExplicitType = mutable.ArrayBuffer[(IRuneS, ITemplataType)]()

    val (defaultRegionRuneRangeS, defaultRegionRuneS, maybeRegionGenericParam) =
      maybeDefaultRegionRuneP match {
        case None => {
          val regionRange = RangeS(bodyRangeS.begin, bodyRangeS.begin)
          val rune = DenizenDefaultRegionRuneS(structName)
          headerRuneToExplicitType += ((rune, RegionTemplataType()))
          val implicitRegionGenericParam =
            GenericParameterS(regionRange, RuneUsage(regionRange, rune), RegionGenericParameterTypeS(ReadWriteRegionS), None)
          (regionRange, rune, Some(implicitRegionGenericParam))
        }
        case Some(RegionRunePT(regionRangeP, regionName)) => {
          val regionRangeS = evalRange(file, regionRangeP)
          val rune = CodeRuneS(vassertSome(regionName).str) // impl isolates
          if (!structEnv.allDeclaredRunes().contains(rune)) {
            throw CompileErrorExceptionS(CouldntFindRuneS(regionRangeS, rune.name.str))
          }
          (regionRangeS, rune, None)
        }
      }


    val structUserSpecifiedGenericParametersS =
      genericParametersP.zip(userSpecifiedIdentifyingRunes)
        .map({ case (g, r) =>
          PostParser.scoutGenericParameter(
            templexScout, structEnv, lidb.child(), headerRuneToExplicitType, headerRuleBuilder, defaultRegionRuneS, g, r)
        })
//    val userSpecifiedRunesImplicitRegionRunesS = userSpecifiedRunesImplicitRegionRunesUnflattenedS.flatten

    val genericParametersS =
      structUserSpecifiedGenericParametersS ++
        maybeRegionGenericParam
        //++ userSpecifiedRunesImplicitRegionRunesS

    ruleScout.translateRulexes(structEnv, lidb.child(), headerRuleBuilder, headerRuneToExplicitType, defaultRegionRuneS, templateRulesP)

    val memberRuleBuilder = ArrayBuffer[IRulexSR]()
    val membersRuneToExplicitType = mutable.HashMap[IRuneS, ITemplataType]()

    val mutability =
      mutabilityPT.getOrElse(MutabilityPT(RangeL(bodyRangeP.begin, bodyRangeP.begin), MutableP))
    val mutabilityRuneS =
      templexScout.translateTemplex(
        structEnv, lidb.child(), headerRuleBuilder, defaultRegionRuneS, mutability)._2
    headerRuneToExplicitType += ((mutabilityRuneS.rune, MutabilityTemplataType()))

    val membersS =
      members.flatMap({
        case NormalStructMemberP(range, name, variability, memberType) => {
          val memberRune =
            templexScout.translateTemplex(
              structEnv, lidb.child(), memberRuleBuilder, defaultRegionRuneS, memberType)._2
          membersRuneToExplicitType.put(memberRune.rune, CoordTemplataType())
          Vector(NormalStructMemberS(PostParser.evalRange(structEnv.file, range), name.str, variability, memberRune))
        }
        case VariadicStructMemberP(range, variability, memberType) => {
          val memberRune =
            templexScout.translateTemplex(
              structEnv, lidb.child(), memberRuleBuilder, defaultRegionRuneS, memberType)._2
          membersRuneToExplicitType.put(memberRune.rune, PackTemplataType(CoordTemplataType()))
          Vector(VariadicStructMemberS(PostParser.evalRange(structEnv.file, range), variability, memberRune))
        }
        case StructMethodP(_) => {
          // Implement struct methods one day
          Vector.empty
        }
      })

    val headerRulesS = headerRuleBuilder.toVector
    val memberRulesS = memberRuleBuilder.toVector

    val allRulesS = headerRulesS ++ memberRulesS
    val allRuneToExplicitType = headerRuneToExplicitType ++ membersRuneToExplicitType

    val runeToPredictedType = predictRuneTypes(structRangeS, userSpecifiedIdentifyingRunes.map(_.rune), allRuneToExplicitType, allRulesS)

    val predictedMutability = predictMutability(structRangeS, mutabilityRuneS.rune, allRulesS)

    val runesFromHeader = (userDeclaredRunes.map(_.rune) ++ headerRulesS.flatMap(_.runeUsages.map(_.rune))).toSet
    val headerRuneToPredictedType = runeToPredictedType.filter(x => runesFromHeader.contains(x._1))
    val membersRuneToPredictedType = runeToPredictedType.filter(x => !runesFromHeader.contains(x._1))

    val tyype = TemplateTemplataType(genericParametersS.map(_.tyype.tyype), KindTemplataType())

    val weakable = attributesP.exists({ case w @ WeakableAttributeP(_) => true case _ => false })
    val attrsS = translateCitizenAttributes(file, structName, attributesP.filter({ case WeakableAttributeP(_) => false case _ => true}))

//    val runeSToCanonicalRune = ruleBuilder.runeSToTentativeRune.mapValues(tentativeRune => tentativeRuneToCanonicalRune(tentativeRune))

    StructS(
      structRangeS,
      structName,
      attrsS,
      weakable,
      genericParametersS,
      mutabilityRuneS,
      predictedMutability,
      tyype,
      headerRuneToExplicitType.toMap,
      headerRuneToPredictedType,
      headerRulesS,
      membersRuneToExplicitType.toMap,
      membersRuneToPredictedType,
      memberRulesS,
      defaultRegionRuneS,
      membersS)
  }

  def translateCitizenAttributes(file: FileCoordinate, denizenName: INameS, attrsP: Vector[IAttributeP]): Vector[ICitizenAttributeS] = {
    attrsP.map({
      case ExportAttributeP(_) => ExportS(file.packageCoordinate, ExportDefaultRegionRuneS(denizenName))
      case SealedAttributeP(_) => SealedS
      case MacroCallP(range, dontCall, NameP(_, str)) => MacroCallS(PostParser.evalRange(file, range), dontCall, str)
      case x => vimpl(x.toString)
    })
  }


  def predictRuneTypes(
    rangeS: RangeS,
    identifyingRunesS: Vector[IRuneS],
    runeToExplicitTypeArray: mutable.ArrayBuffer[(IRuneS, ITemplataType)],
    rulesS: Vector[IRulexSR]):
  Map[IRuneS, ITemplataType] = {
    Profiler.frame(() => {
      val runeToExplicitType =
        runeToExplicitTypeArray
          .toVector
          .groupBy(_._1)
          .mapValues(_.map(_._2))
          .mapValues(_.distinct)
          .map({ case (rune, explicitTypes) =>
            if (explicitTypes.size > 1) {
              throw CompileErrorExceptionS(RuneExplicitTypeConflictS(rangeS, rune, explicitTypes))
            }
            (rune, vassertOne(explicitTypes))
          })
      val env =
        new IRuneTypeSolverEnv {
          override def lookup(range: RangeS, name: IImpreciseNameS):
          Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
            vimpl()
          }
        }
      val runeSToLocallyPredictedTypes =
        new RuneTypeSolver(interner).solve(
          globalOptions.sanityCheck,
          globalOptions.useOptimizedSolver,
          env, List(rangeS), true, rulesS, identifyingRunesS, false, runeToExplicitType) match {
          case Ok(t) => t
          // This likely cannot happen because we aren't even asking for a complete solve.
          case Err(e) => throw CompileErrorExceptionS(CouldntSolveRulesS(rangeS, e))
        }
      runeSToLocallyPredictedTypes
    })
  }


  def checkIdentifiability(
    rangeS: RangeS,
    identifyingRunesS: Vector[IRuneS],
    rulesS: Vector[IRulexSR]):
  Unit = {
    IdentifiabilitySolver.solve(
      globalOptions.sanityCheck,
      globalOptions.useOptimizedSolver,
      interner,
      List(rangeS), rulesS, identifyingRunesS) match {
      case Ok(_) =>
      case Err(e) => throw CompileErrorExceptionS(IdentifyingRunesIncompleteS(rangeS, e))
    }
  }

  private def scoutInterface(
    file: FileCoordinate,
    containingInterfaceP: InterfaceP):
  InterfaceS = {
    val InterfaceP(interfaceRangeP, NameP(interfaceNameRangeS, interfaceHumanName), attributesP, mutabilityPT, maybeGenericParametersP, maybeRulesP, maybeDefaultRegionRuneP, bodyRangeP, internalMethodsP) = containingInterfaceP
    val interfaceRangeS = PostParser.evalRange(file, interfaceRangeP)
    val bodyRangeS = PostParser.evalRange(file, bodyRangeP)
    val interfaceFullName = interner.intern(postparsing.TopLevelInterfaceDeclarationNameS(interfaceHumanName, PostParser.evalRange(file, interfaceNameRangeS)))
    val rulesP = maybeRulesP.toVector.flatMap(_.rules)

    val lidb = new LocationInDenizenBuilder(Vector())

    val genericParametersP =
      maybeGenericParametersP
        .toVector
        .flatMap(_.params)

    val userSpecifiedIdentifyingRunes =
      genericParametersP
        .map({ case GenericParameterP(_, NameP(range, identifyingRuneName), _, _, _, _) =>
          rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName))
        })

    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(rulesP)
        .map({ case NameP(range, identifyingRuneName) => rules.RuneUsage(PostParser.evalRange(file, range), CodeRuneS(identifyingRuneName)) })
    val userDeclaredRunes = (userSpecifiedIdentifyingRunes.map(_.rune) ++ runesFromRules.map(_.rune)).distinct
    val interfaceEnv = postparsing.EnvironmentS(file, None, interfaceFullName, userDeclaredRunes.toSet)

    val ruleBuilder = ArrayBuffer[IRulexSR]()
    // This is an array instead of a map so we can detect conflicts afterward
    val runeToExplicitType = mutable.ArrayBuffer[(IRuneS, ITemplataType)]()

    val (defaultRegionRuneRangeS, defaultRegionRuneS, maybeRegionGenericParam) =
      maybeDefaultRegionRuneP match {
        case None => {
          val regionRange = RangeS(bodyRangeS.begin, bodyRangeS.begin)
          val rune = DenizenDefaultRegionRuneS(interfaceFullName)
          runeToExplicitType += ((rune, RegionTemplataType()))
          val implicitRegionGenericParam =
            GenericParameterS(regionRange, RuneUsage(regionRange, rune), RegionGenericParameterTypeS(ReadWriteRegionS), None)
          (regionRange, rune, Some(implicitRegionGenericParam))
        }
        case Some(RegionRunePT(regionRangeP, regionName)) => {
          val regionRangeS = evalRange(file, regionRangeP)
          val rune = CodeRuneS(vassertSome(regionName).str) // impl isolates
          if (!interfaceEnv.allDeclaredRunes().contains(rune)) {
            throw CompileErrorExceptionS(CouldntFindRuneS(regionRangeS, rune.name.str))
          }
          (regionRangeS, rune, None)
        }
      }

    val interfaceUserSpecifiedGenericParametersS =
      genericParametersP.zip(userSpecifiedIdentifyingRunes)
        .map({ case (g, r) =>
          PostParser.scoutGenericParameter(
            templexScout, interfaceEnv, lidb.child(), runeToExplicitType, ruleBuilder, defaultRegionRuneS, g, r)
        })

    val genericParametersS =
      interfaceUserSpecifiedGenericParametersS
      //++ userSpecifiedRunesImplicitRegionRunesS

    ruleScout.translateRulexes(interfaceEnv, lidb.child(), ruleBuilder, runeToExplicitType, defaultRegionRuneS, rulesP)

    val mutability =
      mutabilityPT.getOrElse(MutabilityPT(RangeL(bodyRangeP.begin, bodyRangeP.begin), MutableP))
    val mutabilityRuneS =
      templexScout.translateTemplex(
        interfaceEnv, lidb.child(), ruleBuilder, defaultRegionRuneS, mutability)._2


    val rulesS = ruleBuilder.toVector

    val runeToPredictedType = predictRuneTypes(interfaceRangeS, userDeclaredRunes, mutable.ArrayBuffer(), rulesS)

    val predictedMutability = predictMutability(interfaceRangeS, mutabilityRuneS.rune, rulesS)

    val tyype = TemplateTemplataType(genericParametersS.map(_.tyype.tyype), KindTemplataType())

    val internalMethodsS =
      internalMethodsP.map(method => {
        functionScout.scoutInterfaceMember(
          ParentInterface(
            interfaceEnv,
            genericParametersS.toVector,
            rulesS,
            runeToExplicitType.toMap),
          method)
      })

    val weakable = attributesP.exists({ case w @ WeakableAttributeP(_) => true case _ => false })
    val attrsS = translateCitizenAttributes(file, interfaceFullName, attributesP.filter({ case WeakableAttributeP(_) => false case _ => true}))

    val interfaceS =
      InterfaceS(
        interfaceRangeS,
        interfaceFullName,
        attrsS,
        weakable,
//        knowableValueRunes,
        genericParametersS,
        runeToExplicitType.toMap,
//        localRunes,
//        maybePredictedType,
        mutabilityRuneS,
        predictedMutability,
        runeToPredictedType,
        tyype,
//        isTemplate,
        rulesS,
        defaultRegionRuneS,
//        runeSToCanonicalRune,
        internalMethodsS)

    interfaceS
  }

}

class ScoutCompilation(
  globalOptions: GlobalOptions,
  interner: Interner,
  keywords: Keywords,
  packagesToBuild: Vector[PackageCoordinate],
  packageToContentsResolver: IPackageResolver[Map[String, String]]) {
  var parserCompilation = new ParserCompilation(globalOptions, interner, keywords, packagesToBuild, packageToContentsResolver)
  var scoutputCache: Option[FileCoordinateMap[ProgramS]] = None

  def getCodeMap(): Result[FileCoordinateMap[String], FailedParse] = parserCompilation.getCodeMap()
  def getParseds(): Result[FileCoordinateMap[(FileP, Vector[RangeL])], FailedParse] = parserCompilation.getParseds()
  def getVpstMap(): Result[FileCoordinateMap[String], FailedParse] = parserCompilation.getVpstMap()

  def getScoutput(): Result[FileCoordinateMap[ProgramS], ICompileErrorS] = {
    scoutputCache match {
      case Some(scoutput) => Ok(scoutput)
      case None => {
        val scoutput =
          parserCompilation.expectParseds().map({ case (fileCoordinate, (code, commentsAndRanges)) =>
            new PostParser(globalOptions, interner, keywords).scoutProgram(fileCoordinate, code) match {
              case Err(e) => return Err(e)
              case Ok(p) => p
            }
          })
        scoutputCache = Some(scoutput)
        Ok(scoutput)
      }
    }
  }
  def expectScoutput(): FileCoordinateMap[ProgramS] = {
    getScoutput() match {
      case Ok(x) => x
      case Err(e) => {
        val codeMap = getCodeMap().getOrDie()
        vfail(PostParserErrorHumanizer.humanize(
          SourceCodeUtils.humanizePos(codeMap, _),
          SourceCodeUtils.linesBetween(codeMap, _, _),
          SourceCodeUtils.lineRangeContaining(codeMap, _),
          SourceCodeUtils.lineContaining(codeMap, _),
          e))
      }
    }
  }
}
