package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{PatternScout, RuleState, RuleStateBox}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.predictor.PredictorEvaluator
import net.verdagon.vale.solver.{IRulexAR, Optimizer, TemplarPuzzler}
import net.verdagon.vale.{Err, FileCoordinate, FileCoordinateMap, IPackageResolver, IProfiler, NullProfiler, Ok, PackageCoordinate, Result, vassert, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.List
import scala.util.parsing.input.OffsetPosition

case class CompileErrorExceptionS(err: ICompileErrorS) extends RuntimeException { override def hashCode(): Int = vcurious() }

sealed trait ICompileErrorS { def range: RangeS }
case class CouldntFindVarToMutateS(range: RangeS, name: String) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class ForgotSetKeywordError(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class CantUseThatLocalName(range: RangeS, name: String) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class ExternHasBody(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class CantInitializeIndividualElementsOfRuntimeSizedArray(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class InitializingRuntimeSizedArrayRequiresSizeAndCallable(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class InitializingStaticSizedArrayRequiresSizeAndCallable(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class InitializingStaticSizedArrayFromCallableNeedsSizeTemplex(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class CantOwnershipInterfaceInImpl(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class CantOwnershipStructInImpl(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class CantOverrideOwnershipped(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class VariableNameAlreadyExists(range: RangeS, name: IVarNameS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class InterfaceMethodNeedsSelf(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }
case class VirtualAndAbstractGoTogether(range: RangeS) extends ICompileErrorS { override def hashCode(): Int = vcurious() }

case class RangedInternalErrorS(range: RangeS, message: String) extends ICompileErrorS { override def hashCode(): Int = vcurious() }

sealed trait IEnvironment {
  def file: FileCoordinate
  def name: INameS
  def allUserDeclaredRunes(): Set[IRuneS]
}

// Someday we might split this into PackageEnvironment and CitizenEnvironment
case class Environment(
    file: FileCoordinate,
    parentEnv: Option[Environment],
    name: INameS,
    userDeclaredRunes: Set[IRuneS]
) extends IEnvironment {
  override def hashCode(): Int = vcurious()
  override def allUserDeclaredRunes(): Set[IRuneS] = {
    userDeclaredRunes ++ parentEnv.toVector.flatMap(pe => pe.allUserDeclaredRunes())
  }
}

case class FunctionEnvironment(
    file: FileCoordinate,
    name: IFunctionDeclarationNameS,
    parentEnv: Option[IEnvironment],
    userDeclaredRunes: Set[IRuneS],
    // So that when we run into a magic param, we can add this to the number of previous magic
    // params to get the final param index.
    numExplicitParams: Int
) extends IEnvironment {
  override def hashCode(): Int = vcurious()
  override def allUserDeclaredRunes(): Set[IRuneS] = {
    userDeclaredRunes ++ parentEnv.toVector.flatMap(_.allUserDeclaredRunes())
  }
}

case class StackFrame(
    file: FileCoordinate,
    name: IFunctionDeclarationNameS,
    parentEnv: FunctionEnvironment,
    maybeParent: Option[StackFrame],
    locals: VariableDeclarations) {
  override def hashCode(): Int = vcurious()
  def ++(newVars: VariableDeclarations): StackFrame = {
    StackFrame(file, name, parentEnv, maybeParent, locals ++ newVars)
  }
  def allDeclarations: VariableDeclarations = {
    locals ++ maybeParent.map(_.allDeclarations).getOrElse(Scout.noDeclarations)
  }
  def findVariable(name: String): Option[IVarNameS] = {
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

object Scout {
  def noVariableUses = VariableUses(Vector.empty)
  def noDeclarations = VariableDeclarations(Vector.empty)

//  val unnamedParamNamePrefix = "__param_"
//  val unrunedParamOverrideRuneSuffix = "Override"
//  val unnamedMemberNameSeparator = "_mem_"

  def scoutProgram(fileCoordinate: FileCoordinate, parsed: FileP): Result[ProgramS, ICompileErrorS] = {
    try {
      val structsS = parsed.topLevelThings.collect({ case TopLevelStructP(s) => s }).map(scoutStruct(fileCoordinate, _));
      val interfacesS = parsed.topLevelThings.collect({ case TopLevelInterfaceP(i) => i }).map(scoutInterface(fileCoordinate, _));
      val implsS = parsed.topLevelThings.collect({ case TopLevelImplP(i) => i }).map(scoutImpl(fileCoordinate, _))
      val functionsS = parsed.topLevelThings.collect({ case TopLevelFunctionP(f) => f }).map(FunctionScout.scoutTopLevelFunction(fileCoordinate, _))
      val exportsS = parsed.topLevelThings.collect({ case TopLevelExportAsP(e) => e }).map(scoutExportAs(fileCoordinate, _))
      val importsS = parsed.topLevelThings.collect({ case TopLevelImportP(e) => e }).map(scoutImport(fileCoordinate, _))
      val programS = ProgramS(structsS, interfacesS, implsS, functionsS, exportsS, importsS)
      Ok(programS)
    } catch {
      case CompileErrorExceptionS(err) => Err(err)
    }
  }

  private def scoutImpl(file: FileCoordinate, impl0: ImplP): ImplS = {
    val ImplP(range, identifyingRuneNames, maybeTemplateRulesP, struct, interface) = impl0

    interface match {
      case InterpretedPT(range, _, _, _) => {
        throw CompileErrorExceptionS(CantOwnershipInterfaceInImpl(Scout.evalRange(file, range)))
      }
      case _ =>
    }

    struct match {
      case InterpretedPT(range, _, _, _) => {
        throw CompileErrorExceptionS(CantOwnershipStructInImpl(Scout.evalRange(file, range)))
      }
      case _ =>
    }


    val templateRulesP = maybeTemplateRulesP.toVector.flatMap(_.rules)

    val codeLocation = Scout.evalPos(file, range.begin)
    val implName = ImplNameS(getHumanName(struct), codeLocation)

    val identifyingRunes: Vector[IRuneS] =
      identifyingRuneNames
        .toVector.flatMap(_.runes)
        .map(_.name.str)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = identifyingRunes ++ runesFromRules

    val implEnv = Environment(file, None, implName, userDeclaredRunes.toSet)

    val rate = RuleStateBox(RuleState(implEnv.name, 0))
    val userRulesS =
      RuleScout.translateRulexes(implEnv, rate, implEnv.allUserDeclaredRunes(), templateRulesP)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunesS = PredictorEvaluator.getAllRunes(identifyingRunes, userRulesS, Vector.empty, None)

    val knowableRunesFromAbove = Set[IRuneS]()

    val ruleBuilder = ScoutRuleBuilder()
    userRulesS.foreach(ruleBuilder.translateRule(knowableRunesFromAbove, _))

    val tentativeStructRune =
      PatternScout.translateMaybeTypeIntoRune(
        implEnv,
        rate,
        Scout.evalRange(file, range),
        knowableRunesFromAbove,
        ruleBuilder,
        Some(struct),
        KindTypePR)

    val tentativeInterfaceRune =
      PatternScout.translateMaybeTypeIntoRune(
        implEnv,
        rate,
        Scout.evalRange(file, range),
        knowableRunesFromAbove,
        ruleBuilder,
        Some(interface),
        KindTypePR)

    val (tentativeRuneToCanonicalRune, world) =
      Optimizer.optimize(
        ruleBuilder.builder,
        (inputRule: IRulexAR[Int, RangeS, ILiteralSR, ILookupSR]) => TemplarPuzzler.apply(inputRule))

    val structRune = tentativeRuneToCanonicalRune(tentativeStructRune)
    val interfaceRune = tentativeRuneToCanonicalRune(tentativeInterfaceRune)

    val Conclusions(knowableValueRunes, _) =
      PredictorEvaluator.solve(
        evalRange(file, range),
        ruleBuilder.runeSToTentativeRune,
        tentativeRuneToCanonicalRune,
        ruleBuilder.tentativeRuneToType,
        world)
    val localRunesS = allRunesS
    val isTemplate = knowableValueRunes != allRunesS

    val runeSToCanonicalRune = ruleBuilder.runeSToTentativeRune.mapValues(tentativeRune => tentativeRuneToCanonicalRune(tentativeRune))

    ImplS(
      Scout.evalRange(file, range),
      implName,
      world.rules,
      runeSToCanonicalRune,
      knowableValueRunes.map(runeSToCanonicalRune) ++
        (if (isTemplate) Vector.empty else Vector(structRune, interfaceRune)),
      localRunesS.map(runeSToCanonicalRune) ++ Vector(structRune, interfaceRune),
      isTemplate,
      structRune,
      interfaceRune)
  }

  private def scoutExportAs(file: FileCoordinate, exportAsP: ExportAsP): ExportAsS = {
    val ExportAsP(range, templexP, exportedName) = exportAsP

    val pos = Scout.evalPos(file, range.begin)
    val exportName = ExportAsNameS(pos)
    val exportEnv = Environment(file, None, exportName, Set())
    val templexS = TemplexScout.translateTemplex(exportEnv, templexP)

    ExportAsS(Scout.evalRange(file, range), exportName, templexS, exportedName.str)
  }

  private def scoutImport(file: FileCoordinate, importP: ImportP): ImportS = {
    val ImportP(range, moduleName, packageNames, importeeName) = importP

    val pos = Scout.evalPos(file, range.begin)

    ImportS(Scout.evalRange(file, range), moduleName.str, packageNames.map(_.str), importeeName.str)
  }

  private def scoutStruct(file: FileCoordinate, head: StructP): StructS = {
    val StructP(range, NameP(_, structHumanName), attributesP, mutabilityPT, maybeIdentifyingRunes, maybeTemplateRulesP, StructMembersP(_, members)) = head
    val codeLocation = Scout.evalPos(file, range.begin)
    val structName = TopLevelCitizenDeclarationNameS(structHumanName, codeLocation)

    val structRangeS = Scout.evalRange(file, range)

    val templateRulesP = maybeTemplateRulesP.toVector.flatMap(_.rules)

    val identifyingRunesS: Vector[IRuneS] =
      maybeIdentifyingRunes
          .toVector.flatMap(_.runes).map(_.name.str)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = identifyingRunesS ++ runesFromRules
    val structEnv = Environment(file, None, structName, userDeclaredRunes.toSet)

    val memberRunes = members.indices.map(index => MemberRuneS(index))
    val memberRules =
      memberRunes.zip(members).collect({ case (memberRune, StructMemberP(range, _, _, memberType)) =>
        val memberRange = Scout.evalRange(file, range)
        EqualsSR(
          memberRange,
          TypedSR(memberRange, memberRune, CoordTypeSR),
          TemplexScout.translateTemplex(structEnv, memberType))
      })

    val rate = RuleStateBox(RuleState(structEnv.name, 0))
    val rulesWithoutMutabilityS =
      RuleScout.translateRulexes(structEnv, rate, structEnv.allUserDeclaredRunes(), templateRulesP) ++
      memberRules

    val mutabilityST =
      RuleScout.translateTemplex(structEnv, rate, structEnv.allUserDeclaredRunes(), mutabilityPT)
    val predictedMutability =
      mutabilityPT match {
        case MutabilityPT(_, mutability) => Some(mutability)
        case _ => None
      }

    val mutabilityRuneS = rate.newImplicitRune()
    val rulesS =
      rulesWithoutMutabilityS :+
        EqualsSR(
          structRangeS,
          RuneSR(structRangeS, mutabilityRuneS),
          mutabilityST)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunesS = PredictorEvaluator.getAllRunes(identifyingRunesS, rulesS, Vector.empty, None)

    val knowableRunesFromAbove = Set[IRuneS]()

    val ruleBuilder = ScoutRuleBuilder()
    rulesS.foreach(ruleBuilder.translateRule(knowableRunesFromAbove, _))

    val (tentativeRuneToCanonicalRune, world) =
      Optimizer.optimize(
        ruleBuilder.builder,
        (inputRule: IRulexAR[Int, RangeS, ILiteralSR, ILookupSR]) => TemplarPuzzler.apply(inputRule))

    val Conclusions(knowableValueRunesS, predictedTypeByRune) =
      PredictorEvaluator.solve(
        evalRange(file, range),
        ruleBuilder.runeSToTentativeRune,
        tentativeRuneToCanonicalRune,
        ruleBuilder.tentativeRuneToType,
        world)
    val localRunesS = allRunesS
    val isTemplate = knowableValueRunesS != allRunesS

    val membersS =
      members.zip(memberRunes).flatMap({
        case (StructMemberP(range, NameP(_, name), variability, _), memberRune) => {
          Vector(StructMemberS(Scout.evalRange(structEnv.file, range), name, variability, memberRune))
        }
        case (StructMethodP(_), memberRune) => {
          // Implement struct methods one day
          Vector.empty
        }
      })

    val maybePredictedType =
      if (isTemplate) {
        if ((identifyingRunesS.toSet -- predictedTypeByRune.keySet).isEmpty) {
          Some(TemplateTypeSR(identifyingRunesS.map(predictedTypeByRune), KindTypeSR))
        } else {
          None
        }
      } else {
        Some(KindTypeSR)
      }

    val weakable = attributesP.exists({ case w @ WeakableP(_) => true case _ => false })
    val attrsS = translateCitizenAttributes(file, attributesP.filter({ case WeakableP(_) => false case _ => true}))

    val runeSToCanonicalRune = ruleBuilder.runeSToTentativeRune.mapValues(tentativeRune => tentativeRuneToCanonicalRune(tentativeRune))

    StructS(
      Scout.evalRange(file, range),
      structName,
      attrsS,
      weakable,
      mutabilityRuneS,
      predictedMutability,
      knowableValueRunesS,
      identifyingRunesS,
      localRunesS,
      maybePredictedType,
      isTemplate,
      world.rules,
      runeSToCanonicalRune,
      membersS)
  }

  def translateCitizenAttributes(file: FileCoordinate, attrsP: Vector[ICitizenAttributeP]): Vector[ICitizenAttributeS] = {
    attrsP.map({
      case ExportP(_) => ExportS(file.packageCoordinate)
      case x => vimpl(x.toString)
    })
  }

  private def scoutInterface(file: FileCoordinate, headP: InterfaceP): InterfaceS = {
    val InterfaceP(range, NameP(_, interfaceHumanName), attributesP, mutabilityPT, maybeIdentifyingRunes, maybeRulesP, internalMethodsP) = headP
    val codeLocation = Scout.evalPos(file, range.begin)
    val interfaceFullName = TopLevelCitizenDeclarationNameS(interfaceHumanName, codeLocation)
    val rulesP = maybeRulesP.toVector.flatMap(_.rules)

    val interfaceRangeS = Scout.evalRange(file, range)

    val identifyingRunes: Vector[IRuneS] =
      maybeIdentifyingRunes
        .toVector.flatMap(_.runes).map(_.name.str)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(rulesP)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = (identifyingRunes ++ runesFromRules).toSet
    val interfaceEnv = Environment(file, None, interfaceFullName, userDeclaredRunes.toSet)

    val ruleState = RuleStateBox(RuleState(interfaceEnv.name, 0))

    val rulesWithoutMutabilityS = RuleScout.translateRulexes(interfaceEnv, ruleState, interfaceEnv.allUserDeclaredRunes(), rulesP)

    val mutabilityST =
      RuleScout.translateTemplex(interfaceEnv, ruleState, interfaceEnv.allUserDeclaredRunes(), mutabilityPT)
    val predictedMutability =
      mutabilityPT match {
        case MutabilityPT(_, mutability) => Some(mutability)
        case _ => None
      }

    val mutabilityRune = ruleState.newImplicitRune()
    val rulesS =
      rulesWithoutMutabilityS :+
      EqualsSR(
        interfaceRangeS,
        RuneSR(interfaceRangeS, mutabilityRune),
        mutabilityST)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(identifyingRunes, rulesS, Vector.empty, None)


    val knowableRunesFromAbove = Set[IRuneS]()

    val ruleBuilder = ScoutRuleBuilder()
    rulesS.foreach(ruleBuilder.translateRule(knowableRunesFromAbove, _))

    val (tentativeRuneToCanonicalRune, world) =
      Optimizer.optimize(
        ruleBuilder.builder,
        (inputRule: IRulexAR[Int, RangeS, ILiteralSR, ILookupSR]) => TemplarPuzzler.apply(inputRule))

    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(Scout.evalRange(file, range), ruleBuilder.runeSToTentativeRune, tentativeRuneToCanonicalRune, ruleBuilder.tentativeRuneToType, world)
    val localRunes = allRunes
    val isTemplate = knowableValueRunes != allRunes.toSet

    val maybePredictedType =
      if (isTemplate) {
        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), KindTypeSR))
        } else {
          None
        }
      } else {
        Some(KindTypeSR)
      }

    val internalMethodsS = internalMethodsP.map(FunctionScout.scoutInterfaceMember(interfaceEnv, _))

    val weakable = attributesP.exists({ case w @ WeakableP(_) => true case _ => false })
    val attrsS = translateCitizenAttributes(file, attributesP.filter({ case WeakableP(_) => false case _ => true}))

    val runeSToCanonicalRune = ruleBuilder.runeSToTentativeRune.mapValues(tentativeRune => tentativeRuneToCanonicalRune(tentativeRune))

    val interfaceS =
      InterfaceS(
        Scout.evalRange(file, range),
        interfaceFullName,
        attrsS,
        weakable,
        mutabilityRune,
        predictedMutability,
        knowableValueRunes,
        identifyingRunes,
        localRunes,
        maybePredictedType,
        isTemplate,
        world.rules,
        runeSToCanonicalRune,
        internalMethodsS)

    interfaceS
  }

  def evalRange(file: FileCoordinate, range: Range): RangeS = {
    RangeS(evalPos(file, range.begin), evalPos(file, range.end))
  }

  def evalPos(file: FileCoordinate, pos: Int): CodeLocationS = {
    CodeLocationS(file, pos)
  }

  def getHumanName(templex: ITemplexPT): String = {
    templex match {
//      case NullablePT(_, inner) => getHumanName(inner)
      case InlinePT(_, inner) => getHumanName(inner)
//      case PermissionedPT(_, permission, inner) => getHumanName(inner)
      case InterpretedPT(_, ownership, permission, inner) => getHumanName(inner)
      case AnonymousRunePT(_) => vwat()
      case NameOrRunePT(NameP(_, name)) => name
      case CallPT(_, template, args) => getHumanName(template)
      case RepeaterSequencePT(_, mutability, variability, size, element) => vwat()
      case ManualSequencePT(_, members) => vwat()
      case IntPT(_, value) => vwat()
      case BoolPT(_, value) => vwat()
      case OwnershipPT(_, ownership) => vwat()
      case MutabilityPT(_, mutability) => vwat()
      case LocationPT(_, location) => vwat()
      case PermissionPT(_, permission) => vwat()
    }
  }
}

class ScoutCompilation(
  packagesToBuild: Vector[PackageCoordinate],
  packageToContentsResolver: IPackageResolver[Map[String, String]]) {
  var parserCompilation = new ParserCompilation(packagesToBuild, packageToContentsResolver)
  var scoutputCache: Option[FileCoordinateMap[ProgramS]] = None

  def getCodeMap(): Result[FileCoordinateMap[String], FailedParse] = parserCompilation.getCodeMap()
  def getParseds(): Result[FileCoordinateMap[(FileP, Vector[(Int, Int)])], FailedParse] = parserCompilation.getParseds()
  def getVpstMap(): Result[FileCoordinateMap[String], FailedParse] = parserCompilation.getVpstMap()

  def getScoutput(): Result[FileCoordinateMap[ProgramS], ICompileErrorS] = {
    scoutputCache match {
      case Some(scoutput) => Ok(scoutput)
      case None => {
        val scoutput =
          parserCompilation.expectParseds().map({ case (fileCoordinate, (code, commentsAndRanges)) =>
            Scout.scoutProgram(fileCoordinate, code) match {
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
    getScoutput().getOrDie()
  }
}
