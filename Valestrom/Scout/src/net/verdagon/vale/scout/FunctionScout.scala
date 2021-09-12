package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.ExpressionScout.NormalResult
import net.verdagon.vale.scout.Scout.noDeclarations
import net.verdagon.vale.scout.patterns._
import net.verdagon.vale.scout.predictor.{Conclusions, PredictorEvaluator}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
//import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
//import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale._

import scala.collection.immutable.{List, Range}

//// Fate is short for Function State. It tracks how many magic params have been used.
//// This is similar to StackFrame, which tracks all the locals that have been declared.
//
//// Maybe we should combine them?
//// As of this writing, we use fate to keep track of magic params, and StackFrame at the
//// block level receives (via return type) declarations from the individual expressions
//// to accumulate them itself.
//case class ScoutFate(private val magicParams: Set[MagicParamNameS]) {
//  def addMagicParam(magicParam: MagicParamNameS): ScoutFate = {
//    ScoutFate(magicParams + magicParam)
//  }
//  def countMagicParams() = magicParams.size
//}
//
//case class ScoutFateBox(var fate: ScoutFate) {
//  def addMagicParam(codeLocation: MagicParamNameS): Unit = {
//    fate = fate.addMagicParam(codeLocation)
//  }
//  def countMagicParams(): Int = fate.countMagicParams()
//}

object FunctionScout {
//  // All closure structs start with this
//  val CLOSURE_STRUCT_NAME = "__Closure:"
//  // In a closure's environment, we also have this. This lets us easily know
//  // what the StructRef for a given closure is.
//  val CLOSURE_STRUCT_ENV_ENTRY_NAME = "__Closure"
//  // Name of anonymous substructs. They're more identified by their CodeLocation though.
//  val ANONYMOUS_SUBSTRUCT_NAME = "__AnonymousSubstruct"

  def scoutTopLevelFunction(file: FileCoordinate, functionP: FunctionP): FunctionS = {
    val FunctionP(
      range,
      FunctionHeaderP(_,
        Some(NameP(_, codeName)),
        attributes,
        userSpecifiedIdentifyingRuneNames,
        templateRulesP,
        paramsP,
        FunctionReturnP(retRange, maybeInferRet, maybeRetType)),
      maybeBody0
    ) = functionP
    val codeLocation = Scout.evalPos(file, range.begin)
    val name = FunctionNameS(codeName, codeLocation)

    val lidb = new LocationInDenizenBuilder(Vector())

    val userSpecifiedIdentifyingRunes =
      userSpecifiedIdentifyingRuneNames
        .toVector
        .flatMap(_.runes)
        // Filter out any regions, we dont do those yet
        .filter({ case IdentifyingRuneP(_, _, attributes) => !attributes.exists({ case TypeRuneAttributeP(_, RegionTypePR) => true case _ => false }) })
        .map({ case IdentifyingRuneP(_, NameP(_, identifyingRuneName), _) => CodeRuneS(identifyingRuneName) })

    // See: Must Scan For Declared Runes First (MSFDRF)
    val userRunesFromRules =
      templateRulesP
        .toVector
        .flatMap(rules => RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(rules.rules))
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = (userSpecifiedIdentifyingRunes ++ userRunesFromRules).distinct

    val functionEnv = FunctionEnvironment(file, name, None, userDeclaredRunes.toSet, paramsP.size)

    val ruleBuilder = ArrayBuffer[IRulexSR]()
    val runeToExplicitType = mutable.HashMap[IRuneS, ITypeSR]()
    RuleScout.translateRulexes(
      functionEnv,
      lidb.child(),
      ruleBuilder,
      runeToExplicitType,
      templateRulesP.toVector.flatMap(_.rules))

    val myStackFrameWithoutParams = StackFrame(file, name, functionEnv, None, noDeclarations)

    val patternsP = paramsP.toVector.flatMap(_.patterns)
    val explicitParamsPatterns1 =
      PatternScout.scoutPatterns(
        myStackFrameWithoutParams, lidb.child(), ruleBuilder, runeToExplicitType, patternsP)

    val explicitParams1 = explicitParamsPatterns1.map(ParameterS)
    val captureDeclarations =
      explicitParams1
        .map(explicitParam1 => VariableDeclarations(PatternScout.getParameterCaptures(explicitParam1.pattern)))
        .foldLeft(noDeclarations)(_ ++ _)

    val maybeRetCoordRune =
      (maybeInferRet, maybeRetType) match {
        case (None, None) => {
          // If nothing's present, assume void
          val rangeS = Scout.evalRange(file, retRange)
          val rune = ImplicitRuneS(lidb.child().consume())
          runeToExplicitType.put(rune, CoordTypeSR)
          ruleBuilder += LookupSR(rangeS, rune, ImpreciseNameSN(CodeTypeNameS("void")))
          Some(rune)
        }
        case (Some(_), None) => None // Infer the return
        case (None, Some(retTypePT)) => {
          PatternScout.translateMaybeTypeIntoMaybeRune(
            functionEnv,
            lidb,
            Scout.evalRange(file, retRange),
            ruleBuilder,
            runeToExplicitType,
            Some(retTypePT),
            CoordTypePR,
            // The rune should be on the right, see DCRC option A.
            false)
        }
        case (Some(_), Some(_)) => throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(file, range), "Can't have return type and infer-ret at the same time"))
      }

    //    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val body1 =
      if (attributes.collectFirst({ case AbstractAttributeP(_) => }).nonEmpty) {
        AbstractBodyS
      } else if (attributes.collectFirst({ case ExternAttributeP(_) => }).nonEmpty) {
        if (maybeBody0.nonEmpty) {
          throw CompileErrorExceptionS(ExternHasBody(Scout.evalRange(file, range)))
        }
        ExternBodyS
      } else if (attributes.collectFirst({ case BuiltinAttributeP(_, _) => }).nonEmpty) {
        GeneratedBodyS(attributes.collectFirst({ case BuiltinAttributeP(_, generatorId) => generatorId}).head.str)
      } else {
        vassert(maybeBody0.nonEmpty)
        val (body1, _, Vector()) =
          scoutBody(
            functionEnv,
            None,
            lidb.child(),
            maybeBody0.get,
            // We hand these into scoutBody instead of assembling a StackFrame on our own because we want
            // StackFrame's to be made in one place, where we can centralize the logic for tracking variable
            // uses and so on.
            captureDeclarations)
        vassert(body1.closuredNames.isEmpty)
        CodeBodyS(body1)
      }

//    val (tentativeRuneToCanonicalRune, world) =
//      Optimizer.optimize(
//        ruleBuilder.builder,
//        (inputRule: IRulexAR[Int, RangeS, IValueSR, IValueSR]) => TemplarPuzzler.apply(inputRule))

//    val allRunes =
//      PredictorEvaluator.getAllRunes(
//        userSpecifiedIdentifyingRunes,
//        rulesS,
//        explicitParams1.map(_.pattern),
//        maybeRetCoordRune)
//    val Conclusions(knowableValueRunes, predictedTypeByRune) =
//      PredictorEvaluator.solve(
//        Scout.evalRange(file, range),
//        ruleBuilder.runeSToTentativeRune,
//        tentativeRuneToCanonicalRune,
//        ruleBuilder.tentativeRuneToType,
//        world)

//    val localRunes = allRunes
//    val unknowableRunes = allRunes -- knowableValueRunes

    // This cant be:
    //   val identifyingRunes =
    //     userSpecifiedIdentifyingRunes ++ (unknowableRunes -- userSpecifiedIdentifyingRunes)
    // because for example if we had:
    //   fn moo<T>(m &T) { m.hp }
    // then userSpecifiedIdentifyingRunes would be
    //   CodeRuneS("T")
    // and unknowableRunes would be
    //   Set(CodeRuneS("T"), ImplicitRuneS(0), ImplicitRuneS(1))
    // and we'd end up with identifyingRunes as
    //   Vector(CodeRuneS("T"), ImplicitRuneS(0), ImplicitRuneS(1))
    // So, what we instead want is like... the original causes of unknowable runes.
    // I think thats just user specified ones, and implicit template runes from params,
    // and magic param runes.
//    val topLevelImplicitRunesS =
//      paramsP.zip(explicitParamsPatterns1).flatMap({
//        case (paramP, explicitParamPatternS) => {
//          if (paramP.templex.isEmpty) {
//            Vector(explicitParamPatternS.coordRune)
//          } else {
//            Vector.empty
//          }
//        }
//      })

//    val identifyingParamCoordRunes =
//      explicitParamsPatterns1
//        .map(_.coordRune)
//        .filter(!knowableValueRunes.contains(_))
//        .filter(!userSpecifiedIdentifyingRunes.contains(_))
//    val identifyingRunes = userSpecifiedIdentifyingRunes ++ identifyingParamCoordRunes
//
//    val isTemplate = identifyingRunes.nonEmpty
//
//    val maybePredictedType =
//      if (isTemplate) {
//        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
//          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
//        } else {
//          None
//        }
//      } else {
//        Some(FunctionTypeSR)
//      }

    val attrsS = translateFunctionAttributes(file, attributes.filter({ case AbstractAttributeP(_) => false case _ => true}))

//    val runeSToCanonicalRune = ruleBuilder.runeSToTentativeRune.mapValues(tentativeRune => tentativeRuneToCanonicalRune(tentativeRune))

    FunctionS(
      Scout.evalRange(file, range),
      name,
      attrsS,
      userSpecifiedIdentifyingRunes,
      runeToExplicitType.toMap,
//      knowableValueRunes,
//      identifyingRunes,
//      localRunes,
//      maybePredictedType,
      explicitParams1,
      maybeRetCoordRune,
//      isTemplate,
      ruleBuilder.toArray,
//      runeSToCanonicalRune,
      body1)
  }

  def translateFunctionAttributes(file: FileCoordinate, attrsP: Vector[IFunctionAttributeP]): Vector[IFunctionAttributeS] = {
    attrsP.map({
      case AbstractAttributeP(_) => vwat() // Should have been filtered out, templar cares about abstract directly
      case ExportAttributeP(_) => ExportS(file.packageCoordinate)
      case ExternAttributeP(_) => ExternS(file.packageCoordinate)
      case PureAttributeP(_) => PureS
      case BuiltinAttributeP(_, generatorName) => BuiltinS(generatorName.str)
      case x => vimpl(x.toString)
    })
  }

  def scoutLambda(
      parentStackFrame: StackFrame,
      lambdaFunction0: FunctionP):
  (FunctionS, VariableUses) = {
    val FunctionP(range,
      FunctionHeaderP(_,
        _, attrsP, userSpecifiedIdentifyingRuneNames, None, paramsP, FunctionReturnP(retRange, maybeInferRet, maybeRetType)),
      Some(body0)) = lambdaFunction0;
    val codeLocation = Scout.evalPos(parentStackFrame.file, range.begin)

    vcurious(userSpecifiedIdentifyingRuneNames.isEmpty)

    val lambdaName = LambdaNameS(/*parentStackFrame.name,*/ codeLocation)
    // Every lambda has a closure as its first arg, even if its empty
    val closureStructName = LambdaStructNameS(lambdaName)

    val functionEnv =
      FunctionEnvironment(
        parentStackFrame.file,
        lambdaName,
        Some(parentStackFrame.parentEnv),
        Set(),
        paramsP.size)


    val myStackFrameWithoutParams = StackFrame(parentStackFrame.file, lambdaName, functionEnv, None, noDeclarations)

    val lidb = new LocationInDenizenBuilder(Vector())

    val ruleBuilder = ArrayBuffer[IRulexSR]()
    val runeToExplicitType = mutable.HashMap[IRuneS, ITypeSR]()
    val explicitParamPatterns1 =
      PatternScout.scoutPatterns(
        myStackFrameWithoutParams,
        lidb.child(),
        ruleBuilder,
        runeToExplicitType,
        paramsP.toVector.flatMap(_.patterns));
    val explicitParams1 = explicitParamPatterns1.map(ParameterS)
//    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val closureParamName = ClosureParamNameS()

    val closureDeclaration =
      VariableDeclarations(Vector(VariableDeclaration(closureParamName)))

    val paramDeclarations =
      explicitParams1.map(_.pattern)
        .map(pattern1 => VariableDeclarations(PatternScout.getParameterCaptures(pattern1)))
        .foldLeft(closureDeclaration)(_ ++ _)

//    val functionBodyStackFrame =
//      StackFrame(
//        parentStackFrame.file,
//        lambdaName,
//        functionBodyEnv,
//        Some(parentStackFrame),
//        // One might expect to hand in paramDeclarations here, but we don't because we want them to be
//        // not in this block, but
//        noDeclarations)

    val (body1, variableUses, lambdaMagicParamNames) =
      scoutBody(
        functionEnv,
        Some(parentStackFrame),
        lidb.child(),
        body0,
        paramDeclarations)

    if (lambdaMagicParamNames.nonEmpty && (explicitParams1.nonEmpty)) {
      throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(parentStackFrame.file, range), "Cant have a lambda with _ and params"))
    }

//    val closurePatternId = fate.nextPatternNumber();

    val closureParamRange = Scout.evalRange(parentStackFrame.file, range)
    val closureStructRune = ImplicitRuneS(lidb.child().consume())
    ruleBuilder +=
      LookupSR(
        closureParamRange, closureStructRune, AbsoluteNameSN(closureStructName))
    val closureParamTypeRune = ImplicitRuneS(lidb.child().consume())
    ruleBuilder +=
      AugmentSR(
        closureParamRange,
        closureParamTypeRune,
        Vector(OwnershipLiteralSL(ConstraintP),PermissionLiteralSL(ReadwriteP)),
        closureStructRune)

    val closureParamS =
      ParameterS(
        AtomSP(
          closureParamRange,
          Some(CaptureS(closureParamName)),None,closureParamTypeRune,None))

    val magicParams =
        lambdaMagicParamNames.map({
          case mpn @ MagicParamNameS(codeLocation) => {
            val magicParamRange = RangeS(codeLocation, codeLocation)
            val magicParamRune = MagicParamRuneS(lidb.child().consume())
            runeToExplicitType.put(magicParamRune,CoordTypeSR)
            val paramS =
              ParameterS(
                AtomSP(
                  magicParamRange,
                  Some(CaptureS(mpn)),None,magicParamRune,None))
            paramS
          }
        })

    // Lambdas identifying runes are determined by their magic params.
    // See: Lambdas Dont Need Explicit Identifying Runes (LDNEIR)
    val identifyingRunes = magicParams.map(_.pattern.coordRune).distinct

    val totalParams = Vector(closureParamS) ++ explicitParams1 ++ magicParams;

    val maybeRetCoordRune =
      (maybeInferRet, maybeRetType) match {
        case (_, None) => None // Infer the return
        case (None, Some(retTypePT)) => {
          PatternScout.translateMaybeTypeIntoMaybeRune(
            functionEnv,
            lidb.child(),
            Scout.evalRange(myStackFrameWithoutParams.file, retRange),
            ruleBuilder,
            runeToExplicitType,
            Some(retTypePT),
            CoordTypePR)
        }
        case (Some(_), Some(_)) => throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(parentStackFrame.file, range), "Can't have return type and infer-ret at the same time"))
      }
//
//    val allRunes =
//      PredictorEvaluator.getAllRunes(
//        userSpecifiedAndMagicParamRunes,
//        rulesS,
//        explicitParams1.map(_.pattern),
//        maybeRetCoordRune)
//    val Conclusions(knowableValueRunes, predictedTypeByRune) =
//      PredictorEvaluator.solve(
//        parentStackFrame.parentEnv.allUserDeclaredRunes(),
//        rulesS,
//        explicitParams1.map(_.pattern),
//        Scout.evalRange(myStackFrame.file, range))
//
//    val localRunes = allRunes -- myStackFrame.parentEnv.allUserDeclaredRunes()
//    val unknowableRunes = allRunes -- knowableValueRunes
//

    // This cant be:
    //   val identifyingRunes =
    //     userSpecifiedIdentifyingRunes ++ (unknowableRunes -- userSpecifiedIdentifyingRunes)
    // because for example if we had:
    //   fn moo<T>(m &T) { m.hp }
    // then userSpecifiedIdentifyingRunes would be
    //   CodeRuneS("T")
    // and unknowableRunes would be
    //   Set(CodeRuneS("T"), ImplicitRuneS(0), ImplicitRuneS(1))
    // and we'd end up with identifyingRunes as
    //   Vector(CodeRuneS("T"), ImplicitRuneS(0), ImplicitRuneS(1))
    // So, what we instead want is like... the original causes of unknowable runes.
    // I think thats just user specified ones, and implicit template runes from params,
    // and magic param runes.
//    val topLevelImplicitRunesS =
//      paramsP.zip(explicitParams1.map(_.pattern)).flatMap({
//        case (paramP, explicitParamPatternS) => {
//          if (paramP.templex.isEmpty) {
//            Vector(explicitParamPatternS.coordRune)
//          } else {
//            Vector.empty
//          }
//        }
//      })

//    val identifyingParamCoordRunes =
//      explicitParamPatterns1
//        .map(_.coordRune)
//        .filter(!knowableValueRunes.contains(_))
//        .filter(!userSpecifiedIdentifyingRunes.contains(_))
//    val identifyingRunes = userSpecifiedIdentifyingRunes ++ magicParams.map(_.pattern.coordRune) ++ identifyingParamCoordRunes
//
//    val isTemplate = identifyingRunes.nonEmpty
//
//    val maybePredictedType =
//      if (isTemplate) {
//        if (identifyingRunes.isEmpty) {
//          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
//        } else {
//          None
//        }
//      } else {
//        Some(FunctionTypeSR)
//      }

    val function1 =
      FunctionS(
        Scout.evalRange(parentStackFrame.file, range),
        lambdaName,
        translateFunctionAttributes(parentStackFrame.file, attrsP),
        identifyingRunes,
        runeToExplicitType.toMap,
//        knowableValueRunes,
//        identifyingRunes,b
//        localRunes,
//        maybePredictedType,
        totalParams,
        maybeRetCoordRune,
//        isTemplate,
        ruleBuilder.toArray,
        CodeBodyS(body1))
    (function1, variableUses)
  }

  // Returns:
  // - Body.
  // - Uses of parent variables.
  // - Magic params made/used inside.
  private def scoutBody(
    functionBodyEnv: FunctionEnvironment,
    // This might be the block containing the lambda that we're evaluating now.
    parentStackFrame: Option[StackFrame],
    lidb: LocationInDenizenBuilder,
    body0: BlockPE,
    initialDeclarations: VariableDeclarations):
  (BodySE, VariableUses, Vector[MagicParamNameS]) = {
    // There's an interesting consequence of calling this function here...
    // If we have a lone lookup node, like "m = Marine(); m;" then that
    // 'm' will be turned into an expression, which means that's how it's
    // destroyed. So, thats how we destroy things before their time.
    val (NormalResult(_, block1), selfUses, childUses) =
      ExpressionScout.scoutBlock(
        functionBodyEnv,
        parentStackFrame,
        lidb.child(),
        body0,
        initialDeclarations)

    vcurious(
      childUses.uses.map(_.name).collect({ case mpn @ MagicParamNameS(_) => mpn }).isEmpty)
    val magicParamNames =
      selfUses.uses.map(_.name).collect({ case mpn @ MagicParamNameS(_) => mpn })
    val magicParamVars = magicParamNames.map(n => VariableDeclaration(n))

    val magicParamLocals =
      magicParamVars.map({ declared =>
        LocalS(
          declared.name,
          selfUses.isBorrowed(declared.name),
          selfUses.isMoved(declared.name),
          selfUses.isMutated(declared.name),
          childUses.isBorrowed(declared.name),
          childUses.isMoved(declared.name),
          childUses.isMutated(declared.name))
      })
    val block1WithParamLocals = BlockSE(Scout.evalRange(functionBodyEnv.file, body0.range), block1.locals ++ magicParamLocals, block1.exprs)

    val allUses =
      selfUses.combine(childUses, {
        case (None, other) => other
        case (other, None) => other
        case (Some(NotUsed), other) => other
        case (other, Some(NotUsed)) => other
        case (Some(Used), Some(Used)) => Some(Used)
      })

    // We're trying to figure out which variables from parent environments
    // we're using.
    // This is so we can remember in BodySE which variables we're using from
    // containing functions (so we can define the struct which we take in as
    // an implicit first parameter), and also so we can report those upward
    // so if we're using variables from our grandparent, our parent can know
    // that it needs to capture them for us.
    val usesOfParentVariables =
      allUses.uses.filter(use => {
        if (block1WithParamLocals.locals.exists(_.varName == use.name)) {
          // This is a use of a variable declared in this function.
          false
        } else {
          use.name match {
            case MagicParamNameS(_) => {
              // We're using a magic param, which we'll have in this function's params.
              false
            }
            case _ => {
              // This is a use of a variable from somewhere above.
              true
            }
          }
        }
      })

    val bodySE = BodySE(Scout.evalRange(functionBodyEnv.file, body0.range), usesOfParentVariables.map(_.name), block1WithParamLocals)
    (bodySE, VariableUses(usesOfParentVariables), magicParamNames)
  }

  def scoutInterfaceMember(interfaceEnv: Environment, functionP: FunctionP): FunctionS = {
    val FunctionP(
      range,
      FunctionHeaderP(_,
        Some(NameP(_, codeName)),
        attrsP,
        userSpecifiedIdentifyingRuneNames,
        templateRulesP,
        maybeParamsP,
        FunctionReturnP(retRange, maybeInferRet, maybeRetType)),
      None) = functionP;
    val retRangeS = Scout.evalRange(interfaceEnv.file, retRange)

    maybeParamsP match {
      case None =>
      case Some(paramsP) => {
        if (!paramsP.patterns.exists(_.virtuality == Some(AbstractP))) {
          throw CompileErrorExceptionS(InterfaceMethodNeedsSelf(Scout.evalRange(interfaceEnv.file, range)))
        }
      }
    }

    val codeLocation = Scout.evalPos(interfaceEnv.file, range.begin)
    val funcName = FunctionNameS(codeName, codeLocation)
    val userSpecifiedIdentifyingRunes: Vector[IRuneS] =
      userSpecifiedIdentifyingRuneNames
          .toVector
        .flatMap(_.runes)
        // Filter out any regions, we dont do those yet
        .filter({ case IdentifyingRuneP(_, _, attributes) => !attributes.exists({ case TypeRuneAttributeP(_, RegionTypePR) => true case _ => false }) })
        .map({ case IdentifyingRuneP(_, NameP(_, identifyingRuneName), _) => CodeRuneS(identifyingRuneName) })

    // See: Must Scan For Declared Runes First (MSFDRF)
    val userRunesFromRules =
      templateRulesP
        .toVector
        .flatMap(rules => RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(rules.rules))
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = (userSpecifiedIdentifyingRunes ++ userRunesFromRules).distinct

    val lidb = new LocationInDenizenBuilder(Vector())

    val ruleBuilder = ArrayBuffer[IRulexSR]()
    val runeToExplicitType = mutable.HashMap[IRuneS, ITypeSR]()

    RuleScout.translateRulexes(interfaceEnv, lidb.child(), ruleBuilder, runeToExplicitType, templateRulesP.toVector.flatMap(_.rules))

    val functionEnv = FunctionEnvironment(interfaceEnv.file, funcName, Some(interfaceEnv), userDeclaredRunes.toSet, maybeParamsP.size)
    val myStackFrame = StackFrame(interfaceEnv.file, funcName, functionEnv, None, noDeclarations)
    val patternsS =
      PatternScout.scoutPatterns(myStackFrame, lidb.child(), ruleBuilder, runeToExplicitType, maybeParamsP.toVector.flatMap(_.patterns))

    val paramsS = patternsS.map(ParameterS)

    val maybeReturnRune =
      (maybeInferRet, maybeRetType) match {
        case (None, None) => {
          // If nothing's present, assume void
          val rune = ImplicitRuneS(lidb.child().consume())
          runeToExplicitType.put(rune, CoordTypeSR)
          ruleBuilder += LookupSR(retRangeS, rune, ImpreciseNameSN(CodeTypeNameS("void")))
          Some(rune)
        }
        case (Some(_), None) => {
          throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(myStackFrame.file, range), "Can't infer the return type of an interface method!"))
        }
        case (None, Some(retTypePT)) => {
          PatternScout.translateMaybeTypeIntoMaybeRune(
            interfaceEnv,
            lidb.child(),
            Scout.evalRange(myStackFrame.file, retRange),
            ruleBuilder,
            runeToExplicitType,
            Some(retTypePT),
            CoordTypePR)
        }
        case (Some(_), Some(_)) => throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(myStackFrame.file, range), "Can't have return type and infer-ret at the same time"))
      }

//    val allRunes =
//      PredictorEvaluator.getAllRunes(
//        userSpecifiedIdentifyingRunes,
//        rulesS,
//        paramsS.map(_.pattern),
//        maybeReturnRune)
//    val Conclusions(knowableValueRunes, predictedTypeByRune) =
//      PredictorEvaluator.solve(
//        interfaceEnv.allUserDeclaredRunes(),
//        rulesS,
//        paramsS.map(_.pattern),
//        Scout.evalRange(myStackFrame.file, range))
//
//    val localRunes = allRunes -- interfaceEnv.allUserDeclaredRunes()
//    val unknowableRunes = allRunes -- knowableValueRunes

//    val identifyingParamCoordRunes =
//      patternsS
//        .map(_.coordRune)
//        .filter(!knowableValueRunes.contains(_))
//        .filter(!userSpecifiedIdentifyingRunes.contains(_))
//    val identifyingRunes = userSpecifiedIdentifyingRunes ++ identifyingParamCoordRunes
//    val isTemplate = identifyingRunes.nonEmpty
//    vassert(!isTemplate) // interface members cant be templates

//    val maybePredictedType =
//      if (isTemplate) {
//        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
//          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
//        } else {
//          None
//        }
//      } else {
//        Some(FunctionTypeSR)
//      }

    if (attrsP.collect({ case AbstractAttributeP(_) => true  }).nonEmpty) {
      throw CompileErrorExceptionS(RangedInternalErrorS(Scout.evalRange(interfaceEnv.file, range), "Dont need abstract here"))
    }

    FunctionS(
      Scout.evalRange(functionEnv.file, range),
      funcName,
      translateFunctionAttributes(functionEnv.file, attrsP),
//      knowableValueRunes,
      userSpecifiedIdentifyingRunes,
      runeToExplicitType.toMap,
//      localRunes,
//      maybePredictedType,
      paramsS,
      maybeReturnRune,
//      isTemplate,
      ruleBuilder.toArray,
      AbstractBodyS)
  }
}
