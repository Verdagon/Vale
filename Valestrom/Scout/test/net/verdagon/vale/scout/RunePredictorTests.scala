package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules.{EqualsSR, _}
import net.verdagon.vale.scout.predictor.PredictorEvaluator
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class RunePredictorTests extends FunSuite with Matchers {
  val tz = RangeS.testZero

  test("Predict doesnt crash for simple templex") {
    vimpl()
//    val knowableRunesFromAbove = Set[IRuneS]()
//
////    val rules =
////      LookupAR[IRuneS, RangeS, ILiteralSR, ILookupSR](tz,
////        ImplicitRuneS(FunctionNameS("f", CodeLocationS.testZero), 0),
////        NameLookupSR(CodeTypeNameS("int")))
//
//    val (tentativeRuneToCanonicalRune, world) =
//      Optimizer.optimize(
//        ruleBuilder.builder,
//        (inputRule: IRulexAR[Int, RangeS, IValueSR, IValueSR]) => TemplarPuzzler.apply(inputRule))
//
//    val conclusions =
//      PredictorEvaluator.solve(tz, ruleBuilder.runeSToTentativeRune, tentativeRuneToCanonicalRune, ruleBuilder.tentativeRuneToType, world)
//    conclusions shouldEqual Conclusions(Set(), Map())
  }

  test("Can know rune from simple equals") {
//    val conclusions =
//      PredictorEvaluator.solve(
//        Set(),
//        Vector(
//          EqualsSR(tz,RuneSR(tz,CodeRuneS("T")), NameSR(tz, CodeTypeNameS("int")))),
//        Vector.empty, tz)
//    conclusions shouldEqual Conclusions(Set(CodeRuneS("T")), Map())
    vimpl()
  }

  test("Predict for simple equals 2") {
    vimpl()
//    val conclusions =
//      PredictorEvaluator.solve(
//        Set(),
//        Vector(
//          TypedSR(tz,CodeRuneS("Z"),CoordTypeSR),
//          EqualsSR(tz,
//            RuneSR(tz,CodeRuneS("Z")),
//            CallSR(tz,NameSR(tz, CodeTypeNameS("MyOption")),Vector(InterpretedSR(tz,ShareP,ReadonlyP, NameSR(tz, CodeTypeNameS("int"))))))),
//        Vector.empty, tz)
//    conclusions shouldEqual Conclusions(Set(CodeRuneS("Z")), Map(CodeRuneS("Z") -> CoordTypeSR))
  }

  test("Predict doesn't know value from Or rule") {
    vimpl()
//    val tRune = CodeRuneS("T")
//    val conclusions =
//      PredictorEvaluator.solve(
//        Set(),
//        Vector(
//          EqualsSR(tz,
//            TypedSR(tz, tRune, OwnershipTypeSR),
//            OrSR(tz,Vector(OwnershipSR(tz,OwnP), OwnershipSR(tz,ShareP))))),
//        Vector.empty, tz)
//    conclusions shouldEqual
//      Conclusions(Set(), Map(tRune -> OwnershipTypeSR))
  }

  test("Predict doesnt know T from components with anonymous kind") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//    val conclusions =
//    PredictorEvaluator.solve(
//      Set(),
//      Vector(
//        ComponentsSR(
//          RangeS.internal(-82),
//          TypedSR(tz,CodeRuneS("T"),CoordTypeSR),
//          Vector(
//            OrSR(tz,Vector(OwnershipSR(tz,OwnP), OwnershipSR(tz,ShareP))),
//            PermissionSR(tz,ReadonlyP),
//            BuiltinCallSR(tz,"passThroughIfConcrete",Vector(RuneSR(tz,CodeRuneS("Z")))))),
//        EqualsSR(tz,TypedSR(tz,CodeRuneS("V"),CoordTypeSR),BuiltinCallSR(tz,"toRef",Vector(NameSR(tz, CodeTypeNameS("void")))))),
//      Vector.empty, tz)
//    conclusions shouldEqual
//      Conclusions(
//        Set(CodeRuneS("V")),
//        Map(
//          CodeRuneS("T") -> CoordTypeSR,
//          CodeRuneS("V") -> CoordTypeSR))
  }

  test("Predict returns true for array sequence") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//    val conclusions =
//    PredictorEvaluator.solve(
//      Set(),
//      Vector(
//        TypedSR(tz,CodeRuneS("Z"),CoordTypeSR),
//        EqualsSR(tz,
//          RuneSR(tz,CodeRuneS("Z")),
//          RepeaterSequenceSR(tz,MutabilitySR(tz,MutableP), VariabilitySR(tz,VaryingP), IntSR(tz,5),InterpretedSR(tz,ShareP,ReadonlyP,NameSR(tz, CodeTypeNameS("int")))))),
//      Vector.empty, tz)
//    conclusions shouldEqual Conclusions(Set(CodeRuneS("Z")), Map(CodeRuneS("Z") -> CoordTypeSR))
  }

  test("Predict for idestructor for interface") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//    val conclusions =
//    PredictorEvaluator.solve(
//      Set(),
//      Vector(
//        ComponentsSR(
//          RangeS.internal(-83),
//          TypedSR(tz,CodeRuneS("T"),CoordTypeSR),
//          Vector(
//            OrSR(tz,Vector(OwnershipSR(tz,OwnP), OwnershipSR(tz,ShareP))),
//            PermissionSR(tz, ReadonlyP),
//            BuiltinCallSR(tz,"passThroughIfInterface",Vector(RuneSR(tz,CodeRuneS("Z")))))),
//        EqualsSR(tz,TypedSR(tz,CodeRuneS("V"),CoordTypeSR),BuiltinCallSR(tz,"toRef",Vector(NameSR(tz, CodeTypeNameS("void")))))),
//      Vector.empty, tz)
//    conclusions shouldEqual
//      Conclusions(
//        Set(CodeRuneS("V")),
//        Map(
//          CodeRuneS("T") -> CoordTypeSR,
//          CodeRuneS("V") -> CoordTypeSR))

  }

  test("Predict for idestructor for struct") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//    val conclusions =
//    PredictorEvaluator.solve(
//      Set(),
//      Vector(
//        ComponentsSR(
//          RangeS.internal(-84),
//          TypedSR(tz,CodeRuneS("T"),CoordTypeSR),
//          Vector(
//            OrSR(tz,Vector(OwnershipSR(tz,OwnP), OwnershipSR(tz,ShareP))),
//            PermissionSR(tz, ReadonlyP),
//            BuiltinCallSR(tz,"passThroughIfStruct",Vector(RuneSR(tz,CodeRuneS("Z")))))),
//        BuiltinCallSR(tz,"passThroughIfInterface",Vector(RuneSR(tz,CodeRuneS("I"))))),
//      Vector.empty, tz)
//    conclusions shouldEqual Conclusions(Set(), Map(CodeRuneS("T") -> CoordTypeSR))
  }

  // See MKKRFA.
  test("Predict runes from above") {
    vimpl()
//    val conclusions =
//      PredictorEvaluator.solve(
//        Set(CodeRuneS("P1"), CodeRuneS("R")),
//        Vector(
//          EqualsSR(tz,
//            TypedSR(tz,CodeRuneS("Z"),CoordTypeSR),
//            InterpretedSR(tz,
//              ConstraintP,
//              ReadonlyP,
//              CallSR(tz,
//                NameSR(tz, CodeTypeNameS("MyIFunction1")),
//                Vector(
//                  RuneSR(tz,CodeRuneS("P1")),
//                  RuneSR(tz,CodeRuneS("R")))))),
//          TypedSR(tz,CodeRuneS("P1"),CoordTypeSR),
//          TypedSR(tz,CodeRuneS("R"),CoordTypeSR)),
//        Vector.empty, tz)
//    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("P1")))
//    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("R")))
//    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("Z")))
  }
}
