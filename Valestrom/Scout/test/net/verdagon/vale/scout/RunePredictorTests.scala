package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules.{EqualsSR, _}
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class RunePredictorTests extends FunSuite with Matchers {
  test("Predict doesnt crash for simple templex") {
    val conclusions =
      PredictorEvaluator.solve(Set(), Vector(NameSR(RangeS.testZero, CodeTypeNameS("int"))), Vector.empty)
    conclusions shouldEqual Conclusions(Set(), Map())
  }

  test("Can know rune from simple equals") {
    val conclusions =
      PredictorEvaluator.solve(
        Set(),
        Vector(
          EqualsSR(RangeS.testZero,RuneSR(RangeS.testZero,CodeRuneS("T")), NameSR(RangeS.testZero, CodeTypeNameS("int")))),
        Vector.empty)
    conclusions shouldEqual Conclusions(Set(CodeRuneS("T")), Map())
  }

  test("Predict for simple equals 2") {
    val conclusions =
      PredictorEvaluator.solve(
        Set(),
        Vector(
          TypedSR(RangeS.testZero,CodeRuneS("Z"),CoordTypeSR),
          EqualsSR(RangeS.testZero,
            RuneSR(RangeS.testZero,CodeRuneS("Z")),
            CallSR(RangeS.testZero,NameSR(RangeS.testZero, CodeTypeNameS("MyOption")),Vector(InterpretedSR(RangeS.testZero,ShareP,ReadonlyP, NameSR(RangeS.testZero, CodeTypeNameS("int"))))))),
        Vector.empty)
    conclusions shouldEqual Conclusions(Set(CodeRuneS("Z")), Map(CodeRuneS("Z") -> CoordTypeSR))
  }

  test("Predict doesn't know value from Or rule") {
    val tRune = CodeRuneS("T")
    val aRune = CodeRuneS("A")
    val bRune = CodeRuneS("B")
    val conclusions =
      PredictorEvaluator.solve(
        Set(),
        Vector(
          ComponentsSR(
            RangeS.internal(-81),
            TypedSR(RangeS.testZero,tRune,CoordTypeSR),
            Vector(
              OrSR(RangeS.testZero,Vector(OwnershipSR(RangeS.testZero,OwnP), OwnershipSR(RangeS.testZero,ShareP))),
              // Not exactly valid but itll do for this test
              OrSR(RangeS.testZero,Vector(TypedSR(RangeS.testZero,aRune,KindTypeSR), TypedSR(RangeS.testZero,bRune,CoordTypeSR)))))),
        Vector.empty)
    conclusions shouldEqual
      Conclusions(Set(), Map(tRune -> CoordTypeSR, aRune -> KindTypeSR, bRune -> CoordTypeSR))
  }

  test("Predict doesnt know T from components with anonymous kind") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      Set(),
      Vector(
        ComponentsSR(
          RangeS.internal(-82),
          TypedSR(RangeS.testZero,CodeRuneS("T"),CoordTypeSR),
          Vector(
            OrSR(RangeS.testZero,Vector(OwnershipSR(RangeS.testZero,OwnP), OwnershipSR(RangeS.testZero,ShareP))),
            BuiltinCallSR(RangeS.testZero,"passThroughIfConcrete",Vector(RuneSR(RangeS.testZero,CodeRuneS("Z")))))),
        EqualsSR(RangeS.testZero,TypedSR(RangeS.testZero,CodeRuneS("V"),CoordTypeSR),BuiltinCallSR(RangeS.testZero,"toRef",Vector(NameSR(RangeS.testZero, CodeTypeNameS("void")))))),
      Vector.empty)
    conclusions shouldEqual
      Conclusions(
        Set(CodeRuneS("V")),
        Map(
          CodeRuneS("T") -> CoordTypeSR,
          CodeRuneS("V") -> CoordTypeSR))
  }

  test("Predict returns true for array sequence") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      Set(),
      Vector(
        TypedSR(RangeS.testZero,CodeRuneS("Z"),CoordTypeSR),
        EqualsSR(RangeS.testZero,
          RuneSR(RangeS.testZero,CodeRuneS("Z")),
          RepeaterSequenceSR(RangeS.testZero,MutabilitySR(RangeS.testZero,MutableP), VariabilitySR(RangeS.testZero,VaryingP), IntSR(RangeS.testZero,5),InterpretedSR(RangeS.testZero,ShareP,ReadonlyP,NameSR(RangeS.testZero, CodeTypeNameS("int")))))),
      Vector.empty)
    conclusions shouldEqual Conclusions(Set(CodeRuneS("Z")), Map(CodeRuneS("Z") -> CoordTypeSR))
  }

  test("Predict for idestructor for interface") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      Set(),
      Vector(
        ComponentsSR(
          RangeS.internal(-83),
          TypedSR(RangeS.testZero,CodeRuneS("T"),CoordTypeSR),
          Vector(
            OrSR(RangeS.testZero,Vector(OwnershipSR(RangeS.testZero,OwnP), OwnershipSR(RangeS.testZero,ShareP))),
            BuiltinCallSR(RangeS.testZero,"passThroughIfInterface",Vector(RuneSR(RangeS.testZero,CodeRuneS("Z")))))),
        EqualsSR(RangeS.testZero,TypedSR(RangeS.testZero,CodeRuneS("V"),CoordTypeSR),BuiltinCallSR(RangeS.testZero,"toRef",Vector(NameSR(RangeS.testZero, CodeTypeNameS("void")))))),
      Vector.empty)
    conclusions shouldEqual
      Conclusions(
        Set(CodeRuneS("V")),
        Map(
          CodeRuneS("T") -> CoordTypeSR,
          CodeRuneS("V") -> CoordTypeSR))

  }

  test("Predict for idestructor for struct") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      Set(),
      Vector(
        ComponentsSR(
          RangeS.internal(-84),
          TypedSR(RangeS.testZero,CodeRuneS("T"),CoordTypeSR),
          Vector(
            OrSR(RangeS.testZero,Vector(OwnershipSR(RangeS.testZero,OwnP), OwnershipSR(RangeS.testZero,ShareP))),
            BuiltinCallSR(RangeS.testZero,"passThroughIfStruct",Vector(RuneSR(RangeS.testZero,CodeRuneS("Z")))))),
        BuiltinCallSR(RangeS.testZero,"passThroughIfInterface",Vector(RuneSR(RangeS.testZero,CodeRuneS("I"))))),
      Vector.empty)
    conclusions shouldEqual Conclusions(Set(), Map(CodeRuneS("T") -> CoordTypeSR))
  }

  // See MKKRFA.
  test("Predict runes from above") {
    val conclusions =
      PredictorEvaluator.solve(
        Set(CodeRuneS("P1"), CodeRuneS("R")),
        Vector(
          EqualsSR(RangeS.testZero,
            TypedSR(RangeS.testZero,CodeRuneS("Z"),CoordTypeSR),
            InterpretedSR(RangeS.testZero,
              ConstraintP,
              ReadonlyP,
              CallSR(RangeS.testZero,
                NameSR(RangeS.testZero, CodeTypeNameS("MyIFunction1")),
                Vector(
                  RuneSR(RangeS.testZero,CodeRuneS("P1")),
                  RuneSR(RangeS.testZero,CodeRuneS("R")))))),
          TypedSR(RangeS.testZero,CodeRuneS("P1"),CoordTypeSR),
          TypedSR(RangeS.testZero,CodeRuneS("R"),CoordTypeSR)),
        Vector.empty)
    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("P1")))
    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("R")))
    vassert(conclusions.knowableValueRunes.contains(CodeRuneS("Z")))
  }
}
