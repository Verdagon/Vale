package net.verdagon.vale.scout.rules

//import net.verdagon.vale.scout.{AbsoluteNameSR, CallSR, IRuneS, IntSR, InterpretedSR, ManualSequenceSR, MutabilitySR, NameSR, OwnershipSR, PermissionSR, RangeS, RepeaterSequenceSR, RuneSR, StringSR, VariabilitySR}
//import net.verdagon.vale.solver.{AugmentAR, Builder, CallAR, CoerceToCoord, CoordComponentsAR, IRulexAR, IsConcreteAR, IsInterfaceAR, IsStructAR, KindComponentsAR, LiteralAR, LookupAR, ManualSequenceAR, OneOfAR, RepeaterSequenceAR, TentativeRune, Analysis}
//import net.verdagon.vale.{vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
//
//case class ScoutWorld(
//  runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
//  tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
//  tentativeRuneToCanonicalRune: mutable.HashMap[TentativeRune, Int],
//  world: Analysis[IRuneS, RangeS, ILiteralSR, ILookupSR]
//)
//
//object ScoutRuleBuilder {
//  def apply(): ScoutRuleBuilder = {
//    ScoutRuleBuilder(
//      mutable.HashMap[IRuneS, TentativeRune](),
//      mutable.HashMap[TentativeRune, ITypeSR](),
//      Builder[RangeS, ILiteralSR, ILookupSR](
//        ArrayBuffer[IRulexAR[TentativeRune, RangeS, ILiteralSR, ILookupSR]](),
//        0,
//        mutable.HashMap[TentativeRune, TentativeRune]()))
//  }
//}
//
//case class ScoutRuleBuilder(
//    runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
//    tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
//    builder: Builder[RangeS, ILiteralSR, ILookupSR]) {
//
//  // This will map an IRuneS to a tentative rune.
//  // This is useful because a lot of the scout's AST thinks in terms of IRuneS.
//  // They can't think in terms of canonical runes (ints) yet because those are
//  // only known after we assemble all the AST.
//  def nameTentativeRune(rangeS: RangeS, runeS: IRuneS, tentativeRune: TentativeRune) = {
//    builder.noteRunesEqual(tentativeRune, addRune(rangeS, runeS))
//    runeS
//  }
//
//}
