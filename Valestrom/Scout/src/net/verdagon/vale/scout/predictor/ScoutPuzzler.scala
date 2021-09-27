package net.verdagon.vale.scout.predictor

//import net.verdagon.vale.solver.{AugmentAR, BuiltinCallAR, CallAR, CoordComponentsAR, CoordListAR, IRulexSR, IsaAR, KindComponentsAR, LiteralAR, LookupAR, ManualSequenceAR, OrAR, PrototypeAR, RepeaterSequenceAR}
//import net.verdagon.vale.{vfail, vimpl}

//object ScoutPuzzler {
//  def apply[RuneID, RuleID, Literal, Lookup](
//    inputRule: IRulexSR[Int, RuleID, Literal, Lookup]
//  ): Array[Array[Int]] = {
//    inputRule match {
//      case LiteralAR(range, canonicalResultRune, value) => {
//        Array(Array())
//      }
//      case LookupAR(range, canonicalResultRune, name) => {
//        Array(Array())
//      }
//      case BuiltinCallAR(range, canonicalResultRune, name, args) => {
//        vimpl() // split into actual things
//        //            (BoolAR(range, canonicalResultRune, value), Array(Array()))
//      }
//      case CallAR(range, canonicalResultRune, canonicalTemplateRune, canonicalArgRunes) => {
//        Array(Array(canonicalResultRune, canonicalTemplateRune), (Vector(canonicalTemplateRune) ++ canonicalArgRunes).toArray)
//      }
//      case CoordComponentsAR(range, canonicalCoordRune, canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune) => {
//        Array(Array(canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune), Array(canonicalCoordRune))
//      }
//      case CoordListAR(range, canonicalResultRune, canonicalElementRunes) => {
//        Array(Array(canonicalResultRune), canonicalElementRunes)
//      }
//      case AugmentAR(range, canonicalResultRune, literal, canonicalInnerRune) => {
//        Array(Array(canonicalResultRune), Array(canonicalInnerRune))
//      }
//      case IsaAR(range, canonicalSubRune, canonicalInterfaceRune) => {
//        Array(Array(vimpl()))
//      }
//      case KindComponentsAR(range, canonicalKindRune, canonicalMutabilityRune) => {
//        Array(Array(canonicalKindRune), Array(canonicalMutabilityRune))
//      }
//      case ManualSequenceAR(range, canonicalResultRune, canonicalElementRunes) => {
//        Array(Array(canonicalResultRune), canonicalElementRunes)
//      }
//      case OrAR(range, possibilities) => {
//        vimpl()
//
//      }
//      case PrototypeAR(range, canonicalResultRune, name, canonicalParameterRunes, canonicalReturnRune) => {
//        Array(Array(canonicalResultRune), canonicalParameterRunes :+ canonicalReturnRune)
//      }
//      case RepeaterSequenceAR(range, canonicalResultRune, canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune) => {
//        Array(Array(canonicalResultRune), Array(canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune))
//      }
//      case _ => vfail()
//    }
//  }
//}
