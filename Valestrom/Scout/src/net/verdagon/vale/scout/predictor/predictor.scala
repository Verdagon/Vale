//package net.verdagon.vale.scout
//
//import net.verdagon.vale.scout.IRuneS
////import net.verdagon.vale.scout.rules.ITemplataType
//import net.verdagon.vale.{vcurious, vfail}
//
////package net.verdagon.vale.scout
////
////import net.verdagon.vale.scout.rules.ITypeSR
////import net.verdagon.vale.{vcurious, vfail}
////
//package object predictor {
////  case class ConclusionsBox(var conclusions: Conclusions) {
////    override def hashCode(): Int = vfail() // Shouldnt hash, is mutable
////
////    def knowableValueRunes: Set[IRuneS] = conclusions.knowableValueRunes
////    def predictedruneToType: Map[IRuneS, ITypeSR] = conclusions.predictedruneToType
////    def markRuneValueKnowable(rune: IRuneS): Unit = {
////      conclusions = conclusions.markRuneValueKnowable(rune)
////    }
////    def markRuneTypeKnown(rune: IRuneS, tyype: ITypeSR): Unit = {
////      conclusions = conclusions.markRuneTypeKnown(rune, tyype)
////    }
////  }
//
//  case class Conclusions(
//      knowableValueRunes: Set[IRuneS],
//      predictedruneToType: Map[IRuneS, ITemplataType]) {
//    override def hashCode(): Int = vcurious()
//
//    def markRuneValueKnowable(rune: IRuneS): Conclusions = {
//      Conclusions(
//        knowableValueRunes + rune,
//        predictedruneToType)
//    }
//    def markRuneTypeKnown(rune: IRuneS, tyype: ITemplataType): Conclusions = {
//      Conclusions(
//        knowableValueRunes,
//        predictedruneToType + (rune -> tyype))
//    }
//  }
//}
