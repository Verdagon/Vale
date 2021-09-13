//package net.verdagon.vale.astronomer
//
//import net.verdagon.vale.parser.{LocationP, MutabilityP, OwnershipP, PermissionP, VariabilityP}
//import net.verdagon.vale.vcurious
//
//sealed trait ILiteralAR
//
//case class IntAR(value: Long) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class StringAR(value: String) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class BoolAR(value: Boolean) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class MutabilityAR(mutability: MutabilityP) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class PermissionAR(permission: PermissionP) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class LocationAR(location: LocationP) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class OwnershipAR(ownership: OwnershipP) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//case class VariabilityAR(variability: VariabilityP) extends ILiteralAR {
//  override def hashCode(): Int = vcurious()
//}
//
//
//sealed trait ILookupAR
//
//case class NameAR(
//  name: IImpreciseNameStepA
//) extends ILookupAR {
//  override def hashCode(): Int = vcurious()
//  //  println("hi")
//}
//
//case class AbsoluteNameAR(
//  name: INameA
//) extends ILookupAR {
//  override def hashCode(): Int = vcurious()
//  //  println("hi")
//}
