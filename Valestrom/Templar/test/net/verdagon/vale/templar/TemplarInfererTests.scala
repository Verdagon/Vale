package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.rules.{IRulexSR, RuneUsage}
import net.verdagon.vale.solver.IncompleteSolve
import net.verdagon.vale.templar.OverloadTemplar.{InferFailure, FindFunctionFailure}
import net.verdagon.vale.templar.ast.{ProgramT, PrototypeT}
import net.verdagon.vale.templar.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, INameT, PackageTopLevelNameT, PrimitiveNameT}
import net.verdagon.vale.templar.{CompileErrorExceptionT, CouldntFindFunctionToCallT, TemplarTestCompilation}
import net.verdagon.vale.{CodeLocationS, Err, RangeS, vassertOne}
//import net.verdagon.vale.astronomer.ruletyper.IRuleTyperEvaluatorDelegate
import net.verdagon.vale.astronomer.{InterfaceA, StructA}
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules.{LiteralSR, LookupSR, MutabilityLiteralSL}
import net.verdagon.vale.scout.{IEnvironment => _, _}
import net.verdagon.vale.{IProfiler, NullProfiler, PackageCoordinate, vassert, vassertSome, vfail, vimpl, scout => s}
import net.verdagon.vale.templar.env._
//import net.verdagon.vale.templar.infer.{InfererEquator, InfererEvaluator}
//import net.verdagon.vale.templar.infer.infer.{IInferSolveResult, InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.templar.templata._
//import org.scalamock.scalatest.MockFactory
import net.verdagon.vale.templar.types._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

case class FakeEnv() { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class FakeState() { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

case class SimpleEnvironment(templatas: TemplatasStore) extends IEnvironment {
//  override def localNamespaces: List[TemplatasStore] = vimpl()
  override def globalEnv: GlobalEnvironment = vimpl()
//  override def globalNamespaces: Vector[TemplatasStore] = vimpl()

  def fullName = FullNameT(PackageCoordinate.BUILTIN, Vector(), PackageTopLevelNameT())
  def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    vimpl()
//    templatas.lookupWithImpreciseName(profiler, this, nameS, lookupFilter, getOnlyNearest)
  }

  def lookupWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    vimpl()
//    templatas.lookupWithName(profiler, this, nameS, lookupFilter, getOnlyNearest)
  }
}

class FakeTemplataTemplarDelegate extends IInfererDelegate[SimpleEnvironment, FakeState] {
//  override def lookupTemplata(profiler: IProfiler, env: SimpleEnvironment, range: RangeS, name: INameS): ITemplata = {
//    val results = env.getAllTemplatasWithName(profiler, name, Set(TemplataLookupContext))
//    vassert(results.size == 1)
//    results.head
//  }

  override def coerce(env: SimpleEnvironment, state: FakeState, range: RangeS, toType: ITemplataType, templata: ITemplata): ITemplata = {
    if (templata.tyype == toType) {
      templata
    } else {
      vimpl()
    }
  }

  override def isDescendant(env: SimpleEnvironment, state: FakeState, kind: KindT): Boolean = {
    vimpl()
  }

  override def isAncestor(env: SimpleEnvironment, state: FakeState, kind: KindT): Boolean = {
    vimpl()
  }

  override def getMutability(state: FakeState, kind: KindT): MutabilityT = {
    kind match {
      case StructTT(FullNameT(_, _, CitizenNameT(humanName, _))) if humanName.startsWith("Mut") => MutableT
      case StructTT(FullNameT(_, _, CitizenNameT(humanName, _))) if humanName.startsWith("Imm") => ImmutableT
      case InterfaceTT(FullNameT(_, _, CitizenNameT(humanName, _))) if humanName.startsWith("Mut") => MutableT
      case InterfaceTT(FullNameT(_, _, CitizenNameT(humanName, _))) if humanName.startsWith("Imm") => ImmutableT
      case IntT(_) | VoidT() | BoolT() => ImmutableT
      case StaticSizedArrayTT(_, RawArrayTT(_, mutability, _)) => mutability
      case RuntimeSizedArrayTT(RawArrayTT(_, mutability, _)) => mutability
//      case TupleTT(_, StructTT(FullNameT(_, _, CitizenNameT(humanName, _)))) if humanName.startsWith("Imm") => ImmutableT
      case _ => vfail()
    }
  }
  override def evaluateInterfaceTemplata(state: FakeState, callRange: RangeS, templata: InterfaceTemplata, templateArgs: Vector[ITemplata]): (KindT) = {
    (templata, templateArgs) match {
      case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameS("MutTInterface", _))), Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)) )) => {
        InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTInterface", Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))))))
      }
      case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameS("MutInterface", _))), Vector()) => {
        InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface", Vector())))
      }
      case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameS("ImmInterface", _))), Vector()) => {
        InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ImmInterface", Vector())))
      }
    }
  }

  override def evaluateStructTemplata(state: FakeState, callRange: RangeS, templata: StructTemplata, templateArgs: Vector[ITemplata]): (KindT) = {
    (templata, templateArgs) match {
      case (StructTemplata(_,structName(TopLevelCitizenDeclarationNameS("MutTStruct", _))), Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)) )) => {
        StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct", Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))))))
      }
      case (StructTemplata(_,structName(TopLevelCitizenDeclarationNameS("MutStruct", _))), Vector()) => {
        StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))
      }
    }
  }
  override def getInterfaceTemplataType(it: InterfaceTemplata): TemplateTemplataType = {
    it match {
      case InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameS("MutTInterface", _))) => {
        TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)
      }
      case InterfaceTemplata(_, interfaceName(TopLevelCitizenDeclarationNameS("MutInterface", _))) => vfail()
    }
  }
  override def getStructTemplataType(it: StructTemplata): TemplateTemplataType = {
    it match {
      case StructTemplata(_,structName(TopLevelCitizenDeclarationNameS("MutTStruct", _))) => TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)
    }
  }
  override def getStaticSizedArrayKind(env: SimpleEnvironment, state: FakeState, mutability: MutabilityT, variability: VariabilityT, size: Int, element: CoordT): (StaticSizedArrayTT) = {
    (StaticSizedArrayTT(size, RawArrayTT(element, mutability, variability)))
  }

//  override def getTupleKind(env: SimpleEnvironment, state: FakeState, elements: Vector[CoordT]): TupleTT = {
//    // Theres only one tuple in this test, and its backed by the ImmStruct.
//    TupleTT(elements, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ImmStruct", Vector()))))
//  }

//  override def getAncestorInterfaces(state: FakeState, descendantCitizenRef: CitizenRefT): (Set[InterfaceTT]) = {
//    descendantCitizenRef match {
//      case StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))))) => Set(InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTInterface", Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))))))
//      case StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct",Vector()))) => Set(InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface", Vector()))))
//      case InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface",Vector()))) => Set()
//      case StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutSoloStruct",Vector()))) => Set()
//      case _ => vfail(descendantCitizenRef.toString)
//    }
//  }
//
//  override def getAncestorInterfaceDistance(temputs: FakeState, descendantCitizenRef: CitizenRefT, ancestorInterfaceRef: InterfaceTT): (Option[Int]) = {
//    vfail()
//  }

  override def kindIsFromTemplate(state: FakeState, actualCitizenRef: KindT, expectedCitizenTemplata: ITemplata): Boolean = {
    vimpl()
  }
  override def getAncestors(temputs: FakeState, descendant: KindT, includeSelf: Boolean): Set[KindT] = {
    vimpl()
  }

  override def lookupMemberTypes(state: FakeState, kind: KindT, expectedNumMembers: Int): Option[Vector[CoordT]] = {
    vfail()
  }

  override def getMemberCoords(state: FakeState, structTT: StructTT): Vector[CoordT] = {
    vfail()
  }

  override def structIsClosure(state: FakeState, structTT: StructTT): Boolean = {
    vfail()
  }

  override def lookupTemplata(env: SimpleEnvironment, state: FakeState, range: RangeS, name: INameT): ITemplata = {
    vassertOne(env.lookupWithName(new NullProfiler(), name, Set(TemplataLookupContext), true))
  }

  override def lookupTemplataImprecise(env: SimpleEnvironment, state: FakeState, range: RangeS, name: INameS): ITemplata = {
    vassertOne(env.lookupWithImpreciseName(new NullProfiler(), name, Set(TemplataLookupContext), true))
  }

  override def resolveExactSignature(env: SimpleEnvironment, state: FakeState, range: RangeS, name: String, coords: Vector[CoordT]): PrototypeT = {
    val templatas = env.lookupWithImpreciseName(new NullProfiler(), GlobalFunctionFamilyNameS(name), Set(TemplataLookupContext), false)
    val prototypes = templatas.collect({ case PrototypeTemplata(prot) => prot })
    val matchingPrototypes = prototypes.filter(_.paramTypes == coords)
    vassert(matchingPrototypes.size == 1)
    matchingPrototypes.head
  }

  override def getRuntimeSizedArrayKind(env: SimpleEnvironment, state: FakeState, type2: CoordT, arrayMutability: MutabilityT, arrayVariability: VariabilityT): RuntimeSizedArrayTT = {
    RuntimeSizedArrayTT(RawArrayTT(type2, arrayMutability, arrayVariability))
  }
}

class InfererTests extends FunSuite with Matchers {
  val incrementPrototype =
    PrototypeT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), FunctionNameT("increment", Vector(), Vector(CoordT(ShareT, ReadonlyT, IntT.i32)))), CoordT(ShareT, ReadonlyT, IntT.i32))

  def makeCannedEnvironment(): SimpleEnvironment = {
    // FullNameT(PackageCoordinate.BUILTIN, Vector(), PackageTopLevelNameT()),
    var entries: TemplatasStore = TemplatasStore(ProgramT.topLevelName, Map(), Map())
    val voidName = PrimitiveNameT("void")
    entries = entries.addEntry(voidName, TemplataEnvEntry(KindTemplata(VoidT())))
    val intName = PrimitiveNameT("int")
    entries = entries.addEntry(intName, TemplataEnvEntry(KindTemplata(IntT.i32)))
    val boolName = PrimitiveNameT("bool")
    entries = entries.addEntry(boolName, TemplataEnvEntry(KindTemplata(BoolT())))
    entries = entries.addEntry(
      CitizenNameT("ImmInterface", Vector()),
        InterfaceEnvEntry(
          InterfaceA(
            RangeS.internal(-70),
            TopLevelCitizenDeclarationNameS("ImmInterface", RangeS.internal(-24)),
            Vector(),
            false,
            RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
            Some(ImmutableP),
            KindTemplataType,
            Vector(),
            Map(CodeRuneS("M") -> MutabilityTemplataType),
            Vector(LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),MutabilityLiteralSL(ImmutableP))),
            Vector())))
    entries = entries.addEntry(
      CitizenNameT("ImmStruct", Vector()),
        StructEnvEntry(
          StructA(
            RangeS.internal(-71),
            TopLevelCitizenDeclarationNameS("ImmStruct", RangeS.internal(-24)),
            Vector(),
            false,
            RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
            Some(ImmutableP),
            KindTemplataType,
            Vector(),
            Map(CodeRuneS("M") -> MutabilityTemplataType, CodeRuneS("I") -> CoordTemplataType, CodeRuneS("B") -> CoordTemplataType),
            Vector(
              LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")), MutabilityLiteralSL(ImmutableP)),
              LookupSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("I")), CodeNameS("int")),
              LookupSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("B")), CodeNameS("bool"))),
            Vector(
              NormalStructMemberS(RangeS.testZero,"i", FinalP, RuneUsage(RangeS.internal(-70001), CodeRuneS("I"))),
              NormalStructMemberS(RangeS.testZero,"i", FinalP, RuneUsage(RangeS.internal(-70001), CodeRuneS("B")))))))
    entries = entries.addEntry(PrimitiveNameT("Array"), TemplataEnvEntry(RuntimeSizedArrayTemplateTemplata()))
    entries = entries.addEntry(
        CitizenTemplateNameT("MutTStruct"),//, CodeLocationS.internal(-25)),
          StructEnvEntry(
            StructA(
              RangeS.internal(-74),
              TopLevelCitizenDeclarationNameS("MutTStruct", RangeS.internal(-26)),
              Vector(),
              false,
              RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
              Some(MutableP),
              TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType),
//              Set(CodeRuneS("M")),
              Vector(RuneUsage(RangeS.internal(-70001), CodeRuneS("T"))),
//              Set(CodeRuneS("T"), CodeRuneS("M")),
              Map(CodeRuneS("T") -> CoordTemplataType, CodeRuneS("M") -> MutabilityTemplataType),
              Vector(LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")), MutabilityLiteralSL(MutableP))),
              Vector())))
    entries = entries.addEntry(CitizenTemplateNameT("MutTInterface"),//, CodeLocationS.internal(-27)),
      InterfaceEnvEntry(
        InterfaceA(
          RangeS.internal(-75),
          TopLevelCitizenDeclarationNameS("MutTInterface", RangeS.internal(-28)),
          Vector(),
          false,
          RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
          Some(MutableP),
          TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType),
//          Set(CodeRuneS("M")),
          Vector(RuneUsage(RangeS.internal(-70001), CodeRuneS("T"))),
//          Set(CodeRuneS("T"), CodeRuneS("M")),
          Map(CodeRuneS("T") -> CoordTemplataType, CodeRuneS("M") -> MutabilityTemplataType),
          Vector(LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")), MutabilityLiteralSL(MutableP))),
          Vector())))
    entries = entries.addEntry(CitizenTemplateNameT("MutStruct"),//, CodeLocationS.internal(-29)),
      StructEnvEntry(
        StructA(
          RangeS.internal(-73),
          TopLevelCitizenDeclarationNameS("MutStruct", RangeS.internal(-30)),
          Vector(),
          false,
          RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
          Some(MutableP),
          KindTemplataType,
//          Set(CodeRuneS("M")),
          Vector(),
//          Set(CodeRuneS("M")),
          Map(CodeRuneS("M") -> MutabilityTemplataType),
          Vector(LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")), MutabilityLiteralSL(MutableP))),
          Vector())))
    entries = entries.addEntry(CitizenTemplateNameT("MutInterface"),//, CodeLocationS.internal(-31)),
      InterfaceEnvEntry(
        InterfaceA(
          RangeS.internal(-72),
          TopLevelCitizenDeclarationNameS("MutInterface", RangeS.internal(-32)),
          Vector(),
          false,
          RuneUsage(RangeS.internal(-70001), CodeRuneS("M")),
          Some(MutableP),
          KindTemplataType,
//          Set(CodeRuneS("M")),
          Vector(),
//          Set(CodeRuneS("M")),
          Map(CodeRuneS("M") -> MutabilityTemplataType),
          Vector(LiteralSR(RangeS.testZero,RuneUsage(RangeS.internal(-70001), CodeRuneS("M")), MutabilityLiteralSL(MutableP))),
          Vector())))
    entries = entries.addEntry(CitizenNameT("MutStructConstraint", Vector()),
      TemplataEnvEntry(CoordTemplata(CoordT(ConstraintT,ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))))))
    entries = entries.addEntry(CitizenNameT("MutStructConstraintRW", Vector()),
      TemplataEnvEntry(CoordTemplata(CoordT(ConstraintT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))))))
    entries = entries.addEntry(CitizenNameT("MutStructWeak", Vector()),
      TemplataEnvEntry(CoordTemplata(CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))))))
    entries = entries.addEntry(CitizenNameT("MutStructWeakRW", Vector()),
      TemplataEnvEntry(CoordTemplata(CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))))))
    entries = entries.addEntry(CitizenNameT("MutStaticSizedArrayOf4Int", Vector()),
      TemplataEnvEntry(KindTemplata(StaticSizedArrayTT(4, RawArrayTT(CoordT(ShareT, ReadonlyT, IntT.i32), MutableT, VaryingT)))))
    // Tuples are normally addressed by TupleNameT, but that's a detail this test doesn't need to care about.
    entries = entries.addEntry(CitizenNameT("IntAndBoolTupName", Vector()),
      TemplataEnvEntry(
        KindTemplata(
          StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Tup", Vector(CoordTemplata(ProgramT.intType), CoordTemplata(ProgramT.boolType))))))))
    val callPrototype = PrototypeTemplata(incrementPrototype)
    entries = entries.addEntry(callPrototype.value.fullName.last, TemplataEnvEntry(callPrototype))
    SimpleEnvironment(entries)
  }

  // Makes an evaluator with some canned data
  def makeCannedEvaluator(): Unit = {
//    makeEvaluator(Some(templataTemplarDelegate), Some(delegate))
  }

  def makeEvaluator():
  Unit = {
    vimpl()
//  InfererEvaluator[SimpleEnvironment, FakeState] = {
//    val templataTemplar =
//      new TemplataTemplarInner[SimpleEnvironment, FakeState](
//        maybeTemplataTemplarDelegate match {
//          case None => new FakeTemplataTemplarInnerDelegate()
//          case Some(t) => t
//        })
//
//    val equalsLayer =
//      new InfererEquator[SimpleEnvironment, FakeState](
//        templataTemplar)
//    val inferEvaluatorDelegate =
//      maybeEvaluatorDelegate match {
//        case Some(e) => e
//        case None => new FakeInfererEvaluatorDelegate()
//      }
//    val evaluator =
//      new InfererEvaluator[SimpleEnvironment, FakeState](
//        new NullProfiler(),
//        templataTemplar,
//        equalsLayer,
//        inferEvaluatorDelegate)
//    evaluator
  }

  test("Not enough to solve") {
    val compile = TemplarTestCompilation.test(
      """
        |fn bork<T, K, Y>(a T) rules(K Ref = Y) { }
        |fn main() export { bork<int>(); }
        |""".stripMargin)
    compile.getTemputs() match {
      case Err(CouldntFindFunctionToCallT(_, FindFunctionFailure(_, _, rejectedReasonByBanner))) => {
        val List(rejection) = rejectedReasonByBanner.values.toList
        rejection match {
          case InferFailure(IncompleteSolve(incompleteConclusions, unsolvedRules, unknownRunes)) => {
            unknownRunes shouldEqual Set(CodeRuneS("Y"), CodeRuneS("K"))
          }
        }
      }
    }
  }

  test("Constraint becomes share if kind is immutable") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("__C"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("__C"), CoordTemplataType),
//              InterpretedTT(RangeS.testZero,ConstraintP,ReadonlyP,NameTT(RangeS.testZero,CodeTypeNameA("ImmInterface"), CoordTemplataType)))),
//          RangeS.testZero,
//          Map(CodeRuneT("__C") -> CoordTemplataType),
//          Set(CodeRuneT("__C")),
//          Map(),
//          Vector(),
//          None,
//          true)
//
//    vassert(
//      inferences.templatasByRune(CodeRuneT("__C")) ==
//        CoordTemplata(CoordT(ShareT, ReadonlyT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ImmInterface", Vector()))))))
  }

  test("Can infer coord rune from an incoming kind") {
//    val (isf @ InferSolveFailure(_, _, _,_,_, _, _)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(RuneTT(RangeS.testZero,CodeRuneT("C"), CoordTemplataType)),
//          RangeS.testZero,
//          Map(CodeRuneT("C") -> CoordTemplataType),
//          Set(CodeRuneT("C")),
//          Map(CodeRuneT("C") -> KindTemplata(InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ImmInterface",Vector(KindTemplata(IntT.i32))))))),
//          Vector(),
//          None,
//          true)
//
//    vassert(isf.toString.contains("doesn't match expected type"))
    vimpl()
  }

  test("Detects conflict between types") {
    vimpl()
//    val (isf @ InferSolveFailure(_, _, _,_,_, _, _)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(EqualsTR(RangeS.testZero,RuneTT(RangeS.testZero,CodeRuneT("C"), CoordTemplataType), RuneTT(RangeS.testZero,CodeRuneT("A"), KindTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("A") -> KindTemplataType),
//          Set(CodeRuneT("A"), CodeRuneT("C")),
//          Map(CodeRuneT("A") -> KindTemplata(IntT.i32)),
//          Vector(),
//          None,
//          true)
//
//    vassert(isf.toString.contains("Doesn't match type!"))
  }

  test("Can explicitly coerce from kind to coord") {
    vimpl()
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("C"), CoordTemplataType),
//              CallTR(RangeS.testZero,"toRef", Vector(RuneTT(RangeS.testZero,CodeRuneT("A"), KindTemplataType)), CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("C") -> CoordTemplataType, CodeRuneT("A") -> KindTemplataType),
//          Set(CodeRuneT("C"), CodeRuneT("A")),
//          Map(CodeRuneT("A") -> KindTemplata(IntT.i32)),
//          Vector(),
//          None,
//          true)
//
//    conclusions.templatasByRune(CodeRuneT("C")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))
  }

  test("Can explicitly coerce from kind to coord 2") {
    vimpl()
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("Z") -> CoordTemplataType),
//          Set(CodeRuneT("Z")),
//          Map(),
//          Vector(),
//          None,
//          true)
//
//    conclusions.templatasByRune(CodeRuneT("Z")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    vimpl()
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("__RetRune"), CoordTemplataType),
//              CallTR(RangeS.testZero,
//                "toRef",
//                Vector(NameTT(RangeS.testZero,CodeTypeNameA("MutStruct"), KindTemplataType)),
//                CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("__RetRune") -> CoordTemplataType),
//          Set(CodeRuneT("__RetRune")),
//          Map(),
//          Vector(),
//          None,
//          true)
//
//    conclusions.templatasByRune(CodeRuneT("__RetRune")) shouldEqual
//      CoordTemplata(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector())))))
  }

  test("Can infer from simple rules") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),CallTR(RangeS.testZero,"toRef", Vector(NameTT(RangeS.testZero,CodeTypeNameA("int"), KindTemplataType)), CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("Z") -> CoordTemplataType),
//          Set(CodeRuneT("Z")),
//          Map(),
//          Vector(),
//          None,
//          true)
//
//    vassert(inferences.templatasByRune(CodeRuneT("Z")) == CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))
  }

  test("Can infer templata from CallAT") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("X"), KindTemplataType),
//              CallTT(RangeS.testZero,NameTT(RangeS.testZero,CodeTypeNameA("MutTInterface"), TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)),Vector(RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType)), KindTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("X") -> KindTemplataType, CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("X"), CodeRuneT("T")),
//          Map(CodeRuneT("X") -> KindTemplata(InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTInterface",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))))))),
//          Vector(),
//          None,
//          true)
//
//    vassert(inferences.templatasByRune(CodeRuneT("T")) == CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))
  }

  test("Can conjure an owning coord from a borrow coord") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType),
//            RuneTT(RangeS.testZero,CodeRuneT("1337"), KindTemplataType),
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType),
//              ComponentsTR(
//                RangeS.internal(-100),
//                CoordTemplataType,
//                Vector(
//                  OwnershipTT(RangeS.testZero,OwnP),
//                  PermissionTT(RangeS.testZero,ReadwriteP),
//                  RuneTT(RangeS.testZero,CodeRuneT("1337"), KindTemplataType)))),
//            RuneTT(RangeS.testZero,CodeRuneT("0"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("0"), CoordTemplataType),
//              ComponentsTR(
//                RangeS.internal(-101),
//                CoordTemplataType,
//                Vector(
//                  OwnershipTT(RangeS.testZero,ConstraintP),
//                  PermissionTT(RangeS.testZero,ReadonlyP),
//                  RuneTT(RangeS.testZero,CodeRuneT("1337"), KindTemplataType))))),
//          RangeS.testZero,
//          Map(
//            CodeRuneT("1337") -> KindTemplataType,
//            CodeRuneT("0") -> CoordTemplataType,
//            CodeRuneT("YT") -> CoordTemplataType),
//          Set(CodeRuneT("1337"), CodeRuneT("0"), CodeRuneT("T")),
//          Map(),
//          Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("m"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("0"),None)),
//          Some(Vector(ParamFilter(CoordT(ConstraintT,ReadonlyT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface", Vector())))),None))),
//          true)
//
//    vassert(inferences.templatasByRune(CodeRuneT("T")) == CoordTemplata(CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface", Vector()))))))
    vimpl()
  }

  test("Rune 0 upcasts to right type, simple") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("__Let0_"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("__Let0_"), CoordTemplataType),
//              CallTR(RangeS.testZero,"toRef", Vector(NameTT(RangeS.testZero,CodeTypeNameA("MutInterface"), KindTemplataType)), CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("__Let0_") -> CoordTemplataType),
//          Set(CodeRuneT("__Let0_")),
//          Map(),
//          Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("x"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("__Let0_"),None)),
//          Some(Vector(ParamFilter(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct",Vector())))),None))),
//          true)
//
//    vassert(inferences.templatasByRune(CodeRuneT("__Let0_")) == CoordTemplata(CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface", Vector()))))))
  }

  test("Rune 0 upcasts to right type templated") {
    vimpl()
//    val (InferSolveSuccess(inferences)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            RuneTT(RangeS.testZero,CodeRuneT("__Let0_"), CoordTemplataType),
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("__Let0_"), CoordTemplataType),
//              CallTR(RangeS.testZero,
//                "toRef",
//                Vector(
//                  CallTT(RangeS.testZero,
//                    NameTT(RangeS.testZero,CodeTypeNameA("MutTInterface"), TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)),
//                    Vector(RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType)),
//                    KindTemplataType)),
//                CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("__Let0_") -> CoordTemplataType, CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("__Let0_"), CodeRuneT("T")),
//          Map(),
//          Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("x"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("__Let0_"),None)),
//          Some(Vector(ParamFilter(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))))))),None))),
//          true)
//
//    vassert(
//      inferences.templatasByRune(CodeRuneT("__Let0_")) ==
//        CoordTemplata(CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTInterface", Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))))))))
//    vassert(
//      inferences.templatasByRune(CodeRuneT("T")) ==
//        CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32)))
  }

  test("Tests destructor") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//
//    val rules =
//      Vector(
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType),
//          ComponentsTR(
//            RangeS.internal(-102),
//            CoordTemplataType,
//            Vector(
//              OrTR(RangeS.testZero,Vector(OwnershipTT(RangeS.testZero,OwnP), OwnershipTT(RangeS.testZero,ShareP))),
//              OrTR(RangeS.testZero,Vector(PermissionTT(RangeS.testZero,ReadwriteP), PermissionTT(RangeS.testZero,ReadonlyP))),
//              CallTR(RangeS.testZero,"passThroughIfConcrete",Vector(RuneTT(RangeS.testZero,CodeRuneT("Z"), KindTemplataType)), KindTemplataType)))),
//        EqualsTR(RangeS.testZero,RuneTT(RangeS.testZero,CodeRuneT("V"), CoordTemplataType),CallTR(RangeS.testZero,"toRef",Vector(NameTT(RangeS.testZero,CodeTypeNameA("void"),KindTemplataType)), CoordTemplataType)))
//    val atoms =
//      Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None))
//
//    val solve =
//      (paramFilter: ParamFilter) => {
//        makeCannedEvaluator().solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          rules,
//          RangeS.testZero,
//          Map(CodeRuneT("V") -> CoordTemplataType, CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("V"), CodeRuneT("T"), CodeRuneT("Z")),
//          Map(),
//          atoms,
//          Some(Vector(paramFilter)),
//          true)
//      }
//
//    // Test that it does match a pack
//    val packCoord = CoordT(ShareT, ReadonlyT,PackTT(Vector(),StructTT(FullNameT(PackageCoordinate.BUILTIN, Vector(), CitizenNameT("__Pack",Vector())))))
//    val (InferSolveSuccess(inferencesA)) = solve(ParamFilter(packCoord,None))
//    vassert(inferencesA.templatasByRune(CodeRuneT("T")) == CoordTemplata(packCoord))
//
//    // Test that it does match a struct
//    val structCoord = CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct",Vector()))))
//    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
//    vassert(inferencesD.templatasByRune(CodeRuneT("T")) == CoordTemplata(structCoord))
//
//    // Test that it doesn't match an int
//    val intCoord = CoordT(ShareT, ReadonlyT,IntT.i32)
//    val (isfE @ InferSolveFailure(_, _,_,_, _, _, _)) = solve(ParamFilter(intCoord,None))
//    vassert(isfE.toString.contains("Bad arguments to passThroughIfConcrete"))
//
//    // Test that it doesn't match an interface
//    val interfaceCoord = CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface",Vector()))))
//    val (isfF @ InferSolveFailure(_, _, _,_,_, _, _)) = solve(ParamFilter(interfaceCoord,None))
//    vassert(isfF.toString.contains("Bad arguments to passThroughIfConcrete"))
  }

  test("Tests passThroughIfInterface") {
    vimpl()
//    // Tests that we can make a rule that will only match interfaces.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//
//    val rules =
//      Vector(
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType),
//          ComponentsTR(
//            RangeS.internal(-103),
//            CoordTemplataType,
//            Vector(
//              OrTR(RangeS.testZero,Vector(OwnershipTT(RangeS.testZero,OwnP), OwnershipTT(RangeS.testZero,ShareP))),
//              OrTR(RangeS.testZero,Vector(PermissionTT(RangeS.testZero,ReadwriteP), PermissionTT(RangeS.testZero,ReadonlyP))),
//              CallTR(RangeS.testZero,"passThroughIfInterface",Vector(RuneTT(RangeS.testZero,CodeRuneT("Z"), KindTemplataType)), KindTemplataType)))),
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("V"), CoordTemplataType),
//          CallTR(RangeS.testZero,"toRef",Vector(NameTT(RangeS.testZero,CodeTypeNameA("void"), KindTemplataType)), CoordTemplataType)))
//    val atoms =
//      Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"),NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None))
//
//    val solve =
//      (paramFilter: ParamFilter) => {
//        makeCannedEvaluator().solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          rules,
//          RangeS.testZero,
//          Map(CodeRuneT("T") -> CoordTemplataType, CodeRuneT("V") -> CoordTemplataType),
//          Set(CodeRuneT("T"), CodeRuneT("V"), CodeRuneT("Z")),
//          Map(),
//          atoms,
//          Some(Vector(paramFilter)),
//          true)
//      }
//
//    // Test that it does match an interface
//    val interfaceCoord = CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface",Vector()))))
//    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(interfaceCoord,None))
//    vassert(inferencesD.templatasByRune(CodeRuneT("T")) == CoordTemplata(interfaceCoord))
//
//    // Test that it doesn't match an int
//    val intCoord = CoordT(ShareT, ReadonlyT,IntT.i32)
//    val (isfE @ InferSolveFailure(_, _, _, _,_,_, _)) = solve(ParamFilter(intCoord,None))
//    vassert(isfE.toString.contains("Bad arguments to passThroughIfInterface"))
//
//    // TODO: make a more accurate test that tests a struct doesn't match. Tried doing
//    // it like the int, but since its handed in as a parameter, it just upcasted! LOL
  }


  test("Tests passThroughIfStruct") {
    vimpl()
//    // Tests that we can make a rule that will only match structs.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//
//    val rules =
//      Vector(
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType),
//          ComponentsTR(
//            RangeS.internal(-107),
//            CoordTemplataType,
//            Vector(
//              OrTR(RangeS.testZero,Vector(OwnershipTT(RangeS.testZero,OwnP), OwnershipTT(RangeS.testZero,ShareP))),
//              OrTR(RangeS.testZero,Vector(PermissionTT(RangeS.testZero,ReadwriteP), PermissionTT(RangeS.testZero,ReadonlyP))),
//              CallTR(RangeS.testZero,"passThroughIfStruct",Vector(RuneTT(RangeS.testZero,CodeRuneT("Z"), KindTemplataType)), KindTemplataType)))))
//    val atoms =
//      Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None))
//
//    val solve =
//      (paramFilter: ParamFilter) => {
//        makeCannedEvaluator().solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          rules,
//          RangeS.testZero,
//          Map(CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("T"), CodeRuneT("Z")),
//          Map(),
//          atoms,
//          Some(Vector(paramFilter)),
//          true)
//      }
//
//    // Test that it does match a struct
//    val structCoord = CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct",Vector()))))
//    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
//    vassert(inferencesD.templatasByRune(CodeRuneT("T")) == CoordTemplata(structCoord))
//
//    // Test that it doesn't match an int
//    val intCoord = CoordT(ShareT, ReadonlyT,IntT.i32)
//    val (isfE @ InferSolveFailure(_, _, _,_,_, _, _)) = solve(ParamFilter(intCoord,None))
//    vassert(isfE.toString.contains("Bad arguments to passThroughIfStruct"))
//
//    // Test that it doesn't match an interface
//    val interfaceCoord = CoordT(OwnT,ReadwriteT, InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutInterface",Vector()))))
//    val (isfF @ InferSolveFailure(_, _, _,_,_, _, _)) = solve(ParamFilter(interfaceCoord,None))
//    vassert(isfF.toString.contains("Bad arguments to passThroughIfStruct"))
//
//    // Test that it doesn't match an pack
//    val packCoord = CoordT(ShareT, ReadonlyT,PackTT(Vector(),StructTT(FullNameT(PackageCoordinate.BUILTIN, Vector(), CitizenNameT("__Pack",Vector())))))
//    val (isfG @ InferSolveFailure(_, _, _,_,_, _, _)) = solve(ParamFilter(packCoord,None))
//    vassert(isfG.toString.contains("Bad arguments to passThroughIfStruct"))
  }

  test("Test coercing template call result") {
    vimpl()
//    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
//    // It doesn't have to be in this form, but we do need the capability in some way, so that
//    // we can have a templated destructor that matches any of those.
//
//    val rules =
//      Vector(
//        RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//          CallTT(RangeS.testZero,
//            NameTT(RangeS.testZero,CodeTypeNameA("MutTStruct"), TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)),
//            Vector(NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType)),
//            CoordTemplataType)))
//    val atoms =
//      Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None))
//
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        rules,
//        RangeS.testZero,
//        Map(CodeRuneT("Z") -> CoordTemplataType),
//        Set(CodeRuneT("Z")),
//        Map(),
//        atoms,
//        None,
//        true)
//
//    inferencesD.templatasByRune(CodeRuneT("Z")) shouldEqual
//      CoordTemplata(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))))))))
  }


  test("Test result of a CallAT can coerce to coord") {
    vimpl()
//    val rules =
//      Vector(
//        RuneTT(RangeS.testZero,CodeRuneT("__Par0"), CoordTemplataType),
//        EqualsTR(RangeS.testZero,RuneTT(RangeS.testZero,CodeRuneT("__Par0"), CoordTemplataType),NameTT(RangeS.testZero,CodeTypeNameA("MutStruct"), CoordTemplataType)))
//    val atoms =
//      Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None))
//
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        rules,
//        RangeS.testZero,
//        Map(CodeRuneT("__Par0") -> CoordTemplataType),
//        Set(CodeRuneT("__Par0")),
//        Map(),
//        atoms,
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("__Par0")) shouldEqual
//      CoordTemplata(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct",Vector())))))
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    vimpl()
//    val rules =
//      Vector(
//        RuneTT(RangeS.testZero,CodeRuneT("0"), CoordTemplataType),
//        EqualsTR(RangeS.testZero,
//          RuneTT(RangeS.testZero,CodeRuneT("0"), CoordTemplataType),
//            CallTT(RangeS.testZero,
//              NameTT(RangeS.testZero,CodeTypeNameA("MutTStruct"), TemplateTemplataType(Vector(CoordTemplataType), KindTemplataType)),
//              Vector(RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType)),
//              CoordTemplataType)))
//
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        rules,
//        RangeS.testZero,
//        Map(CodeRuneT("0") -> CoordTemplataType, CodeRuneT("T") -> CoordTemplataType),
//        Set(CodeRuneT("0"), CodeRuneT("T")),
//        Map(),
//        Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("x"),NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),Some(AbstractAP),CodeRuneS("0"),None)),
//        Some(Vector(ParamFilter(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))))))),None))),
//        true)
//    inferencesD.templatasByRune(CodeRuneT("0")) shouldEqual
//      CoordTemplata(CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutTStruct",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))))))))
  }

  test("Test destructuring") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          RuneTT(RangeS.testZero,CodeRuneT("__Let0_"), CoordTemplataType),
//          RuneTT(RangeS.testZero,CodeRuneT("__Let0__Mem_0"), CoordTemplataType),
//          RuneTT(RangeS.testZero,CodeRuneT("__Let0__Mem_1"), CoordTemplataType)),
//        RangeS.testZero,
//        Map(CodeRuneT("__Let0_") -> CoordTemplataType, CodeRuneT("__Let0__Mem_0") -> CoordTemplataType, CodeRuneT("__Let0__Mem_1") -> CoordTemplataType),
//        Set(CodeRuneT("__Let0_"), CodeRuneT("__Let0__Mem_0"), CodeRuneT("__Let0__Mem_1")),
//        Map(),
//        Vector(
//          AtomSP(RangeS.testZero,
//            Some(LocalA(CodeVarNameA("a"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
//            None,
//            CodeRuneS("__Let0_"),
//            Some(
//              Vector(
//                AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("x"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("__Let0__Mem_0"),None),
//                AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("y"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("__Let0__Mem_1"),None))))),
//        Some(Vector(ParamFilter(CoordT(ShareT, ReadonlyT,PackTT(Vector(CoordT(ShareT, ReadonlyT,IntT.i32), CoordT(ShareT, ReadonlyT,IntT.i32)),StructTT(FullNameT(PackageCoordinate.BUILTIN, Vector(), CitizenNameT("__Pack",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32)), CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32)))))))),None))),
//        true)
//    inferencesD.templatasByRune(CodeRuneT("__Let0_")) shouldEqual
//      CoordTemplata(
//        CoordT(
//          ShareT,
//          ReadonlyT,
//          PackTT(
//            Vector(CoordT(ShareT, ReadonlyT,IntT.i32), CoordT(ShareT, ReadonlyT,IntT.i32)),
//            StructTT(FullNameT(PackageCoordinate.BUILTIN, Vector(), CitizenNameT("__Pack",Vector(CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32)), CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32)))))))))
//    inferencesD.templatasByRune(CodeRuneT("__Let0__Mem_0")) shouldEqual
//      CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
//    inferencesD.templatasByRune(CodeRuneT("__Let0__Mem_1")) shouldEqual
//      CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
  }

  test("Test evaluating array sequence") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//          EqualsTR(RangeS.testZero,
//            RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//              RepeaterSequenceTT(RangeS.testZero,
//                MutabilityTT(RangeS.testZero,ImmutableP),
//                VariabilityTT(RangeS.testZero,FinalP),
//                IntTT(RangeS.testZero,5),
//                InterpretedTT(RangeS.testZero,ShareP,ReadonlyP,NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType)), CoordTemplataType))),
//        RangeS.testZero,
//        Map(CodeRuneT("Z") -> CoordTemplataType),
//        Set(CodeRuneT("Z")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("Z")) shouldEqual
//      CoordTemplata(CoordT(ShareT, ReadonlyT,StaticSizedArrayTT(5,RawArrayTT(CoordT(ShareT, ReadonlyT,IntT.i32),ImmutableT,FinalT))))
  }

  test("Test matching array sequence as coord") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            NameTT(RangeS.testZero,CodeTypeNameA("MutStaticSizedArrayOf4Int"), CoordTemplataType),
//            RepeaterSequenceTT(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("M"), MutabilityTemplataType),
//              RuneTT(RangeS.testZero,CodeRuneT("V"), VariabilityTemplataType),
//              RuneTT(RangeS.testZero,CodeRuneT("N"), IntegerTemplataType),
//              RuneTT(RangeS.testZero,CodeRuneT("E"), CoordTemplataType),
//              CoordTemplataType))),
//        RangeS.testZero,
//        Map(CodeRuneT("E") -> CoordTemplataType),
//        Set(CodeRuneT("E"), CodeRuneT("M"), CodeRuneT("V"), CodeRuneT("N")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("M")) shouldEqual MutabilityTemplata(MutableT)
//    inferencesD.templatasByRune(CodeRuneT("V")) shouldEqual VariabilityTemplata(VaryingT)
//    inferencesD.templatasByRune(CodeRuneT("N")) shouldEqual IntegerTemplata(4)
//    inferencesD.templatasByRune(CodeRuneT("E")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
  }

  test("Test matching array sequence as kind") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            NameTT(RangeS.testZero,CodeTypeNameA("MutStaticSizedArrayOf4Int"), KindTemplataType),
//              RepeaterSequenceTT(RangeS.testZero,
//                RuneTT(RangeS.testZero,CodeRuneT("M"), MutabilityTemplataType),
//                RuneTT(RangeS.testZero,CodeRuneT("V"), VariabilityTemplataType),
//                RuneTT(RangeS.testZero,CodeRuneT("N"), IntegerTemplataType),
//                RuneTT(RangeS.testZero,CodeRuneT("E"), CoordTemplataType),
//                KindTemplataType))),
//        RangeS.testZero,
//        Map(CodeRuneT("E") -> CoordTemplataType),
//        Set(CodeRuneT("E"), CodeRuneT("M"), CodeRuneT("V"), CodeRuneT("N")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("M")) shouldEqual MutabilityTemplata(MutableT)
//    inferencesD.templatasByRune(CodeRuneT("V")) shouldEqual VariabilityTemplata(VaryingT)
//    inferencesD.templatasByRune(CodeRuneT("N")) shouldEqual IntegerTemplata(4)
//    inferencesD.templatasByRune(CodeRuneT("E")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
  }

  test("Test evaluating manual sequence") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            RuneTT(RangeS.testZero,CodeRuneT("Z"), CoordTemplataType),
//              ManualSequenceTT(RangeS.testZero,
//                Vector(
//                  NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType),
//                  NameTT(RangeS.testZero,CodeTypeNameA("bool"), CoordTemplataType)),
//                CoordTemplataType))),
//        RangeS.testZero,
//        Map(CodeRuneT("Z") -> CoordTemplataType),
//        Set(CodeRuneT("Z")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("Z")) shouldEqual
//      CoordTemplata(
//        CoordT(
//          ShareT,
//          ReadonlyT,
//          TupleTT(
//            Vector(CoordT(ShareT, ReadonlyT,IntT.i32), CoordT(ShareT, ReadonlyT,BoolT())),
//            StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(),CitizenNameT("ImmStruct",Vector()))))))
  }

  test("Test matching manual sequence as coord") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            NameTT(RangeS.testZero,CodeTypeNameA("IntAndBoolTupName"), CoordTemplataType),
//              ManualSequenceTT(RangeS.testZero,
//                Vector(
//                  RuneTT(RangeS.testZero,CodeRuneT("A"), CoordTemplataType),
//                  RuneTT(RangeS.testZero,CodeRuneT("B"), CoordTemplataType)),
//                CoordTemplataType))),
//        RangeS.testZero,
//        Map(),
//        Set(CodeRuneT("A"), CodeRuneT("B")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("A")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
//    inferencesD.templatasByRune(CodeRuneT("B")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,BoolT()))
  }

  test("Test matching manual sequence as kind") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            NameTT(RangeS.testZero,CodeTypeNameA("IntAndBoolTupName"), KindTemplataType),
//              ManualSequenceTT(RangeS.testZero,
//                Vector(
//                  RuneTT(RangeS.testZero,CodeRuneT("A"), CoordTemplataType),
//                  RuneTT(RangeS.testZero,CodeRuneT("B"), CoordTemplataType)),
//                KindTemplataType))),
//        RangeS.testZero,
//        Map(),
//        Set(CodeRuneT("A"), CodeRuneT("B")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("A")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
//    inferencesD.templatasByRune(CodeRuneT("B")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,BoolT()))
  }

  test("Test array") {
    vimpl()
//    val (InferSolveSuccess(inferencesD)) =
//      makeCannedEvaluator().solve(
//        makeCannedEnvironment(),
//        FakeState(),
//        Vector(
//          EqualsTR(RangeS.testZero,
//            RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              CallTT(RangeS.testZero,
//                NameTT(RangeS.testZero,CodeTypeNameA("Array"), TemplateTemplataType(Vector(MutabilityTemplataType, VariabilityTemplataType, CoordTemplataType), KindTemplataType)),
//                Vector(MutabilityTT(RangeS.testZero,MutableP), VariabilityTT(RangeS.testZero,VaryingP), NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType)),
//                KindTemplataType)),
//          EqualsTR(RangeS.testZero,
//            RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              CallTT(RangeS.testZero,
//                NameTT(RangeS.testZero,CodeTypeNameA("Array"), TemplateTemplataType(Vector(MutabilityTemplataType, VariabilityTemplataType, CoordTemplataType), KindTemplataType)),
//                Vector(
//                  RuneTT(RangeS.testZero,CodeRuneT("M"), MutabilityTemplataType),
//                  RuneTT(RangeS.testZero,CodeRuneT("V"), VariabilityTemplataType),
//                  RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType)),
//                KindTemplataType))),
//        RangeS.testZero,
//        Map(
//          CodeRuneT("T") -> CoordTemplataType,
//          CodeRuneT("M") -> MutabilityTemplataType,
//          CodeRuneT("V") -> VariabilityTemplataType,
//          CodeRuneT("K") -> KindTemplataType),
//        Set(CodeRuneT("T"), CodeRuneT("M"), CodeRuneT("V"), CodeRuneT("K")),
//        Map(),
//        Vector(),
//        None,
//        true)
//    inferencesD.templatasByRune(CodeRuneT("M")) shouldEqual MutabilityTemplata(MutableT)
//    inferencesD.templatasByRune(CodeRuneT("V")) shouldEqual VariabilityTemplata(VaryingT)
//    inferencesD.templatasByRune(CodeRuneT("T")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT,IntT.i32))
  }

  test("Test evaluating isa") {
    vimpl()
//    val (InferSolveSuccess(_)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            IsaTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              NameTT(RangeS.testZero,CodeTypeNameA("MutInterface"), KindTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("K") -> KindTemplataType),
//          Set(CodeRuneT("K")),
//          Map(CodeRuneT("K") -> KindTemplata(StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))),
//          Vector(),
//          None,
//          true)
//
//    val (isf @ InferSolveFailure(_, _, _,_,_, _, _)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            IsaTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              NameTT(RangeS.testZero,CodeTypeNameA("MutInterface"), KindTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("K") -> KindTemplataType),
//          Set(CodeRuneT("K")),
//          Map(CodeRuneT("K") -> KindTemplata(StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutSoloStruct", Vector()))))),
//          Vector(),
//          None,
//          true)
//    vassert(isf.toString.contains("Isa failed"))
  }

  test("Test matching isa") {
    vimpl()
//    val (InferSolveSuccess(_)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              IsaTR(RangeS.testZero,
//                RuneTT(RangeS.testZero,CodeRuneT("Z"), KindTemplataType),
//                NameTT(RangeS.testZero,CodeTypeNameA("MutInterface"), KindTemplataType)))),
//          RangeS.testZero,
//          Map(CodeRuneT("K") -> KindTemplataType),
//          Set(CodeRuneT("K"), CodeRuneT("Z")),
//          Map(CodeRuneT("K") -> KindTemplata(StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))),
//          Vector(),
//          None,
//          true)
//
//    val (isf @ InferSolveFailure(_, _,_,_, _, _, _)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            IsaTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("K"), KindTemplataType),
//              NameTT(RangeS.testZero,CodeTypeNameA("MutInterface"), KindTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("K") -> KindTemplataType),
//          Set(CodeRuneT("K")),
//          Map(CodeRuneT("K") -> KindTemplata(StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutSoloStruct", Vector()))))),
//          Vector(),
//          None,
//          true)
//    vassert(isf.toString.contains("Isa failed"))
  }

  test("Test evaluate prototype components") {
    vimpl()
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              ComponentsTR(
//                RangeS.internal(-104),
//                PrototypeTemplataType,
//                Vector(
//                  StringTT(RangeS.testZero,"increment"),
//                  CoordListTT(RangeS.testZero,Vector(NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType))),
//                  NameTT(RangeS.testZero,CodeTypeNameA("int"), CoordTemplataType))),
//              RuneTT(RangeS.testZero,CodeRuneT("F"),PrototypeTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("F") -> PrototypeTemplataType),
//          Set(CodeRuneT("F")),
//          Map(),
//          Vector(),
//          None,
//          true)
//    conclusions.templatasByRune(CodeRuneT("F")) shouldEqual PrototypeTemplata(incrementPrototype)
  }

  test("Test evaluate prototype return") {
    vimpl()
//    // We evaluate the prototype return when we fail to evaluate the name and params.
//    // Lets make it so we can only evaluate the params from evaluating the ret, to exercise
//    // evaluating the ret.
//
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              ComponentsTR(
//                RangeS.internal(-105),
//                PrototypeTemplataType,
//                Vector(
//                  StringTT(RangeS.testZero,"increment"),
//                    CoordListTT(RangeS.testZero,
//                      Vector(
//                        RuneTT(RangeS.testZero,CodeRuneT("T"),CoordTemplataType))),
//                  EqualsTR(RangeS.testZero,
//                    NameTT(RangeS.testZero,CodeTypeNameA("int"),CoordTemplataType),
//                    RuneTT(RangeS.testZero,CodeRuneT("T"),CoordTemplataType)))),
//              RuneTT(RangeS.testZero,CodeRuneT("F"),PrototypeTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("T") -> CoordTemplataType, CodeRuneT("F") -> PrototypeTemplataType),
//          Set(CodeRuneT("T"), CodeRuneT("F")),
//          Map(),
//          Vector(),
//          None,
//          true)
//    conclusions.templatasByRune(CodeRuneT("F")) shouldEqual PrototypeTemplata(incrementPrototype)
  }

  test("Test match prototype components") {
    vimpl()
//    val (InferSolveSuccess(conclusions)) =
//      makeCannedEvaluator()
//        .solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              RuneTT(RangeS.testZero,CodeRuneT("F"),PrototypeTemplataType),
//              ComponentsTR(
//                RangeS.internal(-106),
//                PrototypeTemplataType,
//                Vector(
//                  RuneTT(RangeS.testZero,CodeRuneT("X"),StringTemplataType),
//                  RuneTT(RangeS.testZero,CodeRuneT("Y"),PackTemplataType(CoordTemplataType)),
//                  RuneTT(RangeS.testZero,CodeRuneT("T"),CoordTemplataType))))),
//          RangeS.testZero,
//          Map(CodeRuneT("X") -> StringTemplataType, CodeRuneT("Y") -> PackTemplataType(CoordTemplataType), CodeRuneT("T") -> CoordTemplataType, CodeRuneT("F") -> PrototypeTemplataType),
//          Set(CodeRuneT("X"), CodeRuneT("Y"), CodeRuneT("T"), CodeRuneT("F")),
//          Map(CodeRuneT("F") -> PrototypeTemplata(incrementPrototype)),
//          Vector(),
//          None,
//          true)
//    conclusions.templatasByRune(CodeRuneT("X")) shouldEqual StringTemplata("increment")
//    conclusions.templatasByRune(CodeRuneT("Y")) shouldEqual CoordListTemplata(Vector(CoordT(ShareT, ReadonlyT, IntT.i32)))
//    conclusions.templatasByRune(CodeRuneT("T")) shouldEqual CoordTemplata(CoordT(ShareT, ReadonlyT, IntT.i32))
  }

  test("Test InterpretedTT") {
    vimpl()
//    def run(sourceName: String, targetOwnership: OwnershipP, targetPermission: PermissionP): IInferSolveResult = {
//      val result =
//        makeCannedEvaluator().solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              InterpretedTT(RangeS.testZero,targetOwnership,targetPermission, NameTT(RangeS.testZero,CodeTypeNameA(sourceName), CoordTemplataType)),
//              RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType))),
//          RangeS.testZero,
//          Map(CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("T")),
//          Map(),
//          Vector(AtomSP(RangeS.testZero,Some(LocalA(CodeVarNameA("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),None,CodeRuneS("T"),None)),
//          None,
//          true)
//      result
//    }
//
//    def expectSuccess(inferSolveResult: IInferSolveResult): CoordT = {
//      val InferSolveSuccess(inferencesD) = inferSolveResult
//      val CoordTemplata(coord) = inferencesD.templatasByRune(CodeRuneT("T"))
//      coord
//    }
//
//    def expectFail(inferSolveResult: IInferSolveResult): String = {
//      val isf @ InferSolveFailure(_, _, _, _, _, _, _) = inferSolveResult
//      isf.toString
//    }
//
//    // Dont need to test Own + Readonly, because its impossible to express that with an InterpretedTT rule.
//    expectSuccess(run("int", OwnP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    expectSuccess(run("int", ConstraintP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    expectSuccess(run("int", ConstraintP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    vassert(expectFail(run("int", WeakP, ReadonlyP)).contains("Expected a weak, but was a share"))
//    vassert(expectFail(run("int", WeakP, ReadwriteP)).contains("Expected a weak, but was a share"))
//    expectSuccess(run("int", ShareP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    expectSuccess(run("int", ShareP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//
//    vassert(expectFail(run("MutStruct", ShareP, ReadonlyP)).contains("Expected a share, but was an own"))
//    expectSuccess(run("MutStruct", OwnP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStruct", ConstraintP, ReadonlyP)) shouldEqual CoordT(ConstraintT,ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStruct", ConstraintP, ReadwriteP)) shouldEqual CoordT(ConstraintT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStruct", WeakP, ReadonlyP)) shouldEqual CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStruct", WeakP, ReadwriteP)) shouldEqual CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//
//    vassert(expectFail(run("MutStructConstraint", ShareP, ReadonlyP)).contains("Expected a share, but was a borrow"))
//    expectSuccess(run("MutStructConstraint", OwnP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructConstraint", ConstraintP, ReadonlyP)) shouldEqual CoordT(ConstraintT,ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    // &!T given a &Spaceship should give a &T, it should make the ro into a Readwrite.
//    expectSuccess(run("MutStructConstraint", ConstraintP, ReadwriteP)) shouldEqual CoordT(ConstraintT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructConstraint", WeakP, ReadonlyP)) shouldEqual CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    // &&!T given a &Spaceship should give a &T, it should make the ro into a Readwrite, and the borrow into a weak.
//    expectSuccess(run("MutStructConstraint", WeakP, ReadwriteP)) shouldEqual CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//
//    vassert(expectFail(run("MutStructConstraintRW", ShareP, ReadonlyP)).contains("Expected a share, but was a borrow"))
//    expectSuccess(run("MutStructConstraintRW", OwnP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    // &T given a &!Spaceship should give a &T, it should make the Readwrite into a Readonly.
//    expectSuccess(run("MutStructConstraintRW", ConstraintP, ReadonlyP)) shouldEqual CoordT(ConstraintT,ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructConstraintRW", ConstraintP, ReadwriteP)) shouldEqual CoordT(ConstraintT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructConstraintRW", WeakP, ReadonlyP)) shouldEqual CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    // &&T given a &!Spaceship should give a &T, it should make the Readwrite into a Readonly, and the borrow into a weak.
//    expectSuccess(run("MutStructConstraintRW", WeakP, ReadwriteP)) shouldEqual CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//
//    vassert(expectFail(run("MutStructWeak", ShareP, ReadonlyP)).contains("Expected a share, but was a weak"))
//    vassert(expectFail(run("MutStructWeak", OwnP, ReadwriteP)).contains("Expected a own, but was a weak"))
//    vassert(expectFail(run("MutStructWeak", ConstraintP, ReadonlyP)).contains("Expected a borrow, but was a weak"))
//    vassert(expectFail(run("MutStructWeak", ConstraintP, ReadwriteP)).contains("Expected a borrow, but was a weak"))
//    expectSuccess(run("MutStructWeak", WeakP, ReadonlyP)) shouldEqual CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructWeak", WeakP, ReadwriteP)) shouldEqual CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//
//    vassert(expectFail(run("MutStructWeakRW", ShareP, ReadonlyP)).contains("Expected a share, but was a weak"))
//    vassert(expectFail(run("MutStructWeakRW", OwnP, ReadwriteP)).contains("Expected a own, but was a weak"))
//    vassert(expectFail(run("MutStructWeakRW", ConstraintP, ReadonlyP)).contains("Expected a borrow, but was a weak"))
//    vassert(expectFail(run("MutStructWeakRW", ConstraintP, ReadwriteP)).contains("Expected a borrow, but was a weak"))
//    expectSuccess(run("MutStructWeakRW", WeakP, ReadonlyP)) shouldEqual CoordT(WeakT, ReadonlyT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    expectSuccess(run("MutStructWeakRW", WeakP, ReadwriteP)) shouldEqual CoordT(WeakT, ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//
//    expectSuccess(run("void", ShareP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    expectSuccess(run("void", OwnP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    expectSuccess(run("void", ConstraintP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    expectSuccess(run("void", ConstraintP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    vassert(expectFail(run("void", WeakP, ReadonlyP)).contains("Expected a weak, but was a share"))
//    vassert(expectFail(run("void", WeakP, ReadwriteP)).contains("Expected a weak, but was a share"))
  }

  test("test matching ownershipped") {
    vimpl()
//    def run(sourceName: String, targetOwnership: OwnershipP, targetPermission: PermissionP): IInferSolveResult = {
//      val result =
//        makeCannedEvaluator().solve(
//          makeCannedEnvironment(),
//          FakeState(),
//          Vector(
//            EqualsTR(RangeS.testZero,
//              NameTT(RangeS.testZero,CodeTypeNameA(sourceName), CoordTemplataType),
//              InterpretedTT(RangeS.testZero,targetOwnership,targetPermission, RuneTT(RangeS.testZero,CodeRuneT("T"), CoordTemplataType)))),
//          RangeS.testZero,
//          Map(CodeRuneT("T") -> CoordTemplataType),
//          Set(CodeRuneT("T")),
//          Map(),
//          Vector(),
//          None,
//          true)
//      result
//    }
//
//    def expectSuccess(inferSolveResult: IInferSolveResult): CoordT = {
//      val InferSolveSuccess(inferencesD) = inferSolveResult
//      val CoordTemplata(coord) = inferencesD.templatasByRune(CodeRuneT("T"))
//      coord
//    }
//
//    def expectFail(inferSolveResult: IInferSolveResult): String = {
//      val isf @ InferSolveFailure(_, _, _, _, _, _, _) = inferSolveResult
//      isf.toString
//    }
//
//    expectSuccess(run("int", OwnP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    expectSuccess(run("int", ConstraintP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    expectSuccess(run("int", ConstraintP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//    vassert(expectFail(run("int", WeakP, ReadonlyP)).contains("Couldn't match incoming share against expected weak"))
//    vassert(expectFail(run("int", WeakP, ReadwriteP)).contains("Couldn't match incoming share against expected weak"))
//    expectSuccess(run("int", ShareP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, IntT.i32)
//
//    expectSuccess(run("void", OwnP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    expectSuccess(run("void", ConstraintP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    expectSuccess(run("void", ConstraintP, ReadwriteP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//    vassert(expectFail(run("void", WeakP, ReadonlyP)).contains("Couldn't match incoming share against expected weak"))
//    vassert(expectFail(run("void", WeakP, ReadwriteP)).contains("Couldn't match incoming share against expected weak"))
//    expectSuccess(run("void", ShareP, ReadonlyP)) shouldEqual CoordT(ShareT, ReadonlyT, VoidT())
//
//    vassert(expectFail(run("MutStruct", ShareP, ReadonlyP)).contains("Couldn't match incoming own against expected share"))
//    // Takes the own off the incoming own coord, ends up as another own.
//    expectSuccess(run("MutStruct", OwnP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    // Tries to take the borrow off the incoming own coord... fails.
//    vassert(expectFail(run("MutStruct", ConstraintP, ReadonlyP)).contains("Couldn't match incoming own against expected constraint"))
//    vassert(expectFail(run("MutStruct", ConstraintP, ReadwriteP)).contains("Couldn't match incoming own against expected constraint"))
//    vassert(expectFail(run("MutStruct", WeakP, ReadonlyP)).contains("Couldn't match incoming own against expected weak"))
//    vassert(expectFail(run("MutStruct", WeakP, ReadwriteP)).contains("Couldn't match incoming own against expected weak"))
//
//    // Tries to take the own off the incoming borrow coord... fails.
//    vassert(expectFail(run("MutStructConstraint", OwnP, ReadwriteP)).contains("Couldn't match incoming constraint against expected own"))
//    // Takes the borrow off the incoming borrow coord, succeeds and gives us an own.
//    expectSuccess(run("MutStructConstraint", ConstraintP, ReadonlyP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    vassert(expectFail(run("MutStructConstraint", ConstraintP, ReadwriteP)).contains("Couldn't match incoming ro against expected rw"))
//    // Takes the weak off the incoming borrow coord... fails.
//    vassert(expectFail(run("MutStructConstraint", WeakP, ReadonlyP)).contains("Couldn't match incoming constraint against expected weak"))
//    vassert(expectFail(run("MutStructConstraint", WeakP, ReadwriteP)).contains("Couldn't match incoming constraint against expected weak"))
//    vassert(expectFail(run("MutStructConstraint", ShareP, ReadonlyP)).contains("Couldn't match incoming constraint against expected share"))
//
//    // Tries to take the own off the incoming borrow coord... fails.
//    vassert(expectFail(run("MutStructConstraintRW", OwnP, ReadwriteP)).contains("Couldn't match incoming constraint against expected own"))
//    vassert(expectFail(run("MutStructConstraintRW", ConstraintP, ReadonlyP)).contains("Couldn't match incoming rw against expected ro"))
//    // Takes the borrow off the incoming borrow coord, succeeds and gives us an own.
//    expectSuccess(run("MutStructConstraintRW", ConstraintP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT,StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(),CitizenNameT("MutStruct",Vector()))))
//    // Takes the weak off the incoming borrow coord... fails.
//    vassert(expectFail(run("MutStructConstraintRW", WeakP, ReadonlyP)).contains("Couldn't match incoming constraint against expected weak"))
//    vassert(expectFail(run("MutStructConstraintRW", WeakP, ReadwriteP)).contains("Couldn't match incoming constraint against expected weak"))
//    vassert(expectFail(run("MutStructConstraintRW", ShareP, ReadonlyP)).contains("Couldn't match incoming constraint against expected share"))
//
//    // Tries to take the own off the incoming weak coord... fails.
//    vassert(expectFail(run("MutStructWeak", OwnP, ReadwriteP)).contains("Couldn't match incoming weak against expected own"))
//    // Takes the borrow off the incoming weak coord... fails.
//    vassert(expectFail(run("MutStructWeak", ConstraintP, ReadonlyP)).contains("Couldn't match incoming weak against expected constraint"))
//    vassert(expectFail(run("MutStructWeak", ConstraintP, ReadwriteP)).contains("Couldn't match incoming weak against expected constraint"))
//    // Takes the weak off the incoming weak coord, succeeds and gives us an own.
//    expectSuccess(run("MutStructWeak", WeakP, ReadonlyP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    vassert(expectFail(run("MutStructWeak", WeakP, ReadwriteP)).contains("Couldn't match incoming ro against expected rw"))
//    vassert(expectFail(run("MutStructWeak", ShareP, ReadonlyP)).contains("Couldn't match incoming weak against expected share"))
//
//    // Tries to take the own off the incoming weak coord... fails.
//    vassert(expectFail(run("MutStructWeakRW", OwnP, ReadwriteP)).contains("Couldn't match incoming weak against expected own"))
//    // Takes the borrow off the incoming weak coord... fails.
//    vassert(expectFail(run("MutStructWeakRW", ConstraintP, ReadonlyP)).contains("Couldn't match incoming weak against expected constraint"))
//    vassert(expectFail(run("MutStructWeakRW", ConstraintP, ReadwriteP)).contains("Couldn't match incoming weak against expected constraint"))
//    vassert(expectFail(run("MutStructWeakRW", WeakP, ReadonlyP)).contains("Couldn't match incoming rw against expected ro"))
//    // Takes the weak off the incoming weak coord, succeeds and gives us an own.
//    expectSuccess(run("MutStructWeakRW", WeakP, ReadwriteP)) shouldEqual CoordT(OwnT,ReadwriteT, StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("MutStruct", Vector()))))
//    vassert(expectFail(run("MutStructWeakRW", ShareP, ReadonlyP)).contains("Couldn't match incoming weak against expected share"))
  }
}
