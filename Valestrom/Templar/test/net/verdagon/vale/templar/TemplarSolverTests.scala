package net.verdagon.vale.templar

import net.verdagon.vale._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.rules.{CoordComponentsSR, KindComponentsSR, RuneUsage}
import net.verdagon.vale.solver.{FailedSolve, IncompleteSolve, RuleError}
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, WrongNumberOfArguments}
import net.verdagon.vale.templar.env.ReferenceLocalVariableT
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.infer.KindIsNotConcrete
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
//import net.verdagon.vale.templar.infer.NotEnoughToSolveError
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class TemplarSolverTests extends FunSuite with Matchers {
  // TODO: pull all of the templar specific stuff out, the unit test-y stuff

  def readCodeFromResource(resourceFilename: String): String = {
    val is = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream(resourceFilename))
    vassert(is != null)
    is.mkString("")
  }


  test("Humanize errors") {
    val fireflyKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Firefly", Vector())))
    val fireflyCoord = CoordT(OwnT,ReadwriteT,fireflyKind)
    val serenityKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Serenity", Vector())))
    val serenityCoord = CoordT(OwnT,ReadwriteT,serenityKind)
    val ispaceshipKind = InterfaceTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("ISpaceship", Vector())))
    val ispaceshipCoord = CoordT(OwnT,ReadwriteT,ispaceshipKind)
    val unrelatedKind = StructTT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), CitizenNameT("Spoon", Vector())))
    val unrelatedCoord = CoordT(OwnT,ReadwriteT,unrelatedKind)
    val fireflySignature = SignatureT(FullNameT(PackageCoordinate.TEST_TLD, Vector(), FunctionNameT("myFunc", Vector(), Vector(fireflyCoord))))
    val fireflyExport = KindExportT(RangeS.testZero, fireflyKind, PackageCoordinate.TEST_TLD, "Firefly");
    val serenityExport = KindExportT(RangeS.testZero, fireflyKind, PackageCoordinate.TEST_TLD, "Serenity");

    val codeStr = "Hello I am A large piece Of code [that has An error]"
    val filenamesAndSources = FileCoordinateMap.test(codeStr)
    def makeLoc(pos: Int) = CodeLocationS(FileCoordinate.test, pos)
    def makeRange(begin: Int, end: Int) = RangeS(makeLoc(begin), makeLoc(end))

    val unsolvedRules =
      Vector(
        CoordComponentsSR(
          makeRange(0, codeStr.length),
          RuneUsage(makeRange(6, 7), CodeRuneS("I")),
          RuneUsage(makeRange(11, 12), CodeRuneS("A")),
          RuneUsage(makeRange(25, 27), CodeRuneS("Of")),
          RuneUsage(makeRange(33, 52), ImplicitRuneS(LocationInDenizen(Vector(7))))),
        KindComponentsSR(
          makeRange(33, 52),
          RuneUsage(makeRange(33, 52), ImplicitRuneS(LocationInDenizen(Vector(7)))),
          RuneUsage(makeRange(43, 45), CodeRuneS("An"))))

    vassert(TemplarErrorHumanizer.humanize(false, filenamesAndSources,
      TemplarSolverError(
        RangeS.testZero,
        FailedSolve(
          Map(
            CodeRuneS("A") -> OwnershipTemplata(OwnT)),
          unsolvedRules,
          RuleError(
            0, KindIsNotConcrete(ispaceshipKind)))))
      .nonEmpty)

    val errorText =
      TemplarErrorHumanizer.humanize(false, filenamesAndSources,
        TemplarSolverError(
          RangeS.testZero,
          IncompleteSolve(
            Map(
              CodeRuneS("A") -> OwnershipTemplata(OwnT)),
            unsolvedRules,
            Set(
              CodeRuneS("I"),
              CodeRuneS("Of"),
              CodeRuneS("An"),
              ImplicitRuneS(LocationInDenizen(Vector(7)))))))
    println(errorText)
    vassert(errorText.nonEmpty)
    vassert(errorText.contains("\n           ^ own"))
    vassert(errorText.contains("\n      ^ (unknown)"))
    vassert(errorText.contains("\n                                 ^^^^^^^^^^^^^^^^^^^ (unknown)"))
  }
}
