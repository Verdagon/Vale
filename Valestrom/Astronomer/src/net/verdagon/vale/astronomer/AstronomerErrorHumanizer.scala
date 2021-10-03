package net.verdagon.vale.astronomer

import net.verdagon.vale.{FileCoordinateMap, RangeS}
import net.verdagon.vale.SourceCodeUtils.{humanizePos, lineContaining, nextThingAndRestOfLine}
import net.verdagon.vale.scout.rules.IRulexSR
import net.verdagon.vale.scout.{IRuneS, RuneTypeSolveError, ScoutErrorHumanizer}
import net.verdagon.vale.solver.{FailedSolve, IncompleteSolve, SolverErrorHumanizer}
import net.verdagon.vale.templar.types.ITemplataType

object AstronomerErrorHumanizer {
  def assembleError(
    filenamesAndSources: FileCoordinateMap[String],
    range: RangeS,
    errorStrBody: String) = {
    val posStr = humanizePos(filenamesAndSources, range.file, range.begin.offset)
    val nextStuff = lineContaining(filenamesAndSources, range.file, range.begin.offset)
    val errorId = "A"
    f"${posStr} error ${errorId}: ${errorStrBody}\n${nextStuff}\n"
  }

  def humanize(
    filenamesAndSources: FileCoordinateMap[String],
    range: RangeS,
    err: RuneTypeSolveError):
  String = {
    ": Couldn't solve generics rules:\n" +
    SolverErrorHumanizer.humanizeFailedSolve(
      filenamesAndSources,
      ScoutErrorHumanizer.humanizeRune,
      (codeMap, tyype: ITemplataType) => tyype.toString,
      (codeMap, u: Unit) => "",
      (rule: IRulexSR) => rule.range,
      (rule: IRulexSR) => rule.runeUsages.map(u => (u.rune, u.range)),
      err.failedSolve)
  }

  def humanize(
      filenamesAndSources: FileCoordinateMap[String],
      err: ICompileErrorA):
  String = {
    val errorStrBody =
      err match {
        case RangedInternalErrorA(range, message) => {
          ": internal error: " + message
        }
        case CouldntFindTypeA(range, name) => {
          ": Couldn't find type `" + ScoutErrorHumanizer.humanizeName(name) + "`:\n"
        }
        case CouldntSolveRulesA(range, err) => {
          ": Couldn't solve generics rules:\n" +
          SolverErrorHumanizer.humanizeFailedSolve(
            filenamesAndSources,
            ScoutErrorHumanizer.humanizeRune,
            (codeMap, tyype: ITemplataType) => ScoutErrorHumanizer.humanizeTemplataType(tyype),
            (codeMap, u: Unit) => "",
            (rule: IRulexSR) => rule.range,
            (rule: IRulexSR) => rule.runeUsages.map(u => (u.rune, u.range)),
            err.failedSolve)
        }
        case WrongNumArgsForTemplateA(range, expectedNumArgs, actualNumArgs) => {
          ": Expected " + expectedNumArgs + " template args but received " + actualNumArgs + "\n"
        }
      }
    assembleError(filenamesAndSources, err.range, errorStrBody)
  }
}
