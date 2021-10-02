package net.verdagon.vale.astronomer

import net.verdagon.vale.FileCoordinateMap
import net.verdagon.vale.SourceCodeUtils.{humanizePos, lineContaining, nextThingAndRestOfLine}
import net.verdagon.vale.scout.{RangeS, RuneTypeSolveError, ScoutErrorHumanizer}

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
    val RuneTypeSolveError(rules) = err
    ": Couldn't solve generics rules:\n" + lineContaining(filenamesAndSources, range.file, range.begin.offset) + "\n" + rules.toString
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
        case CouldntSolveRulesA(range, rules) => {
          ": Couldn't solve generics rules:\n" + lineContaining(filenamesAndSources, range.file, range.begin.offset) + "\n" + rules.toString
        }
        case WrongNumArgsForTemplateA(range, expectedNumArgs, actualNumArgs) => {
          ": Expected " + expectedNumArgs + " template args but received " + actualNumArgs + "\n"
        }
      }
    assembleError(filenamesAndSources, err.range, errorStrBody)
  }
}
