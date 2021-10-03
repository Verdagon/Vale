package net.verdagon.vale.solver

import net.verdagon.vale.SourceCodeUtils.{lineContaining, lineRangeContaining}
import net.verdagon.vale.{CodeLocationS, FileCoordinateMap, RangeS, repeatStr}

object SolverErrorHumanizer {
  def humanizeFailedSolve[Rule, RuneID, Conclusion, ErrType](
    codeMap: FileCoordinateMap[String],
    humanizeRune: RuneID => String,
    humanizeTemplata: (FileCoordinateMap[String], Conclusion) => String,
    humanizeRuleError: (FileCoordinateMap[String], ErrType) => String,
    getRuleRange: (Rule) => RangeS,
    getRuneUsages: (Rule) => Iterable[(RuneID, RangeS)],
    result: IIncompleteOrFailedSolve[Rule, RuneID, Conclusion, ErrType]):
  String = {
    val errorBody =
      (result match {
        case IncompleteSolve(incompleteConclusions, unsolvedRules, unknownRunes) => {
          "Couldn't solve some runes:"  + unknownRunes.toVector.map(humanizeRune).mkString(", ")
        }
        case FailedSolve(incompleteConclusions, unsolvedRules, error) => {
          error match {
            case SolverConflict(rule, rune, previousConclusion, newConclusion) => {
              "Conflict, thought rune " + humanizeRune(rune) + " was " + humanizeTemplata(codeMap, previousConclusion) + " but now concluding it's " + humanizeTemplata(codeMap, newConclusion)
            }
            case RuleError(ruleIndex, err) => {
              humanizeRuleError(codeMap, err) + "\n"
            }
          }
        }
      })

    val unsolvedRules = result.unsolvedRules
    val allLineBeginLocs =
      unsolvedRules.flatMap(rule => {
        val ruleBeginLineBegin = lineRangeContaining(codeMap, getRuleRange(rule).file, getRuleRange(rule).begin.offset)._1
        val ruleEndLineBegin = lineRangeContaining(codeMap, getRuleRange(rule).file, getRuleRange(rule).end.offset)._1
        ruleBeginLineBegin.to(ruleEndLineBegin).map(lineBegin => CodeLocationS(getRuleRange(rule).file, lineBegin))
      })
        .distinct
    val allRuneUsages = unsolvedRules.flatMap(getRuneUsages).distinct
    val lineBeginLocToRuneUsage =
      allRuneUsages
        .map(runeUsage => {
          val usageBeginLine = lineRangeContaining(codeMap, runeUsage._2.file, runeUsage._2.begin.offset)._1
          (usageBeginLine, runeUsage)
        })
        .groupBy(_._1)
        .mapValues(_.map(_._2))

    errorBody + "\n" +
      allLineBeginLocs
        // Show the lines in order
        .sortBy(_.offset)
        .map({ case CodeLocationS(file, lineBegin) =>
          lineContaining(codeMap, file, lineBegin) + "\n" +
            lineBeginLocToRuneUsage
              .getOrElse(lineBegin, Vector())
              // Show the runes from right to left
              .sortBy(-_._2.begin.offset)
              .map({ case (rune, range) =>
                val numSpaces = range.begin.offset - lineBegin
                val numArrows = range.end.offset - range.begin.offset
                repeatStr(" ", numSpaces) + repeatStr("^", numArrows) + " " +
                  result.incompleteConclusions.get(rune).map(humanizeTemplata(codeMap, _)).getOrElse("(unknown)") +
                  " (" + humanizeRune(rune) + ")" +
                  "\n"
              }).mkString("")
        }).mkString("")
  }

}
