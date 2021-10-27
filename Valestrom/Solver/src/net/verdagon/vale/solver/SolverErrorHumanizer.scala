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
    ruleToString: (Rule) => String,
    result: IIncompleteOrFailedSolve[Rule, RuneID, Conclusion, ErrType]):
  // Returns text and all line begins
  (String, Vector[CodeLocationS]) = {
    val errorBody =
      (result match {
        case IncompleteSolve(incompleteConclusions, unsolvedRules, unknownRunes) => {
          "Couldn't solve some runes: "  + unknownRunes.toVector.map(humanizeRune).mkString(", ")
        }
        case FailedSolve(incompleteConclusions, unsolvedRules, error) => {
          error match {
            case SolverConflict(rune, previousConclusion, newConclusion) => {
              "Conflict, thought rune " + humanizeRune(rune) + " was " + humanizeTemplata(codeMap, previousConclusion) + " but now concluding it's " + humanizeTemplata(codeMap, newConclusion)
            }
            case RuleError(err) => {
              humanizeRuleError(codeMap, err)
            }
          }
        }
      })

    val unsolvedRules = result.unsolvedRules
    val builtinUnsolvedRules = unsolvedRules.filter(getRuleRange(_).file.isInternal)
    val userUnsolvedRules = unsolvedRules.filter(!getRuleRange(_).file.isInternal)

    val allLineBeginLocs =
      userUnsolvedRules.flatMap(rule => {
        val range = getRuleRange(rule)
        val RangeS(begin, end) = range
        val ruleBeginLineBegin = lineRangeContaining(codeMap, begin)._1
        val ruleEndLineBegin = lineRangeContaining(codeMap, end)._1
        ruleBeginLineBegin.to(ruleEndLineBegin).map(lineBegin => CodeLocationS(getRuleRange(rule).file, lineBegin))
      })
        .distinct
    val allRuneUsages = unsolvedRules.flatMap(getRuneUsages).distinct
    val lineBeginLocToRuneUsage =
      allRuneUsages
        .map(runeUsage => {
          val usageBeginLine = lineRangeContaining(codeMap, runeUsage._2.begin)._1
          (usageBeginLine, runeUsage)
        })
        .groupBy(_._1)
        .mapValues(_.map(_._2))

    val textFromUserRules =
      allLineBeginLocs
        // Show the lines in order
        .sortBy(_.offset)
        .map({ case loc @ CodeLocationS(file, lineBegin) =>
          lineContaining(codeMap, loc) + "\n" +
            lineBeginLocToRuneUsage
              .getOrElse(lineBegin, Vector())
              // Show the runes from right to left
              .sortBy(-_._2.begin.offset)
              .map({ case (rune, range) =>
                val numSpaces = range.begin.offset - lineBegin
                val numArrows = range.end.offset - range.begin.offset
                val runeName = humanizeRune(rune)
                (if (runeName.length + 4 < numSpaces) {
                  "  " + runeName + ": " +
                    repeatStr(" ", numSpaces - runeName.length - 4) + repeatStr("^", numArrows) + " "
                } else {
                  repeatStr(" ", numSpaces) + repeatStr("^", numArrows) + " " +
                    runeName + ": "
                }) +
                  result.incompleteConclusions.get(rune).map(humanizeTemplata(codeMap, _)).getOrElse("(unknown)") +
                  "\n"
              }).mkString("")
        }).mkString("")
    val textFromBuiltinRules =
      builtinUnsolvedRules.map(rule => {
        ruleToString(rule) + "\n" +
        getRuneUsages(rule).map({ case (rune, _) =>
        "  " + humanizeRune(rune) + ": " +
            result.incompleteConclusions.get(rune).map(humanizeTemplata(codeMap, _)).getOrElse("(unknown)") + "\n"
        }).mkString("")
      }).mkString("")
    val text = errorBody + "\n" + textFromUserRules + textFromBuiltinRules
    (text, allLineBeginLocs.toVector)
  }

}
