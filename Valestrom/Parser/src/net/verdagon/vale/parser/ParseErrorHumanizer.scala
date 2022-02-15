package net.verdagon.vale.parser

import net.verdagon.vale.{CodeLocationS, FileCoordinate, FileCoordinateMap}
import net.verdagon.vale.SourceCodeUtils.humanizePos
import net.verdagon.vale.SourceCodeUtils.nextThingAndRestOfLine

object ParseErrorHumanizer {
  def humanize(
      fileMap: FileCoordinateMap[String],
      fileCoord: FileCoordinate,
      err: IParseError):
  String = {
    val errorStrBody =
      err match {
        case CombinatorParseError(pos, msg) => msg
        case BadStringInterpolationEnd(pos) => "Bad string interpolation end."
        case CantUseBreakInExpression(pos) => "Can't use break inside an expression."
        case CantUseReturnInExpression(pos) => "Can't use return inside an expression."
        case BadInterfaceHeader(pos) => "Bad interface header."
        case BadStartOfBlock(pos) => "Bad start of block."
        case DontNeedSemicolon(pos) => "Dont need semicolon."
        case BadDot(pos) => "Bad dot."
        case BadImplFor(pos) => "Bad impl, expected `for`"
        case BadForeachInError(pos) => "Bad foreach, expected `in`."
        case BadAttributeError(pos) => "Bad attribute."
        case BadStructContentsBegin(pos) => "Bad start of struct contents."
        case BadInterfaceMember(pos) => "Bad interface member."
        case BadStringChar(stringBeginPos, pos) => "Bad string character, " + humanizePos(fileMap, CodeLocationS(fileCoord, stringBeginPos)) + "-" + humanizePos(fileMap, CodeLocationS(fileCoord, pos))
        case BadExpressionBegin(pos) => "Bad start of expression."
        case NeedWhitespaceAroundBinaryOperator(pos) => "Need whitespace around binary operator."
        case UnknownTupleOrSubExpression(pos) => "Saw ( but expression is neither tuple nor sub-expression."
        case NeedSemicolon(pos) => "Need semicolon."
        case BadStructMember(pos) => "Bad struct member."
        case BadBinaryFunctionName(pos) => "Bad binary function name."
//        case CombinatorParseError(pos, msg) => "Internal parser error: " + msg + ":\n"
        case UnrecognizedTopLevelThingError(pos) => "expected func, struct, interface, impl, import, or export, but found:\n"
        case BadFunctionBodyError(pos) => "expected a function body, or `;` to note there is none. Found:\n"
        case BadStartOfStatementError(pos) => "expected `}` to end the block, but found:\n"
        case BadExpressionEnd(pos) => "expected `;` or `}` after expression, but found:\n"
        case IfBlocksMustBothOrNeitherReturn(pos) => "If blocks should either both return, or neither return."
        case ForgotSetKeyword(pos) => "Need `set` keyword to mutate a variable that already exists."
        case BadImport(pos, cause) => "bad import:\n" + cause.toString + "\n"
        case BadStartOfWhileCondition(pos) => "Bad start of while condition, expected ("
        case BadEndOfWhileCondition(pos) => "Bad end of while condition, expected )"
        case BadStartOfWhileBody(pos) => "Bad start of while body, expected {"
        case BadEndOfWhileBody(pos) => "Bad end of while body, expected }"
        case BadStartOfIfCondition(pos) => "Bad start of if condition, expected ("
        case BadEndOfIfCondition(pos) => "Bad end of if condition, expected )"
        case BadStartOfIfBody(pos) => "Bad start of if body, expected {"
        case BadEndOfIfBody(pos) => "Bad end of if body, expected }"
        case BadStartOfElseBody(pos) => "Bad start of else body, expected {"
        case BadEndOfElseBody(pos) => "Bad end of else body, expected }"
        case BadLetEqualsError(pos) => "Expected = after declarations"
        case BadMutateEqualsError(pos) => "Expected = after set destination"
        case BadLetEndError(pos) => "Expected ; after declarations source"
        case BadWhileCondition(pos, cause) => "Parse error somewhere inside this while condition. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadArraySizerEnd(pos) => "Bad array sizer; expected ]"
        case BadWhileBody(pos, cause) => "Parse error somewhere inside this while body. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadIfCondition(pos, cause) => "Parse error somewhere inside this if condition. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadIfBody(pos, cause) => "Parse error somewhere inside this if body. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadElseBody(pos, cause) => "Parse error somewhere inside this else body. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadStruct(pos, cause) => "Parse error somewhere inside this struct. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadInterface(pos, cause) => "Parse error somewhere inside this interface. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadImpl(pos, cause) => "Parse error somewhere inside this impl. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadFunctionHeaderError(pos, cause) => "Parse error somewhere inside this function header. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadEachError(pos, cause) => "Parse error somewhere inside this each/eachI. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadBlockError(pos, cause) => "Parse error somewhere inside this block. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadResultError(pos, cause) => "Parse error somewhere in this result expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadReturnError(pos, cause) => "Parse error somewhere inside this return expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadDestructError(pos, cause) => "Parse error somewhere inside this destruct expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadStandaloneExpressionError(pos, cause) => "Parse error somewhere inside this expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadMutDestinationError(pos, cause) => "Parse error somewhere inside this set destination expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadMutSourceError(pos, cause) => "Parse error somewhere inside this set source expression. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadLetDestinationError(pos, cause) => "Parse error somewhere inside this let destination pattern. Imprecise inner error: " + humanizeCombinatorParseError(fileMap, fileCoord, cause)
        case BadLetSourceError(pos, cause) => "Parse error somewhere inside this let source expression. Imprecise inner error: " + humanize(fileMap, fileCoord, cause)
      }
    val posStr = humanizePos(fileMap, CodeLocationS(fileCoord, err.pos))
    val nextStuff = nextThingAndRestOfLine(fileMap, fileCoord, err.pos)
    f"${posStr} error ${err.errorId}: ${errorStrBody}\n${nextStuff}\n"
  }

  def humanizeCombinatorParseError(
    fileMap: FileCoordinateMap[String],
    fileCoord: FileCoordinate,
    cpe: CombinatorParseError) = {

    val CombinatorParseError(pos, msg) = cpe
    msg + " when trying to parse:"
  }
}
