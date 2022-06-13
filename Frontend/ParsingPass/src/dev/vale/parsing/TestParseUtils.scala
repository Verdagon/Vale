package dev.vale.parsing

import dev.vale.lexing.{FailedParse, IParseError, Lexer, LexingIterator}
import dev.vale.{Err, FileCoordinate, FileCoordinateMap, IPackageResolver, Interner, Ok, PackageCoordinate, PackageCoordinateMap, Result, vassertOne, vassertSome, vfail, vimpl}
import dev.vale.options.GlobalOptions
import dev.vale.parsing.ast.{FileP, IDenizenP, IExpressionPE, IRulexPR, ITemplexPT, PatternPP}

import scala.collection.immutable.Map

trait TestParseUtils {
  def compileMaybe[T](parser: (ScrambleIterator) => Result[Option[T], IParseError], untrimpedCode: String): T = {
    vimpl()
//    val interner = new Interner()
//    val code = untrimpedCode.trim()
//    // The trim is in here because things inside the parser don't expect whitespace before and after
//    val iter = new ScrambleIterator(code.trim(), 0)
//    parser(iter) match {
//      case Ok(None) => {
//        vfail("Couldn't parse, not applicable!");
//      }
//      case Err(err) => {
//        vfail(
//          "Couldn't parse!\n" +
//            ParseErrorHumanizer.humanize(
//              FileCoordinateMap.test(interner, code).fileCoordToContents.toMap,
//              FileCoordinate.test(interner),
//              err))
//      }
//      case Ok(Some(result)) => {
//        if (!iter.atEnd()) {
//          vfail("Couldn't parse all of the input. Remaining:\n" + code.slice(iter.getPos(), code.length))
//        }
//        result
//      }
//    }
  }

  def compile[T](parser: (LexingIterator) => Result[T, IParseError], untrimpedCode: String): T = {
    vimpl()
//    val interner = new Interner()
//    val code = untrimpedCode.trim()
//    // The trim is in here because things inside the parser don't expect whitespace before and after
//    val iter = ParsingIterator(code.trim(), 0)
//    parser(iter) match {
//      case Err(err) => {
//        vfail(
//          "Couldn't parse!\n" +
//            ParseErrorHumanizer.humanize(
//              FileCoordinateMap.test(interner, code).fileCoordToContents.toMap,
//              FileCoordinate.test(interner),
//              err))
//      }
//      case Ok(result) => {
//        if (!iter.atEnd()) {
//          vfail("Couldn't parse all of the input. Remaining:\n" + code.slice(iter.getPos(), code.length))
//        }
//        result
//      }
//    }
  }

  def compileForError[T](parser: (ScrambleIterator) => Result[T, IParseError], untrimpedCode: String): IParseError = {
    vimpl()
//    val code = untrimpedCode.trim()
//    // The trim is in here because things inside the parser don't expect whitespace before and after
//    val iter = ParsingIterator(code.trim(), 0)
//    parser(iter) match {
//      case Err(err) => {
//        err
//      }
//      case Ok(result) => {
//        if (!iter.atEnd()) {
//          vfail("Couldn't parse all of the input. Remaining:\n" + code.slice(iter.getPos(), code.length))
//        }
//        vfail("We expected parse to fail, but it succeeded:\n" + result)
//      }
//    }
  }

  def compileForRest[T](parser: (ScrambleIterator) => Result[T, IParseError], untrimpedCode: String, expectedRest: String): Unit = {
    vimpl()
//    val interner = new Interner()
//    val code = untrimpedCode.trim()
//    // The trim is in here because things inside the parser don't expect whitespace before and after
//    val iter = ParsingIterator(code.trim(), 0)
//    parser(iter) match {
//      case Err(err) => {
//        vfail(
//          "Couldn't parse!\n" +
//            ParseErrorHumanizer.humanize(
//              FileCoordinateMap.test(interner, code).fileCoordToContents.toMap,
//              FileCoordinate.test(interner),
//              err))
//      }
//      case Ok(_) => {
//        val rest = iter.code.slice(iter.position, iter.code.length)
//        if (rest != expectedRest) {
//          vfail("Couldn't parse all of the input. Remaining:\n" + code.slice(iter.getPos(), code.length))
//        }
//      }
//    }
  }

  def compileExpression(code: String): IExpressionPE = {
    vimpl()
//    compile(
//      makeExpressionParser()
//        .parseExpression(_, StopBeforeCloseBrace), code)
  }

  def compileTopLevel(code: String): IDenizenP = {
    vimpl()
//    vassertSome(compile(makeParser().parseDenizen(_), code))
  }

  def compileExpressionForError(code: String): IParseError = {
    vimpl()
//    compileForError(
//      makeExpressionParser()
//        .parseBlockContents(_, StopBeforeCloseBrace), code)
  }

  def makeParser(): Parser = {
    vimpl()
//    new Parser(GlobalOptions(true, true, true, true))
  }

  def makeExpressionParser(): ExpressionParser = {
    vimpl()
//    new ExpressionParser(GlobalOptions(true, true, true, true))
  }

  def compileFile(code: String): Result[FileP, FailedParse] = {
    val interner = new Interner()
    val opts = GlobalOptions(true, true, true, true)
    val p = new Parser(interner, opts)
    val codeMap = FileCoordinateMap.test(interner, Vector(code))

    ParseAndExplore.parseAndExploreAndCollect(
      interner,
      opts,
      p,
      Array(PackageCoordinate.TEST_TLD(interner)),
      new IPackageResolver[Map[String, String]]() {
        override def resolve(packageCoord: PackageCoordinate): Option[Map[String, String]] = {
          // For testing the parser, we dont want it to fetch things with import statements
          Some(codeMap.resolve(packageCoord).getOrElse(Map("" -> "")))
        }
      }) match {
      case Err(e) => Err(e)
      case Ok(x) => Ok(vassertOne(x.buildArray().map(_._2)))
    }
  }

  def compileDenizens(code: String): Result[Array[IDenizenP], FailedParse] = {
    compileFile(code) match {
      case Err(e) => Err(e)
      case Ok(x) => Ok(x.denizens)
    }
  }


  def compileDenizen(code: String): Result[IDenizenP, FailedParse] = {
    compileDenizens(code) match {
      case Err(e) => Err(e)
      case Ok(x) => Ok(vassertOne(x))
    }
  }

  def compileExpr(code: String): IExpressionPE = {
    val interner = new Interner()
    val node =
      new Lexer(interner)
        .lexNode(LexingIterator(code), false, false)
        .getOrDie()
    val exprP =
      new ExpressionParser(interner, GlobalOptions(true, false, true, true))
        .parseExpression(node)
        .getOrDie()
    exprP
  }

  def compilePattern(code: String): PatternPP = {
    val interner = new Interner()
    val node =
      new Lexer(interner)
        .lexNode(LexingIterator(code), false, false)
        .getOrDie()
    val exprP =
      vimpl()
//      new PatternParser(interner, GlobalOptions(true, false, true, true))
//        .parseExpression(node)
//        .getOrDie()
    exprP
  }

  def compileTemplex(code: String): ITemplexPT = {
    val interner = new Interner()
    val node =
      new Lexer(interner)
        .lexNode(LexingIterator(code), false, false)
        .getOrDie()
    val exprP =
      vimpl()
    //      new PatternParser(interner, GlobalOptions(true, false, true, true))
    //        .parseExpression(node)
    //        .getOrDie()
    exprP
  }

  def compileRulex(code: String): IRulexPR = {
    val interner = new Interner()
    val node =
      new Lexer(interner)
        .lexNode(LexingIterator(code), false, false)
        .getOrDie()
    val exprP =
      vimpl()
    //      new PatternParser(interner, GlobalOptions(true, false, true, true))
    //        .parseExpression(node)
    //        .getOrDie()
    exprP
  }
}
