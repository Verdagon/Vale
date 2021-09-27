package net.verdagon.vale

object SourceCodeUtils {
  def humanizeFile(coordinate: FileCoordinate): String = {
    val FileCoordinate(module, packages, filepath) = coordinate
    if (coordinate.isInternal) {
      "(internal file)"
    } else {
      module + packages.map("." + _).mkString("") + ":" + filepath
    }
  }

  def humanizePos(
    filenamesAndSources: FileCoordinateMap[String],
    file: FileCoordinate,
    pos: Int):
  String = {
    if (file.isInternal) {
      return humanizeFile(file)
    }
    val source = filenamesAndSources(file)

    var line = 0
    var lineBegin = 0
    var i = 0
    while (i < pos) {
      if (source(i) == '\n') {
        lineBegin = i + 1
        line = line + 1
      }
      i = i + 1
    }
    humanizeFile(file) + ":" + (line + 1) + ":" + (i - lineBegin + 1)
  }

  def nextThingAndRestOfLine(
      filenamesAndSources: FileCoordinateMap[String],
      file: FileCoordinate,
      position: Int) = {
    val text = filenamesAndSources(file)
    // TODO: can optimize this
    text.slice(position, text.length).trim().split("\\n")(0).trim()
  }

  def lineRangeContaining(
    filenamesAndSources: FileCoordinateMap[String],
    file: FileCoordinate,
    position: Int):
  (Int, Int) = {
    if (file.isInternal) {
      return (-1, 0)
    }
    val text = filenamesAndSources(file)
    // TODO: can optimize this perhaps
    var lineBegin = 0;
    while (lineBegin < text.length) {
      val lineEnd =
        text.indexOf('\n', lineBegin) match {
          case -1 => text.length
          case other => other
        }
      if (lineBegin <= position && position <= lineEnd) {
        return (lineBegin, lineEnd)
      }
      lineBegin = lineEnd + 1
    }
    vfail()
  }

  def lineContaining(
      filenamesAndSources: FileCoordinateMap[String],
      file: FileCoordinate,
      position: Int):
  String = {
    if (file.isInternal) {
      return humanizeFile(file)
    }
    val (lineBegin, lineEnd) = lineRangeContaining(filenamesAndSources, file, position)
    val text = filenamesAndSources(file)
    text.substring(lineBegin, lineEnd)
  }
}
