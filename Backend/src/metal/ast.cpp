#include "ast.h"
#include "../utils/rustify.h"

extern const std::string BUILTIN_PROJECT_NAME = "__vale";

std::string Package::getKindExportName(Kind* kind, bool includeProjectName) const {
  if (auto innt = dynamic_cast<Int *>(kind)) {
    return std::string() + "int" + std::to_string(innt->bits) + "_t";
  } else if (dynamic_cast<Bool *>(kind)) {
    return "int8_t";
  } else if (dynamic_cast<Float *>(kind)) {
    return "double";
  } else if (dynamic_cast<Str *>(kind)) {
    return "ValeStr*";
  } else {
    // DO NOT SUBMIT this is awkward
    std::string thing;
    auto iter1 = kindToExportName.find(kind);
    if (iter1 != kindToExportName.end()) {
      thing = iter1->second;
    } else {
      auto iter2 = kindToExtern.find(kind);
      if (iter2 != kindToExtern.end()) {
        thing = iter2->second->mangledName;
      } else {
        std::cerr << "Couldn't find export name for: " << getKindHumanName(kind) << std::endl;
        exit(1);
      }
    }
    return (includeProjectName && !packageCoordinate->projectName.empty() ? packageCoordinate->projectName + "_" : "") + thing;
  }
}