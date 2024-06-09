//
// Created by Evan Ovadia on 6/3/24.
//

#include "rustify.h"

std::string rustifySimpleIdStep(SimpleIdStep* simpleIdStep);

// TODO: optimize: use a string builder
std::string rustifySimpleId(SimpleId* simpleId, bool ignoreFirst) {
  bool ignoreNext = ignoreFirst;
  std::string stepsStr;
  for (auto step : simpleId->steps) {
    if (ignoreNext) {
      ignoreNext = false;
      continue;
    }
    if (!stepsStr.empty()) {
      stepsStr += "::";
    }
    stepsStr += rustifySimpleIdStep(step);
  }
  return stepsStr;
}
std::string rustifySimpleIdStep(SimpleIdStep* simpleIdStep) {
  if (simpleIdStep->name == "&" || simpleIdStep->name == "&mut") {
    assert(simpleIdStep->templateArgs.size() == 1);
    return simpleIdStep->name + rustifySimpleId(simpleIdStep->templateArgs[0], false);
  }
  std::string result = simpleIdStep->name;
  if (!simpleIdStep->templateArgs.empty()) {
    std::string templateArgsInnerStr;
    for (auto templateArg : simpleIdStep->templateArgs) {
      if (!templateArgsInnerStr.empty()) {
        templateArgsInnerStr += ", ";
      }
      templateArgsInnerStr += rustifySimpleId(templateArg, false);
    }
    result += "<";
    result += templateArgsInnerStr;
    result += ">";
  }
  return result;
}
