#ifndef METAL_CACHE_H_
#define METAL_CACHE_H_

#include <unordered_map>

#include "metal/types.h"
#include "metal/ast.h"
#include "instructions.h"

namespace std {
    template<>
    struct hash<Location> {
        inline size_t operator()(Location location) const {
            return (size_t)location;
        }
    };
    template<>
    struct hash<Ownership> {
        inline size_t operator()(Ownership ownership) const {
            return (size_t)ownership;
        }
    };
    template<>
    struct hash<Mutability> {
        inline size_t operator()(Mutability mutability) const {
            return (size_t)mutability;
        }
    };
}

template<typename K, typename V, typename H, typename E, typename F>
V makeIfNotPresent(std::unordered_map<K, V, H, E>* map, const K& key, F&& makeElement) {
  auto iter = map->find(key);
  if (iter == map->end()) {
    auto p = map->emplace(key, makeElement());
    iter = p.first;
  }
  return iter->second;
}

struct HashRefVec {
  size_t operator()(const std::vector<Reference *> &refs) const {
    size_t result = 1337;
    for (auto el : refs) {
      result += (size_t) el;
    }
    return result;
  }
};
struct RefVecEquals {
  bool operator()(
      const std::vector<Reference *> &a,
      const std::vector<Reference *> &b) const {
    if (a.size() != b.size())
      return false;
    for (size_t i = 0; i < a.size(); i++) {
      if (a[i] != b[i])
        return false;
    }
    return true;
  }
};

class MetalCache {
public:
  MetalCache() {
//    i8Ref = getReference(Ownership::SHARE, Location::INLINE, i8);
    intRef = getReference(Ownership::SHARE, Location::INLINE, innt);
    floatRef = getReference(Ownership::SHARE, Location::INLINE, flooat);
    boolRef = getReference(Ownership::SHARE, Location::INLINE, boool);
    strRef = getReference(Ownership::SHARE, Location::YONDER, str);
    neverRef = getReference(Ownership::SHARE, Location::INLINE, never);
    regionReferend = getStructReferend(getName("__Region"));
    regionRef = getReference(Ownership::SHARE, Location::YONDER, regionReferend);
  }

//  I8* i8 = new I8();
//  Reference* i8Ref = nullptr;
  Int* innt = new Int();
  Reference* intRef = nullptr;
  Bool* boool = new Bool();
  Reference* boolRef = nullptr;
  Float* flooat = new Float();
  Reference* floatRef = nullptr;
  Str* str = new Str();
  Reference* strRef = nullptr;
  Never* never = new Never();
  Reference* neverRef = nullptr;
  StructReferend* emptyTupleStruct = nullptr;
  Reference* emptyTupleStructRef = nullptr;
  // This is a central referend that holds a region's data.
  // These will hold for example the bump pointer for an arena region,
  // or a free list pointer for HGM.
  // We hand these in to methods like allocate, deallocate, etc.
  // Right now we just use it to hold the bump pointer for linear regions.
  // Otherwise, for now, we're just handing in Nevers.
  StructReferend* regionReferend = nullptr;
  Reference* regionRef = nullptr;

  std::unordered_map<Name*, StructReferend*> structReferends;
  std::unordered_map<Name*, InterfaceReferend*> interfaceReferends;
  std::unordered_map<std::string, Name*> names;

  // This is conceptually a map<[Reference*, Mutability], RawArrayT*>.
  std::unordered_map<Reference*, std::unordered_map<Mutability, RawArrayT*>> rawArrays;
  std::unordered_map<Name*, UnknownSizeArrayT*> unknownSizeArrays;
  std::unordered_map<Name*, KnownSizeArrayT*> knownSizeArrays;
  std::unordered_map<Referend*, std::unordered_map<Ownership, std::unordered_map<Location, Reference*>>> unconvertedReferences;
  std::unordered_map<Name*, std::unordered_map<Reference*, std::unordered_map<std::vector<Reference*>, Prototype*, HashRefVec, RefVecEquals>>> prototypes;
  std::unordered_map<int, std::unordered_map<std::string, VariableId*>> variableIds;
  std::unordered_map<VariableId*, std::unordered_map<Reference*, Local*>> locals;

  RawArrayT* getArray(Mutability mutability, Reference* elementType) {
    return makeIfNotPresent(
        &rawArrays[elementType],
        mutability,
        [&](){ return new RawArrayT(mutability, elementType); });
  }

  StructReferend* getStructReferend(Name* structName) {
    return makeIfNotPresent(
        &structReferends,
        structName,
        [&]() { return new StructReferend(structName); });
  }

  UnknownSizeArrayT* getUnknownSizeArray(Name* name, RawArrayT* rawArray) {
    return makeIfNotPresent(
        &unknownSizeArrays,
        name,
        [&](){ return new UnknownSizeArrayT(name, rawArray); });
  }

  KnownSizeArrayT* getKnownSizeArray(Name* name, int size, RawArrayT* rawArray) {
    return makeIfNotPresent(
        &knownSizeArrays,
        name,
        [&](){ return new KnownSizeArrayT(name, size, rawArray); });
  }

  Name* getName(std::string nameStr) {
    return makeIfNotPresent(
        &names,
        nameStr,
        [&](){ return new Name(nameStr); });
  }

  Reference* getReference(Ownership ownership, Location location, Referend* referend) {
    return makeIfNotPresent<Location, Reference*>(
        &unconvertedReferences[referend][ownership],
        location,
        [&](){ return new Reference(ownership, location, referend); });
  }

  Prototype* getPrototype(Name* name, Reference* returnType, std::vector<Reference*> paramTypes) {
    return makeIfNotPresent(
        &prototypes[name][returnType],
        paramTypes,
        [&](){ return new Prototype(name, paramTypes, returnType); });
  }
};

#endif
