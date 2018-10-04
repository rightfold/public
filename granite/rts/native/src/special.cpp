#include "special.hpp"

#include <algorithm>
#include <cstring>

// See the corresponding macro in special.hpp for more information.
#define GRA_POD_WRAPPER(name, pod)                                          \
    __attribute__((always_inline))                                          \
    gra::value* gra::construct ## name(heap& heap, pod unboxed) {           \
        auto boxed = heap.allocate(0, sizeof(unboxed));                     \
        std::memcpy(boxed->auxiliary(), &unboxed, sizeof(unboxed));         \
        return boxed;                                                       \
    }                                                                       \
                                                                            \
    __attribute__((always_inline))                                          \
    pod gra::read ## name(value* boxed) {                                   \
        pod unboxed;                                                        \
        std::memcpy(&unboxed, boxed->auxiliary(), sizeof(unboxed));         \
        return unboxed;                                                     \
    }

////////////////////////////////////////////////////////////////////////////////
// Integers

GRA_POD_WRAPPER(U8,  std::uint8_t);
GRA_POD_WRAPPER(U16, std::uint16_t);
GRA_POD_WRAPPER(U32, std::uint32_t);
GRA_POD_WRAPPER(U64, std::uint64_t);

////////////////////////////////////////////////////////////////////////////////
// Pointers

GRA_POD_WRAPPER(Pointer, void*);

////////////////////////////////////////////////////////////////////////////////
// Lambdas

__attribute__((always_inline))
gra::value* gra::constructLambda(
    heap& heap,
    lambdaCode* code,
    value** captures,
    std::uint64_t captureCount
) {
    auto lambda = heap.allocate(captureCount, sizeof(code));
    std::copy(captures, captures + captureCount, lambda->pointers());
    std::memcpy(lambda->auxiliary(), &code, sizeof(code));
    return lambda;
}

__attribute__((always_inline))
gra::value* gra::callLambda(heap& heap, value* lambda, value* argument) {
    lambdaCode* code;
    std::memcpy(&code, lambda->auxiliary(), sizeof(code));
    return code(heap, lambda, argument);
}

////////////////////////////////////////////////////////////////////////////////
// Effs

__attribute__((always_inline))
gra::value* gra::constructEff(
    heap& heap,
    effCode* code,
    value** captures,
    std::uint64_t captureCount
) {
    auto eff = heap.allocate(captureCount, sizeof(code));
    std::copy(captures, captures + captureCount, eff->pointers());
    std::memcpy(eff->auxiliary(), &code, sizeof(code));
    return eff;
}

__attribute__((always_inline))
gra::value* gra::performEff(heap& heap, value* eff) {
    effCode* code;
    std::memcpy(&code, eff->auxiliary(), sizeof(code));
    return code(heap, eff);
}

#undef GRA_POD_WRAPPER
