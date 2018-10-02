#pragma once

////////////////////////////////////////////////////////////////////////////////
// Special values
//
// This header contains functions for manipulating values that have layouts
// known by the RTS.

#include <cstdint>

#include "heap.hpp"
#include "value.hpp"

namespace gra {

#define GRA_POD_WRAPPER(name, pod)                                          \
    value* construct ## name(heap&, pod);                                   \
    pod read ## name(value*);

    ////////////////////////////////////////////////////////////////////////////
    // Integers

    GRA_POD_WRAPPER(U8,  std::uint8_t);
    GRA_POD_WRAPPER(U16, std::uint16_t);
    GRA_POD_WRAPPER(U32, std::uint32_t);
    GRA_POD_WRAPPER(U64, std::uint64_t);

    ////////////////////////////////////////////////////////////////////////////
    // Pointers

    GRA_POD_WRAPPER(Pointer, void*);

    ////////////////////////////////////////////////////////////////////////////
    // Lambdas

    using lambdaCode = value*(heap&, value*, value*);
    value* constructLambda(heap&, lambdaCode*, value**, std::uint64_t);
    value* callLambda(heap&, value*, value*);

#undef GRA_POD_WRAPPER

}
