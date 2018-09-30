#pragma once

#include <cstdint>

#include "heap.hpp"
#include "value.hpp"

namespace gra {
    using lambdaCode = value*(heap&, value*, value*);
    value* constructLambda(heap&, lambdaCode*, value**, std::uint64_t);
    value* callLambda(heap&, value*, value*);
}
