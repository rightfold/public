#pragma once

#include <cstdint>

#include "value.hpp"

namespace gra {
    class heap {
    public:
        virtual ~heap();

        virtual value* allocate(
            std::uint64_t pointerCount,
            std::uint64_t auxiliarySize
        ) = 0;
    };
}
