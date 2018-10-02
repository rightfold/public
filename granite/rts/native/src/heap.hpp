#pragma once

#include <cstdint>

#include "value.hpp"

namespace gra {
    class heap {
    public:
        virtual ~heap();

        value* allocate(
            std::uint64_t pointerCount,
            std::uint64_t auxiliarySize
        );

        virtual void* allocate(std::uint64_t size) = 0;
    };
}
