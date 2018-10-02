#pragma once

#include <cstdint>

#include "value.hpp"

namespace gra {
    // A heap is used for allocating memory for values. Heaps may feature
    // garbage collection as well. This is an abstract class; you can write
    // your own allocators and garbage collectors, and specify one when
    // spawning a new thread in user code.
    class heap {
    public:
        virtual ~heap();

        // Wrapper for the unary allocate function. This wrapper computes the
        // size of the value based on the pointer count and the auxiliary size.
        value* allocate(
            std::uint64_t pointerCount,
            std::uint64_t auxiliarySize
        );

        // Allocate memory, keeping track of it for garbage collection.
        virtual void* allocate(std::uint64_t size) = 0;
    };
}
