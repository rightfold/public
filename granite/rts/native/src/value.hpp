#pragma once

#include <cstdint>

namespace gra {
    class value {
    public:
        std::uint64_t pointerCount() const;
        value** pointers();

        std::uint64_t auxiliarySize() const;
        void* auxiliary();

    private:
        std::uint64_t _pointerCount;
        std::uint64_t _auxiliarySize;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        value* _pointers[];
#pragma GCC diagnostic pop
    };
}
