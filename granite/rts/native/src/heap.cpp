#include "heap.hpp"

#include <new>

gra::heap::~heap() = default;

__attribute__((always_inline))
gra::value* gra::heap::allocate(
    std::uint64_t pointerCount,
    std::uint64_t auxiliarySize
) {
    auto size = value::size(pointerCount, auxiliarySize);
    auto memory = allocate(size);
    return new (memory) value(pointerCount, auxiliarySize);
}
