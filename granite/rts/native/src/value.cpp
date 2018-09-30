#include "value.hpp"

__attribute__((always_inline))
gra::value::value(std::uint64_t pointerCount, std::uint64_t auxiliarySize)
    : _pointerCount(pointerCount)
    , _auxiliarySize(auxiliarySize)
{
}

__attribute__((always_inline))
std::uint64_t gra::value::pointerCount() const {
    return _pointerCount;
}

__attribute__((always_inline))
gra::value** gra::value::pointers() {
    return _pointers;
}

__attribute__((always_inline))
std::uint64_t gra::value::auxiliarySize() const {
    return _auxiliarySize;
}

__attribute__((always_inline))
void* gra::value::auxiliary() {
    return _pointers + _pointerCount;
}

__attribute__((always_inline))
std::uint64_t gra::value::size(
    std::uint64_t pointerCount,
    std::uint64_t auxiliarySize
) {
    return sizeof(value) + sizeof(value*) * pointerCount + auxiliarySize;
}
