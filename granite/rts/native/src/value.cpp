#include "value.hpp"

gra::value::value(std::uint64_t pointerCount, std::uint64_t auxiliarySize)
    : _pointerCount(pointerCount)
    , _auxiliarySize(auxiliarySize)
{
}

std::uint64_t gra::value::pointerCount() const {
    return _pointerCount;
}

gra::value** gra::value::pointers() {
    return _pointers;
}

std::uint64_t gra::value::auxiliarySize() const {
    return _auxiliarySize;
}

void* gra::value::auxiliary() {
    return _pointers + _pointerCount;
}

std::uint64_t gra::value::size(
    std::uint64_t pointerCount,
    std::uint64_t auxiliarySize
) {
    return sizeof(value) + sizeof(value*) * pointerCount + auxiliarySize;
}
