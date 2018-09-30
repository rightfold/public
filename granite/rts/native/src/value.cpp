#include "value.hpp"

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
