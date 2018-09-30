#include "lambda.hpp"

#include <algorithm>
#include <cstring>

__attribute__((always_inline))
gra::value* gra::constructLambda(
    heap& heap,
    lambdaCode* code,
    value** captures,
    std::uint64_t captureCount
) {
    auto lambda = heap.allocate(captureCount, sizeof(code));
    std::copy(captures, captures + captureCount, lambda->pointers());
    std::memcpy(lambda->auxiliary(), &code, sizeof(code));
    return lambda;
}

__attribute__((always_inline))
gra::value* gra::callLambda(heap& heap, value* lambda, value* argument) {
    lambdaCode* code;
    std::memcpy(&code, lambda->auxiliary(), sizeof(code));
    return code(heap, lambda, argument);
}
