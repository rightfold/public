#include "lambda.h"

#include <string.h>

graRtsValue* graRtsConstructLambda(
    graRtsHeap *heap,
    graRtsLambdaCode *code,
    graRtsValue **captures,
    uint64_t captureCount
) {
    graRtsValue *lambda = graRtsAllocate(heap, captureCount, sizeof(code));
    memcpy(graRtsPointers(lambda), captures, sizeof(graRtsValue *) * captureCount);
    memcpy(graRtsAuxiliary(lambda), &code, sizeof(code));
    return lambda;
}

graRtsValue* graRtsCallLambda(graRtsHeap *heap,
                              graRtsValue *lambda,
                              graRtsValue *argument) {
    graRtsLambdaCode* code;
    memcpy(&code, graRtsAuxiliary(lambda), sizeof(code));
    return code(heap, lambda, argument);
}
