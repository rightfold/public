#include "lambda.h"

#include <string.h>

graRtsValue* graRtsCallLambda(graRtsHeap *heap,
                              graRtsValue *lambda,
                              graRtsValue *argument) {
    graRtsLambdaCode* code;
    memcpy(&code, graRtsAuxiliary(lambda), sizeof(code));
    return code(heap, lambda, argument);
}
