#pragma once

#include <stdint.h>

#include "heap.h"
#include "value.h"

typedef graRtsValue* graRtsLambdaCode(graRtsHeap*, graRtsValue*, graRtsValue*);

graRtsValue* graRtsConstructLambda(graRtsHeap*, graRtsLambdaCode*,
                                   graRtsValue**, uint64_t);

graRtsValue* graRtsCallLambda(graRtsHeap*, graRtsValue*, graRtsValue*);
