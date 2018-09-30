#pragma once

#include "heap.h"
#include "value.h"

typedef graRtsValue* graRtsLambdaCode(graRtsHeap*, graRtsValue*, graRtsValue*);

graRtsValue* graRtsCallLambda(graRtsHeap*, graRtsValue*, graRtsValue*);
