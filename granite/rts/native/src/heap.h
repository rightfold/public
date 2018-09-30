#pragma once

#include <stdint.h>

#include "value.h"

typedef struct graRtsHeap graRtsHeap;

graRtsValue *graRtsAllocate(graRtsHeap*, uint64_t, uint64_t);
