#include "heap.h"

struct graRtsHeap {
    graRtsValue *(*allocate)(graRtsHeap*, uint64_t, uint64_t);
};

graRtsValue *graRtsAllocate(graRtsHeap *heap,
                            uint64_t pointerCount,
                            uint64_t auxiliarySize) {
    return heap->allocate(heap, pointerCount, auxiliarySize);
}
