#include "value.h"

////////////////////////////////////////////////////////////////////////////////
// Data type

struct graRtsValue {
    uint64_t pointerCount;
    uint64_t auxiliarySize;
    struct graRtsValue *pointers[];
};

////////////////////////////////////////////////////////////////////////////////
// Pointers

uint64_t graRtsPointerCount(graRtsValue* v) {
    return v->pointerCount;
}

graRtsValue **graRtsPointers(graRtsValue* v) {
    return v->pointers;
}

////////////////////////////////////////////////////////////////////////////////
// Auxiliary

uint64_t graRtsAuxiliarySize(graRtsValue* v) {
    return v->auxiliarySize;
}

void *graRtsAuxiliary(graRtsValue* v) {
    return v->pointers + v->pointerCount;
}
