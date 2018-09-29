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

uint64_t graRtsPointerCount(struct graRtsValue* v) {
    return v->pointerCount;
}

struct graRtsValue **graRtsPointers(struct graRtsValue* v) {
    return v->pointers;
}

////////////////////////////////////////////////////////////////////////////////
// Auxiliary

uint64_t graRtsAuxiliarySize(struct graRtsValue* v) {
    return v->auxiliarySize;
}

void *graRtsAuxiliary(struct graRtsValue* v) {
    return v->pointers + v->pointerCount;
}
