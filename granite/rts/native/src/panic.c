#include "panic.h"

#include <unistd.h>
#include <stdlib.h>

noreturn void graRtsPanic(unsigned char const* data, uint64_t size) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    write(2, data, size);
#pragma GCC diagnostic pop
    _Exit(1);
}
