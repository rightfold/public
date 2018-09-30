#include "panic.hpp"

#include <cstdlib>
#include <unistd.h>

[[noreturn]] void gra::panic(unsigned char const* data, std::uint64_t size) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    ::write(2, data, size);
#pragma GCC diagnostic pop
    std::_Exit(1);
}
