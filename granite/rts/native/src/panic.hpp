#pragma once

#include <cstdint>

namespace gra {
    [[noreturn]] void panic(unsigned char const*, std::uint64_t);
}
