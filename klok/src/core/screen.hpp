#pragma once

#include <limits.h>

namespace klok
{
    enum class color : bool
    {
        black = false,
        white = true,
    };

    class screen
    {
    public:
        unsigned int width() const;
        unsigned int height() const;
        void pixel(unsigned int, unsigned int, color);
        void flush();

    private:
        static constexpr unsigned int width_  = 128;
        static constexpr unsigned int height_ = 64;

    protected:
        char buffer[width_ * height_ / CHAR_BIT];
    };
}
