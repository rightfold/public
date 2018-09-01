#include "screen.hpp"

unsigned int klok::screen::width() const
{
    return width_;
}

unsigned int klok::screen::height() const
{
    return height_;
}

void klok::screen::pixel(unsigned int x, unsigned int y, color c)
{
    switch (c)
    {
    case color::white:
        buffer[x + y / CHAR_BIT * width()] |=  (1 << (y & 7));
        break;

    case color::black:
        buffer[x + y / CHAR_BIT * width()] &= ~(1 << (y & 7));
        break;
    }
}

void klok::screen::flush()
{
}
