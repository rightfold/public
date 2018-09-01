#include "tally.hpp"

klok::tally_ui::tally_ui()
    : count(0)
{
}

void klok::tally_ui::render(screen& s) const
{
    unsigned long i = 0;
    for (decltype(s.width()) x = 0; x < s.width(); ++x)
        for (decltype(s.height()) y = 0; y < s.height(); ++y)
            s.pixel(x, y, ++i <= count ? color::white : color::black);
}

void klok::tally_ui::press()
{
    ++count;
}
