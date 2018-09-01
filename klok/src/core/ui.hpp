#pragma once

#include "screen.hpp"

namespace klok
{
    class ui
    {
    public:
        virtual ~ui();
        virtual void render(screen&) const = 0;
        virtual void up();
        virtual void down();
        virtual void press();
    };
}
