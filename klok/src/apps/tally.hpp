#pragma once

#include "../core/ui.hpp"

namespace klok
{
    class tally_ui : public ui
    {
    public:
        tally_ui();

        void render(screen&) const override;
        void press() override;

    private:
        unsigned long count;
    };
}
