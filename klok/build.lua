-- TODO: Pass -DF_CPU=... and -mmcu=... to avr-gcc.
-- TODO: Link the objects using avr-gcc.
-- TODO: Invoke avr-objcopy.

local CXXFLAGS = {
    "-std=c++14",

    "-Wall",
    "-Wextra",
    "-Wpedantic",
    "-Werror",

    "-flto",
}

local function cxx_object(rule)
    local inputs = { ":avr-gcc" }
    table.insert(inputs, rule.source)
    for _, include in ipairs(rule.includes) do
        table.insert(inputs, include)
    end

    local args = { table.unpack(CXXFLAGS) }
    table.insert(args, [[-c]])
    table.insert(args, [[-o]])
    table.insert(args, [["$(loc ]] .. rule.output .. [[)"]])
    table.insert(args, [["$(loc ]] .. rule.source .. [[)"]])

    genrule {
        name = rule.name,
        inputs = inputs,
        outputs = { rule.output },
        command = [[
            export PATH="$PATH:$(loc :avr-gcc bin)"
            avr-gcc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

cxx_object {
    name = "klok-lib-apps-tally",
    source = "+klok/src/apps/tally.cpp",
    includes = {
        "+klok/src/apps/tally.hpp",
        "+klok/src/core/screen.hpp",
        "+klok/src/core/ui.hpp",
    },
    output = "@tally.o",
}

cxx_object {
    name = "klok-lib-core-screen",
    source = "+klok/src/core/screen.cpp",
    includes = {
        "+klok/src/core/screen.hpp",
    },
    output = "@screen.o",
}

cxx_object {
    name = "klok-lib-core-ui",
    source = "+klok/src/core/ui.cpp",
    includes = {
        "+klok/src/core/screen.hpp",
        "+klok/src/core/ui.hpp",
    },
    output = "@ui.o",
}
