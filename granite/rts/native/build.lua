local cxx_objects = { }

local function translation_unit(rule)
    local inputs = { ":clang", ":libcxx" }
    local args = { }

    table.insert(args, [[-std=c++17]])
    table.insert(args, [[-Wall]])
    table.insert(args, [[-Wextra]])
    table.insert(args, [[-Wpedantic]])
    table.insert(args, [[-Werror]])

    table.insert(args, [[-c]])
    table.insert(args, [[-emit-llvm]])

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc ]] .. rule.output .. [[)"]])

    table.insert(args, [[-isystem"$(loc :libcxx include/c++/v1)"]])

    table.insert(inputs, rule.source)
    table.insert(args, [["$(loc ]] .. rule.source .. [[)"]])

    for _, include in ipairs(rule.includes) do
        table.insert(inputs, include)
    end

    genrule {
        name = "granite-rts-native-lib-" .. rule.name,
        inputs = inputs,
        outputs = { rule.output },
        command = [[
            export PATH="$PATH:$(loc :clang bin)"
            clang++ ]] .. table.concat(args, " ") .. [[
        ]],
    }

    local outputs = { }
    parse_output_label(rule.output, outputs)
    table.insert(cxx_objects, ":granite-rts-native-lib-" .. rule.name .. " " .. outputs[1])
end

translation_unit {
    name = "heap",
    source = "+granite/rts/native/src/heap.cpp",
    includes = {
        "+granite/rts/native/src/heap.hpp",
        "+granite/rts/native/src/value.hpp",
    },
    output = "@heap.bc",
}

translation_unit {
    name = "special",
    source = "+granite/rts/native/src/special.cpp",
    includes = {
        "+granite/rts/native/src/heap.hpp",
        "+granite/rts/native/src/special.hpp",
        "+granite/rts/native/src/value.hpp",
    },
    output = "@special.bc",
}

translation_unit {
    name = "panic",
    source = "+granite/rts/native/src/panic.cpp",
    includes = {
        "+granite/rts/native/src/panic.hpp",
    },
    output = "@panic.bc",
}

translation_unit {
    name = "value",
    source = "+granite/rts/native/src/value.cpp",
    includes = {
        "+granite/rts/native/src/value.hpp",
    },
    output = "@value.bc",
}

do
    local inputs = { ":llvm" }
    local args = { }

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc @libgraniterts.bc)"]])

    for _, object in ipairs(cxx_objects) do
        table.insert(inputs, object)
        table.insert(args, [["$(loc ]] .. object .. [[)"]])
    end

    genrule {
        name = "granite-rts-native-lib",
        inputs = inputs,
        outputs = { "@libgraniterts.bc" },
        command = [[
            export PATH="$PATH:$(loc :llvm bin)"
            llvm-link ]] .. table.concat(args, " ") .. [[
        ]],
    }
end
