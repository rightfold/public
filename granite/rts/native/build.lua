local function translation_unit(rule)
    local inputs = { ":gcc" }
    local args = { }

    table.insert(args, [[-std=c++17]])
    table.insert(args, [[-Wall]])
    table.insert(args, [[-Wextra]])
    table.insert(args, [[-Wpedantic]])
    table.insert(args, [[-Werror]])

    table.insert(args, [[-c]])

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc ]] .. rule.output .. [[)"]])

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
            export PATH="$PATH:$(loc :gcc bin)"
            gcc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

translation_unit {
    name = "heap",
    source = "+granite/rts/native/src/heap.cpp",
    includes = {
        "+granite/rts/native/src/heap.hpp",
        "+granite/rts/native/src/value.hpp",
    },
    output = "@heap.o",
}

translation_unit {
    name = "lambda",
    source = "+granite/rts/native/src/lambda.cpp",
    includes = {
        "+granite/rts/native/src/heap.hpp",
        "+granite/rts/native/src/lambda.hpp",
        "+granite/rts/native/src/value.hpp",
    },
    output = "@lambda.o",
}

translation_unit {
    name = "panic",
    source = "+granite/rts/native/src/panic.cpp",
    includes = {
        "+granite/rts/native/src/panic.hpp",
    },
    output = "@panic.o",
}

translation_unit {
    name = "value",
    source = "+granite/rts/native/src/value.cpp",
    includes = {
        "+granite/rts/native/src/value.hpp",
    },
    output = "@value.o",
}
