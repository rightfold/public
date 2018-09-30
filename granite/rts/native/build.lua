local function translation_unit(rule)
    local inputs = { ":gcc" }
    local args = { }

    table.insert(args, [[-std=c11]])
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
    source = "+granite/rts/native/src/heap.c",
    includes = {
        "+granite/rts/native/src/heap.h",
        "+granite/rts/native/src/value.h",
    },
    output = "@heap.o",
}

translation_unit {
    name = "lambda",
    source = "+granite/rts/native/src/lambda.c",
    includes = {
        "+granite/rts/native/src/heap.h",
        "+granite/rts/native/src/lambda.h",
        "+granite/rts/native/src/value.h",
    },
    output = "@lambda.o",
}

translation_unit {
    name = "panic",
    source = "+granite/rts/native/src/panic.c",
    includes = {
        "+granite/rts/native/src/panic.h",
    },
    output = "@panic.o",
}

translation_unit {
    name = "value",
    source = "+granite/rts/native/src/value.c",
    includes = {
        "+granite/rts/native/src/value.h",
    },
    output = "@value.o",
}
