nixrule { name = "go", nixexpr = "nixpkgs.go" }

function go_package(rule)
    local inputs = { ":go" }
    local args = { [[-pack]], [[-o]], [["$(loc ]] .. rule.output .. [[)"]] }

    for _, import in ipairs(rule.imports) do
        table.insert(inputs, import)
        table.insert(args, [[-I]])
        table.insert(args, [["$(loc ]] .. import ..[[)"]])
    end

    for _, source in ipairs(rule.sources) do
        table.insert(inputs, source)
        table.insert(args, [["$(loc ]] .. source .. [[)"]])
    end

    genrule {
        name = rule.name,
        inputs = inputs,
        outputs = { rule.output },
        command = [[
            export PATH="$PATH:$(loc :go bin)"
            go tool compile ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

function go_executable(rule)
    local inputs = { ":go", rule.main }
    local args = { [[-o]], [["$(loc ]] .. rule.output .. [[)"]] }

    for _, import in ipairs(rule.imports) do
        table.insert(inputs, import)
        table.insert(args, [[-L]])
        table.insert(args, [["$(loc ]] .. import .. [[)"]])
    end

    table.insert(args, [["$(loc ]] .. rule.main .. [[)"]])

    genrule {
        name = rule.name,
        inputs = inputs,
        outputs = { rule.output },
        command = [[
            export PATH="$PATH:$(loc :go bin)"
            go tool link ]] .. table.concat(args, " ") .. [[
        ]],
    }
end
