nixrule { name = "rustc", nixexpr = "nixpkgs.rustc" }

local function rust_crate(rule)
    local inputs = { ":rustc" }
    local args = { }

    table.insert(args, "--crate-name")
    table.insert(args, rule.crate)

    table.insert(args, "--crate-type")
    table.insert(args, rule.crate_type)

    for _, flag in ipairs(rule.flags) do
        table.insert(args, flag)
    end

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc ]] .. rule.output .. [[)"]])

    for name, extern in pairs(rule.externs) do
        table.insert(inputs, extern)
        table.insert(args, [[--extern]])
        table.insert(args, name .. [[="$(loc ]] .. extern .. [[)"]])
    end

    table.insert(inputs, rule.root_source)
    table.insert(args, [["$(loc ]] .. rule.root_source .. [[)"]])

    for _, source in ipairs(rule.extra_sources) do
        table.insert(inputs, source)
    end

    genrule {
        name = rule.name,
        inputs = inputs,
        outputs = { rule.output },
        command = [[
            export PATH="$PATH:$(loc :rustc bin)"
            rustc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

function rust_rlib(rule)
    rust_crate {
        name = rule.name,
        crate = rule.crate,
        crate_type = "rlib",
        flags = rule.flags,
        root_source = rule.root_source,
        extra_sources = rule.extra_sources,
        externs = rule.externs,
        output = rule.output,
    }
end

function rust_executable(rule)
    rust_crate {
        name = rule.name,
        crate = rule.crate,
        crate_type = "bin",
        flags = rule.flags,
        root_source = rule.root_source,
        extra_sources = rule.extra_sources,
        externs = rule.externs,
        output = rule.output,
    }
end
