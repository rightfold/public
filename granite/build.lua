nixrule {
    name = "granite-ghc",
    nixexpr = [[
        nixpkgs.haskell.packages.ghc841.ghcWithPackages (p: [
        ])
    ]],
}

local function haskell_module(rule)
    local inputs = { ":granite-ghc" }
    local outputs = { }
    local args = { }

    table.insert(args, [[-XConstraintKinds]])
    table.insert(args, [[-XDataKinds]])
    table.insert(args, [[-XDerivingStrategies]])
    table.insert(args, [[-XGADTs]])
    table.insert(args, [[-XKindSignatures]])
    table.insert(args, [[-XMultiParamTypeClasses]])
    table.insert(args, [[-XStandaloneDeriving]])
    table.insert(args, [[-XTypeOperators]])

    table.insert(args, [[-Wall]])
    table.insert(args, [[-Wincomplete-record-updates]])
    table.insert(args, [[-Wincomplete-uni-patterns]])

    table.insert(args, [[-fdiagnostics-color=always]])

    table.insert(args, [[-c]])

    local object = "@" .. rule.name .. ".o"
    table.insert(outputs, object)
    table.insert(args, [[-odir]])
    table.insert(args, [["$(dirname "$(loc ]] .. object .. [[)")"]])

    local interface = "@" .. rule.name .. ".hi"
    table.insert(outputs, interface)
    table.insert(args, [[-hidir]])
    table.insert(args, [["$(dirname "$(loc ]] .. interface .. [[)")"]])

    for _, import in ipairs(rule.imports) do
        local label = ":granite-lib-" .. string.gsub(import, "%.", "-")
        table.insert(inputs, label)
        table.insert(args, [[-i"$(loc ]] .. label .. [[)"]])
    end

    local source = "+granite/src/" .. string.gsub(rule.name, "%.", "/") .. ".hs"
    table.insert(inputs, source)
    table.insert(args, [["$(loc ]] .. source .. [[)"]])

    genrule {
        name = "granite-lib-" .. string.gsub(rule.name, "%.", "-"),
        inputs = inputs,
        outputs = outputs,
        command = [[
            export PATH="$PATH:$(loc :granite-ghc bin)"
            ghc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

haskell_module {
    name = "Granite.Elaborate",
    imports = { "Granite.Name", "Granite.Interface", "Granite.Source" },
}

haskell_module {
    name = "Granite.Interface",
    imports = { "Granite.Name", "Granite.Source" },
}

haskell_module {
    name = "Granite.Name",
    imports = { },
}

haskell_module {
    name = "Granite.Source",
    imports = { "Granite.Name" },
}
