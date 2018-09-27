nixrule {
    name = "granite-ghc",
    nixexpr = [[
        nixpkgs.haskell.packages.ghc841.ghcWithPackages (p: [
            p.hashable
            p.hashtables
            p.primitive
            p.unordered-containers
        ])
    ]],
}

local function haskell_module(rule)
    local inputs = { ":granite-ghc" }
    local outputs = { }
    local args = { }

    table.insert(args, [[-XDataKinds]])
    table.insert(args, [[-XDeriveAnyClass]])
    table.insert(args, [[-XDeriveGeneric]])
    table.insert(args, [[-XDerivingStrategies]])
    table.insert(args, [[-XFlexibleContexts]])
    table.insert(args, [[-XGADTs]])
    table.insert(args, [[-XGeneralizedNewtypeDeriving]])
    table.insert(args, [[-XKindSignatures]])
    table.insert(args, [[-XOverloadedStrings]])
    table.insert(args, [[-XStandaloneDeriving]])
    table.insert(args, [[-XStrictData]])

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
    name = "Granite.Behavioral.Abstract",
    imports = {
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Constraint",
    imports = { "Granite.Behavioral.Type" },
}

haskell_module {
    name = "Granite.Behavioral.Infer",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Constraint",
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Type",
    imports = { "Granite.Common.Name" },
}

haskell_module {
    name = "Granite.Common.Name",
    imports = { },
}

haskell_module {
    name = "Granite.Common.Position",
    imports = { },
}

haskell_module {
    name = "Granite.Organizational.Abstract",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

