nixrule {
    name = "granite-ghc",
    nixexpr = [[
        nixpkgs.haskell.packages.ghc841.ghcWithPackages (p: [
            p.hashable
            p.hashtables
            p.parsec
            p.primitive
            p.unordered-containers
        ])
    ]],
}

local haskell_objects = { }

local function haskell_module(rule)
    local inputs = { ":granite-ghc" }
    local outputs = { }
    local args = { }

    table.insert(args, [[-XApplicativeDo]])
    table.insert(args, [[-XDataKinds]])
    table.insert(args, [[-XDeriveAnyClass]])
    table.insert(args, [[-XDeriveGeneric]])
    table.insert(args, [[-XDerivingStrategies]])
    table.insert(args, [[-XFlexibleContexts]])
    table.insert(args, [[-XGADTs]])
    table.insert(args, [[-XGeneralizedNewtypeDeriving]])
    table.insert(args, [[-XKindSignatures]])
    table.insert(args, [[-XOverloadedStrings]])
    table.insert(args, [[-XPatternSynonyms]])
    table.insert(args, [[-XStandaloneDeriving]])
    table.insert(args, [[-XStrictData]])
    table.insert(args, [[-XTypeOperators]])

    table.insert(args, [[-Wall]])
    table.insert(args, [[-Wincomplete-record-updates]])
    table.insert(args, [[-Wincomplete-uni-patterns]])

    table.insert(args, [[-fdiagnostics-color=always]])

    table.insert(args, [[-c]])

    local object = "@" .. rule.name .. ".o"
    table.insert(outputs, object)
    table.insert(args, [[-o]])
    table.insert(args, [["$(loc ]] .. object .. [[)"]])

    -- The Garbage Haskell Compiler generates and looks for interface files by
    -- module name, but with dots replaced by directory separators. This relies
    -- on a bug in Snowflake where it does not detect that output files have the
    -- correct names.
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

    table.insert(
        haskell_objects,
        ":granite-lib-" .. string.gsub(rule.name, "%.", "-") .. " " ..
            rule.name .. ".o"
    )
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
    imports = {
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
    },
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
    name = "Granite.Behavioral.Parse",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Common.Lex",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Type",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.TypeCheck",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Constraint",
        "Granite.Behavioral.Infer",
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Interface",
    },
}

haskell_module {
    name = "Granite.Common.Lex",
    imports = {
        "Granite.Common.Position",
        "Granite.Common.Name",
    },
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

haskell_module {
    name = "Granite.Organizational.Interface",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Abstract",
    },
}

haskell_module {
    name = "Granite.Organizational.Parse",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Parse",
        "Granite.Common.Lex",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Abstract",
    },
}

haskell_module {
    name = "Granite.Organizational.TypeCheck",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.TypeCheck",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Abstract",
        "Granite.Organizational.Interface",
    },
}

haskell_module {
    name = "Main",
    imports = {
        "Granite.Behavioral.TypeCheck",
        "Granite.Organizational.Abstract",
        "Granite.Organizational.Interface",
        "Granite.Organizational.Parse",
        "Granite.Organizational.TypeCheck",
    },
}

do
    local inputs = { ":granite-ghc" }
    local args = { }

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc @granitec)"]])

    table.insert(args, [[-package]]); table.insert(args, [[hashable]])
    table.insert(args, [[-package]]); table.insert(args, [[hashtables]])
    table.insert(args, [[-package]]); table.insert(args, [[parsec]])
    table.insert(args, [[-package]]); table.insert(args, [[primitive]])
    table.insert(args, [[-package]]); table.insert(args, [[unordered-containers]])

    for _, object in ipairs(haskell_objects) do
        table.insert(inputs, object)
        table.insert(args, [["$(loc ]] .. object .. [[)"]])
    end

    genrule {
        name = "granite-exe-granitec",
        inputs = inputs,
        outputs = { "@granitec" },
        command = [[
            export PATH="$PATH:$(loc :granite-ghc bin)"
            ghc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end
