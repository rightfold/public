nixrule {
    name = "krokodil-ghc",
    nixexpr = [[
        nixpkgs.haskell.packages.ghc841.ghcWithPackages (p: [
        ])
    ]],
}

local function haskell_module(rule)
    local inputs = { ":krokodil-ghc" }
    local outputs = { }
    local args = { }

    table.insert(args, [[-XDerivingStrategies]])
    table.insert(args, [[-XGeneralizedNewtypeDeriving]])
    table.insert(args, [[-XOverloadedStrings]])

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
        local label = ":krokodil-lib-" .. string.gsub(import, "%.", "-")
        table.insert(inputs, label)
        table.insert(args, [[-i"$(loc ]] .. label .. [[)"]])
    end

    local source = "+krokodil/src/" .. string.gsub(rule.name, "%.", "/") .. ".hs"
    table.insert(inputs, source)
    table.insert(args, [["$(loc ]] .. source .. [[)"]])

    genrule {
        name = "krokodil-lib-" .. string.gsub(rule.name, "%.", "-"),
        inputs = inputs,
        outputs = outputs,
        command = [[
            export PATH="$PATH:$(loc :krokodil-ghc bin)"
            ghc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end

haskell_module {
    name = "Data.Validation",
    imports = { },
}

haskell_module {
    name = "Data.Html.Escape",
    imports = { },
}

haskell_module {
    name = "Krokodil.Article",
    imports = { },
}

haskell_module {
    name = "Krokodil.Article.DisplayArticle",
    imports = {
         "Data.Html.Escape",
         "Krokodil.Article",
    },
}

haskell_module {
    name = "Krokodil.Article.PublishArticle.Validation",
    imports = {
         "Data.Validation",
         "Krokodil.Article",
    },
}
