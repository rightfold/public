nixrule {
    name = "granite-compiler-ghc",
    nixexpr = [[
        let
            llvm-hs-src = fetchTarball {
                url = "https://github.com/llvm-hs/llvm-hs/archive/llvm-hs-6.3.0.tar.gz";
                sha256 = "0g9154n15fmscpmry9m4zr1labjyq40kqrlg0347g9wqfrwjnpmz";
            };
            llvm-hs = p: p.mkDerivation {
                pname = "llvm-hs";
                version = "6.3.0";
                src = llvm-hs-src + "/llvm-hs";
                libraryHaskellDepends = [
                    (llvm-hs-pure p)
                    p.QuickCheck
                    p.attoparsec
                    p.exceptions
                    p.pretty-show
                    p.tasty
                    p.tasty-hunit
                    p.tasty-quickcheck
                    p.temporary
                    p.utf8-string
                ];
                libraryToolDepends = [
                    nixpkgs.llvm_6
                ];
                license = nixpkgs.stdenv.lib.licenses.bsd3;
            };
            llvm-hs-pure = p: p.mkDerivation {
                pname = "llvm-hs-pure";
                version = "6.2.1";
                src = llvm-hs-src + "/llvm-hs-pure";
                libraryHaskellDepends = [
                    p.attoparsec
                    p.fail
                    p.tasty
                    p.tasty-hunit
                    p.tasty-quickcheck
                    p.unordered-containers
                ];
                license = nixpkgs.stdenv.lib.licenses.bsd3;
            };
        in
            nixpkgs.haskell.packages.ghc841.ghcWithPackages (p: [
                (llvm-hs p)
                # (llvm-hs-pure p)
                p.hashable
                p.lens
                p.optparse-applicative
                p.parsec
                p.unordered-containers
            ])
    ]],
}

local haskell_objects = { }

local function haskell_module(rule)
    local inputs = { ":granite-compiler-ghc" }
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
    table.insert(args, [[-XLambdaCase]])
    table.insert(args, [[-XOverloadedStrings]])
    table.insert(args, [[-XPatternSynonyms]])
    table.insert(args, [[-XRankNTypes]])
    table.insert(args, [[-XScopedTypeVariables]])
    table.insert(args, [[-XStandaloneDeriving]])
    table.insert(args, [[-XStrictData]])
    table.insert(args, [[-XTupleSections]])
    table.insert(args, [[-XTypeApplications]])
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
        local label = ":granite-compiler-lib-" .. string.gsub(import, "%.", "-")
        table.insert(inputs, label)
        table.insert(args, [[-i"$(loc ]] .. label .. [[)"]])
    end

    local source = "+granite/compiler/src/" .. string.gsub(rule.name, "%.", "/") .. ".hs"
    table.insert(inputs, source)
    table.insert(args, [["$(loc ]] .. source .. [[)"]])

    genrule {
        name = "granite-compiler-lib-" .. string.gsub(rule.name, "%.", "-"),
        inputs = inputs,
        outputs = outputs,
        command = [[
            export PATH="$PATH:$(loc :granite-compiler-ghc bin)"
            ghc ]] .. table.concat(args, " ") .. [[
        ]],
    }

    table.insert(
        haskell_objects,
        ":granite-compiler-lib-" .. string.gsub(rule.name, "%.", "-") .. " " ..
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
        "Granite.Behavioral.Unify",
        "Granite.Common.Name",
    },
}

haskell_module {
    name = "Granite.Behavioral.Infer",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Constraint",
        "Granite.Behavioral.Type",
        "Granite.Behavioral.Unify",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Llvm",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Llvm.Foreign",
        "Granite.Behavioral.Llvm.Infrastructure",
        "Granite.Behavioral.Llvm.Lambda",
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Llvm.Foreign",
    imports = {
        "Granite.Behavioral.Llvm.Infrastructure",
        "Granite.Behavioral.Llvm.Lambda",
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Llvm.Infrastructure",
    imports = {
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
        "Granite.Common.Position",
    },
}

haskell_module {
    name = "Granite.Behavioral.Llvm.Lambda",
    imports = {
        "Granite.Behavioral.Llvm.Infrastructure",
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
        "Granite.Behavioral.Unify",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Interface",
    },
}

haskell_module {
    name = "Granite.Behavioral.Unify",
    imports = {
        "Granite.Behavioral.Type",
        "Granite.Common.Name",
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
    name = "Granite.Organizational.Llvm",
    imports = {
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Llvm",
        "Granite.Behavioral.Llvm.Infrastructure",
        "Granite.Behavioral.Llvm.Lambda",
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
        "Granite.Behavioral.Infer",
        "Granite.Behavioral.Type",
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
        "Granite.Behavioral.Abstract",
        "Granite.Behavioral.Llvm",
        "Granite.Behavioral.Llvm.Infrastructure",
        "Granite.Behavioral.Llvm.Lambda",
        "Granite.Behavioral.Type",
        "Granite.Behavioral.TypeCheck",
        "Granite.Behavioral.Unify",
        "Granite.Common.Name",
        "Granite.Common.Position",
        "Granite.Organizational.Abstract",
        "Granite.Organizational.Interface",
        "Granite.Organizational.Llvm",
        "Granite.Organizational.Parse",
        "Granite.Organizational.TypeCheck",
    },
}

do
    local inputs = { ":granite-compiler-ghc" }
    local args = { }

    table.insert(args, [[-o]])
    table.insert(args, [["$(loc @granitec)"]])

    table.insert(args, [[-package]]); table.insert(args, [[hashable]])
    table.insert(args, [[-package]]); table.insert(args, [[lens]])
    table.insert(args, [[-package]]); table.insert(args, [[llvm-hs]])
    table.insert(args, [[-package]]); table.insert(args, [[llvm-hs-pure]])
    table.insert(args, [[-package]]); table.insert(args, [[optparse-applicative]])
    table.insert(args, [[-package]]); table.insert(args, [[parsec]])
    table.insert(args, [[-package]]); table.insert(args, [[unordered-containers]])

    for _, object in ipairs(haskell_objects) do
        table.insert(inputs, object)
        table.insert(args, [["$(loc ]] .. object .. [[)"]])
    end

    genrule {
        name = "granite-compiler-exe-granitec",
        inputs = inputs,
        outputs = { "@granitec" },
        command = [[
            export PATH="$PATH:$(loc :granite-compiler-ghc bin)"
            ghc ]] .. table.concat(args, " ") .. [[
        ]],
    }
end
