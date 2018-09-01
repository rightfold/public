go_package {
    name = "ur-lib-redirect",
    imports = {
        ":ur-lib-rules",
        ":ur-lib-shorts",
    },
    sources = { "+ur/src/redirect/redirect.go" },
    output = "@ur/redirect.a",
}

go_package {
    name = "ur-lib-rules",
    imports = { },
    sources = {
        "+ur/src/rules/apply.go",
        "+ur/src/rules/parse.go",
        "+ur/src/rules/rule.go",
    },
    output = "@ur/rules.a",
}

go_package {
    name = "ur-lib-shorts",
    imports = { },
    sources = { "+ur/src/shorts/database.go" },
    output = "@ur/shorts.a",
}

go_package {
    name = "ur-lib-urd",
    imports = {
        ":ur-lib-redirect",
        ":ur-lib-rules",
        ":ur-lib-shorts",
    },
    sources = { "+ur/src/urd/main.go" },
    output = "@ur/urd.a",
}

go_executable {
    name = "ur-exe-urd",
    main = ":ur-lib-urd ur/urd.a",
    imports = {
        ":ur-lib-redirect",
        ":ur-lib-rules",
        ":ur-lib-shorts",
    },
    output = "@urd",
}

genrule {
    name = "ur-test-cucumber",
    inputs = {
        ":cucumber",
        ":ur-exe-urd",
        "+ur/features/rules.feature",
        "+ur/features/shorts.feature",
        "+ur/features/step_definitions/queries.rb",
        "+ur/features/support/env.rb",
        "+ur/features/support/hooks.rb",
        "+ur/testdata/examplerules.txt",
        "+ur/testdata/exampleshorts.json",
        "+ur/testdata/norules.txt",
        "+ur/testdata/noshorts.json",
    },
    outputs = { },
    command = [[
        export PATH="$PATH:$(loc :cucumber bin):$(loc :ur-exe-urd)"
        cd "$(dirname "$(loc +ur/features/rules.feature)")/.."
        cucumber
    ]],
}
