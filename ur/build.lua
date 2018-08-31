go_package {
    name = "ur-lib-redirect",
    imports = { ":ur-lib-rules" },
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
    name = "ur-lib-urd",
    imports = {
        ":ur-lib-redirect",
        ":ur-lib-rules",
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
    },
    output = "@urd",
}
