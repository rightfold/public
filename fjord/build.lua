local RUSTC_FLAGS = { "-Anon-camel-case-types", "-Aunused-parens" }

rust_rlib {
    name = "fjord-lib-token",
    crate = "fjordtoken",
    flags = RUSTC_FLAGS,
    root_source = "+fjord/src/token.rs",
    extra_sources = { },
    externs = { },
    output = "@libfjordtoken.rlib",
}

rust_rlib {
    name = "fjord-lib-lex",
    crate = "fjordlex",
    flags = RUSTC_FLAGS,
    root_source = "+fjord/src/lex/lib.rs",
    extra_sources = { "+fjord/src/lex/source.rs" },
    externs = { fjordtoken = ":fjord-lib-token libfjordtoken.rlib" },
    output = "@libfjordlex.rlib",
}

rust_rlib {
    name = "fjord-lib-ast",
    crate = "fjordast",
    flags = RUSTC_FLAGS,
    root_source = "+fjord/src/ast.rs",
    extra_sources = { },
    externs = { },
    output = "@libfjordast.rlib",
}

rust_rlib {
    name = "fjord-lib-parse",
    crate = "fjordparse",
    flags = RUSTC_FLAGS,
    root_source = "+fjord/src/parse/lib.rs",
    extra_sources = {
        "+fjord/src/parse/defs.rs",
        "+fjord/src/parse/typeexprs.rs",
        "+fjord/src/parse/util.rs",
        "+fjord/src/parse/valexprs.rs",
    },
    externs = {
        fjordast = ":fjord-lib-ast libfjordast.rlib",
        fjordlex = ":fjord-lib-lex libfjordlex.rlib",
        fjordtoken = ":fjord-lib-token libfjordtoken.rlib",
    },
    output = "@libfjordparse.rlib",
}

rust_executable {
    name = "fjord-exe-fjordc",
    crate = "fjordc",
    flags = RUSTC_FLAGS,
    root_source = "+fjord/src/fjordc.rs",
    extra_sources = { },
    externs = {
        fjordast = ":fjord-lib-ast libfjordast.rlib",
        fjordlex = ":fjord-lib-lex libfjordlex.rlib",
        fjordtoken = ":fjord-lib-token libfjordtoken.rlib",
        fjordparse = ":fjord-lib-parse libfjordparse.rlib",
    },
    output = "@fjordc",
}
