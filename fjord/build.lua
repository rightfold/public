local RUSTC_FLAGS = { "-Anon-camel-case-types", "-Aunused-parens" }

rust_rlib {
    name = "fjord-lib-token",
    crate = "fjordtoken",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/token.rs",
    inputs = { },
    externs = { },
    output = "@libfjordtoken.rlib",
}

rust_rlib {
    name = "fjord-lib-lex",
    crate = "fjordlex",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/lex/lib.rs",
    inputs = { "+fjord/src/lex/source.rs" },
    externs = { fjordtoken = ":fjord-lib-token libfjordtoken.rlib" },
    output = "@libfjordlex.rlib",
}

rust_rlib {
    name = "fjord-lib-ast",
    crate = "fjordast",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/ast.rs",
    inputs = { },
    externs = { },
    output = "@libfjordast.rlib",
}

rust_rlib {
    name = "fjord-lib-parse",
    crate = "fjordparse",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/parse/lib.rs",
    inputs = {
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

rust_rlib {
    name = "fjord-lib-lc",
    crate = "fjordlc",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/lc.rs",
    inputs = { },
    externs = { },
    output = "@libfjordlc.rlib",
}

rust_rlib {
    name = "fjord-lib-llvm",
    crate = "fjordllvm",
    flags = RUSTC_FLAGS,
    source = "+fjord/src/llvm.rs",
    inputs = { },
    externs = { },
    output = "@libfjordllvm.rlib",
}

do
    local rustc_flags = { table.unpack(RUSTC_FLAGS) }
    table.insert(rustc_flags, [[$("$(loc :llvm bin/llvm-config)" --ldflags)]])
    rust_executable {
        name = "fjord-exe-fjordc",
        crate = "fjordc",
        flags = rustc_flags,
        source = "+fjord/src/fjordc.rs",
        inputs = { ":llvm" },
        externs = {
            fjordast = ":fjord-lib-ast libfjordast.rlib",
            fjordlc = ":fjord-lib-lc libfjordlc.rlib",
            fjordlex = ":fjord-lib-lex libfjordlex.rlib",
            fjordllvm = ":fjord-lib-llvm libfjordllvm.rlib",
            fjordparse = ":fjord-lib-parse libfjordparse.rlib",
            fjordtoken = ":fjord-lib-token libfjordtoken.rlib",
        },
        output = "@fjordc",
    }
end
