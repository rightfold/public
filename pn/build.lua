genrule {
    name = "pn-test-ticket",
    inputs = {
        ":perl",
        "+pn/src/PN/Ticket.pm",
        "+pn/src/PN/Ticket.t",
        "+pn/testdata/empty.pnt",
        "+pn/testdata/troll.pnt",
    },
    outputs = { "@tap" },
    command = [[
        export PATH="$PATH:$(loc :perl bin)"
        export PERL5LIB="$(dirname "$(loc +pn/src/PN/Ticket.pm)")/.."
        export TESTDATA="$(dirname "$(loc +pn/testdata/empty.pnt)")"
        perl "$(loc +pn/src/PN/Ticket.t)" > "$(loc @tap)"
    ]],
}
