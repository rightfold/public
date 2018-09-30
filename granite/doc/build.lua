genrule {
    name = "granite-doc",
    inputs = {
        ":docbook",
        ":tar",
        ":xsltproc",
        "+granite/doc/src/compiler/index.xml",
        "+granite/doc/src/index.xml",
        "+granite/doc/src/runtime-system/index.xml",
        "+granite/doc/src/runtime-system/native/globals.xml",
        "+granite/doc/src/runtime-system/native/heaps.xml",
        "+granite/doc/src/runtime-system/native/index.xml",
        "+granite/doc/src/runtime-system/native/values.xml",
    },
    outputs = { "@granite-doc.tar" },
    command = [[
        export PATH="$PATH:$(loc :tar bin):$(loc :xsltproc bin)"

        DOCBOOK_XSLT="$(loc :docbook xml/xsl/docbook/xhtml/chunk.xsl)"

        xsltproc                                                            \
            --xinclude                                                      \
            "$DOCBOOK_XSLT"                                                 \
            "$(loc +granite/doc/src/index.xml)"

        tar cf "$(loc @granite-doc.tar)" *.html
    ]],
}
