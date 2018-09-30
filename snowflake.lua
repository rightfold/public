nixpkgs [[
    let
        tarball = fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz";
            sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";
        };
    in
        import tarball {}
]]

require "support/avr-gcc"
require "support/binutils"
require "support/docbook"
require "support/gcc"
require "support/make"
require "support/perl"
require "support/tar"
require "support/verilator"
require "support/xsltproc"

require "cpu/build"
require "granite/build"
require "klok/build"

do
    local inputs = {
        ":perl",
    }

    local tests = { }
    for i, input in ipairs(inputs) do
        if i > 1 then
            table.insert(tests, [["$(loc ]] .. input .. [[ tap)"]])
        end
    end

    local command = [[
        export PATH="$PATH:$(loc :perl bin)"
        prove -e cat ]] .. table.concat(tests, " ") .. [[
    ]]

    genrule {
        name = "test",
        inputs = inputs,
        outputs = { },
        command = command,
    }
end
