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
require "support/cucumber"
require "support/gcc"
require "support/go"
require "support/llvm"
require "support/make"
require "support/perl"
require "support/rust"
require "support/verilator"

require "cpu/build"
require "fjord/build"
require "granite/build"
require "klok/build"
require "krokodil/build"
require "pn/build"
require "ur/build"

do
    local inputs = {
        ":perl",
        ":pn-test-ticket",
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
