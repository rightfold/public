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
require "support/make"
require "support/verilator"

require "cpu/build"
require "klok/build"
require "ur/build"
