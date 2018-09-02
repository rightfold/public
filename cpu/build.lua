genrule {
    name = "cpu-verilate",
    inputs = {
        ":binutils",
        ":gcc",
        ":make",
        ":verilator",
        "+cpu/src/cpu.v",
        "+cpu/src/decode.v",
        "+cpu/src/instruction.v",
        "+cpu/src/reset.v",
        "+cpu/test/test.cpp",
    },
    outputs = { "@cpu-test" },
    command = [[
        export PATH="$PATH:$(loc :binutils  bin)"
        export PATH="$PATH:$(loc :gcc       bin)"
        export PATH="$PATH:$(loc :make      bin)"
        export PATH="$PATH:$(loc :verilator bin)"

        verilator                                                           \
            --sv                                                            \
            --cc                                                            \
            --top-module cpu_cpu                                            \
            "$(loc +cpu/src/cpu.v)"                                         \
            "$(loc +cpu/src/decode.v)"                                      \
            "$(loc +cpu/src/instruction.v)"                                 \
            "$(loc +cpu/src/reset.v)"

        (
            cd obj_dir
            make -f Vcpu_cpu.mk
        )

        g++                                                                 \
            -std=c++14 -Wall -Wextra -Wpedantic                             \
            -c                                                              \
            -Iobj_dir                                                       \
            -I"$(loc :verilator share/verilator/include)"                   \
            -o test.o                                                       \
            "$(loc +cpu/test/test.cpp)"

        g++                                                                 \
            -o "$(loc @cpu-test)"                                           \
            test.o                                                          \
            obj_dir/Vcpu_cpu__ALL.a                                         \
            "$(loc :verilator share/verilator/include/verilated.cpp)"
    ]],
}
