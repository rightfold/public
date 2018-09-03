#include <Vcpu_cpu.h>

#include <bitset>
#include <iostream>

void tick(Vcpu_cpu& cpu)
{
    cpu.clk = 0;
    cpu.eval();
    cpu.clk = 1;
    cpu.eval();
}

void reset(Vcpu_cpu& cpu)
{
    cpu.reset = 1;
    tick(cpu);
    cpu.reset = 0;
}

int main()
{
    Vcpu_cpu cpu;

    bool code[] = {
        0,0,0,0,0  ,  0,0,0  ,  0,0,0,0,0,0,0,0  ,  //  0  IMM r0, 0
        0,0,0,0,0  ,  1,0,0  ,  1,0,1,0,0,0,0,0  ,  // 16  IMM r1, 5

        1,0,0,0,0  ,  1,0,0                      ,  // 32  OUT r1
        1,0,1,1,0  ,  1,0,0  ,  0,0,0            ,  // 40  SUB r1, r0
        0,2,1,0,0  ,  1,0,1,0,1,0,1,0            ,  // 51  JEQ 85
        1,1,0,1,0  ,  1,0,0                      ,  // 64  DEC r1
        0,1,0,0,0  ,  0,0,0,0,0,1,0,0            ,  // 72  JMP 32

        1,0,0,0,0  ,  1,0,0                      ,  // 85  OUT r1

        1,1,0,0,0  ,  1,0,0                      ,  // 93  IMP r1
    };
    for (unsigned i = 0; i < sizeof(code); ++i)
    {
        if (code[i]) cpu.code[i / 32] |=  (1 << (i % 32));
        else         cpu.code[i / 32] &= ~(1 << (i % 32));
    }

    reset(cpu);
    for (int i = 0; i < 80; ++i)
    {
        std::cout << static_cast<int>(cpu.cpu_cpu__DOT__pc) << ' '
                  << static_cast<int>(cpu.out) << '\n';
        tick(cpu);
    }

    return 0;
}
