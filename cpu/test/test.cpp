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
        0,0,0,0  ,  0,0,0  ,  0,0,0,0,0,0,0,0  ,  //  0  IMM r0, 0
        0,0,0,0  ,  1,0,0  ,  1,0,1,0,0,0,0,0  ,  // 15  IMM r1, 5

        1,0,0,0  ,  1,0,0                      ,  // 30  OUT r1
        1,0,0,1  ,  1,0,0  ,  0,0,0            ,  // 37  SUB r1, r0
        0,0,1,0  ,  0,1,1,1,0,0,1,0            ,  // 47  JZ  78
        1,1,1,0  ,  1,0,0                      ,  // 59  DEC r1
        0,1,0,0  ,  0,1,1,1,1,0,0,0            ,  // 66  JMP 30

        1,0,0,0  ,  1,0,0                      ,  // 78  OUT r1

        1,1,0,0  ,  1,0,0                      ,  // 85  IMP r1
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
