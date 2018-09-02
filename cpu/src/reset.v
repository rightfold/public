module cpu_resetter(clk, reset, pc);
    input clk, reset;
    output [cpu_pc_sz - 1 : 0] pc;

    always @(posedge clk) if (reset)
    pc <= 0;
endmodule
