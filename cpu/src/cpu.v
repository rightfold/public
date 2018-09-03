parameter cpu_code_sz = 256;
parameter cpu_pc_sz   =   8;
parameter cpu_reg_sz  =   8;
parameter cpu_out_sz  =   8;

module cpu_cpu(clk, reset, code, out);
    input clk, reset;
    input  [cpu_code_sz - 1 : 0] code;
    output [cpu_out_sz  - 1 : 0] out;

    reg [cpu_pc_sz  - 1 : 0] pc;
    reg [cpu_reg_sz - 1 : 0] regs [ 0 : 7 ];
    reg zero_flag;

    wire [cpu_inst_opcode_sz - 1 : 0] opcode;
    wire [cpu_inst_regop_sz  - 1 : 0] regop1, regop2;
    wire [cpu_inst_immop_sz  - 1 : 0] immop1;

    cpu_resetter resetter(clk, reset, pc);

    cpu_decoder decoder(code, pc, opcode, regop1, regop2, immop1);

    always @(posedge clk) if (!reset)
    case (opcode)
        cpu_inst_opcode_imm : begin
            regs[regop1] <= immop1;
            pc <= pc + cpu_inst_sz_imm;
        end

        cpu_inst_opcode_out : begin
            out <= regs[regop1];
            pc <= pc + cpu_inst_sz_out;
        end

        cpu_inst_opcode_jmp :
            pc <= immop1;

        cpu_inst_opcode_imp :
            pc <= regs[regop1];

        cpu_inst_opcode_jz  :
            if (zero_flag)
                pc <= immop1;
            else
                pc <= pc + cpu_inst_sz_jz;

        cpu_inst_opcode_iz  :
            if (zero_flag)
                pc <= regs[regop1];
            else
                pc <= pc + cpu_inst_sz_jz;

        cpu_inst_opcode_inc : begin
            regs[regop1] = regs[regop1] + 1;
            pc <= pc + cpu_inst_sz_inc;
        end

        cpu_inst_opcode_dec : begin
            regs[regop1] = regs[regop1] - 1;
            pc <= pc + cpu_inst_sz_dec;
        end

        cpu_inst_opcode_add : begin
            regs[regop1] = regs[regop1] + regs[regop2];
            pc <= pc + cpu_inst_sz_add;
        end

        cpu_inst_opcode_sub : begin
            regs[regop1] = regs[regop1] - regs[regop2];
            zero_flag    = regs[regop1] - regs[regop2] == 0;
            pc <= pc + cpu_inst_sz_sub;
        end

        cpu_inst_opcode_mul : begin
            regs[regop1] = regs[regop1] * regs[regop2];
            pc <= pc + cpu_inst_sz_mul;
        end

        cpu_inst_opcode_div : begin
            regs[regop1] = regs[regop1] / regs[regop2];
            pc <= pc + cpu_inst_sz_div;
        end

        cpu_inst_opcode_and : begin
            regs[regop1] = regs[regop1] & regs[regop2];
            pc <= pc + cpu_inst_sz_and;
        end

        cpu_inst_opcode_or  : begin
            regs[regop1] = regs[regop1] | regs[regop2];
            pc <= pc + cpu_inst_sz_or ;
        end

        cpu_inst_opcode_xor : begin
            regs[regop1] = regs[regop1] ^ regs[regop2];
            pc <= pc + cpu_inst_sz_xor;
        end
    endcase
endmodule
