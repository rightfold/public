parameter cpu_code_sz = 256;
parameter cpu_pc_sz   =   8;
parameter cpu_reg_sz  =   8;
parameter cpu_out_sz  =   8;

parameter cpu_cmp_state_sz = 2;
parameter cpu_cmp_state_lt = 0;
parameter cpu_cmp_state_eq = 1;
parameter cpu_cmp_state_gt = 2;

module cpu_cpu(clk, reset, code, out);
    input clk, reset;
    input  [cpu_code_sz - 1 : 0] code;
    output [cpu_out_sz  - 1 : 0] out;

    reg [cpu_pc_sz        - 1 : 0] pc;
    reg [cpu_reg_sz       - 1 : 0] regs [ 0 : 7 ];
    reg [cpu_cmp_state_sz - 1 : 0] cmp_state;

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

        cpu_inst_opcode_jlt :
            if (cmp_state == cpu_cmp_state_lt) pc <= immop1;
            else pc <= pc + cpu_inst_sz_jlt;

        cpu_inst_opcode_ilt :
            if (cmp_state == cpu_cmp_state_lt) pc <= regs[regop1];
            else pc <= pc + cpu_inst_sz_ilt;

        cpu_inst_opcode_jeq :
            if (cmp_state == cpu_cmp_state_eq) pc <= immop1;
            else pc <= pc + cpu_inst_sz_jeq;

        cpu_inst_opcode_ieq :
            if (cmp_state == cpu_cmp_state_eq) pc <= regs[regop1];
            else pc <= pc + cpu_inst_sz_ieq;

        cpu_inst_opcode_jgt :
            if (cmp_state == cpu_cmp_state_gt) pc <= immop1;
            else pc <= pc + cpu_inst_sz_jgt;

        cpu_inst_opcode_igt :
            if (cmp_state == cpu_cmp_state_gt) pc <= regs[regop1];
            else pc <= pc + cpu_inst_sz_igt;

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
            cmp_state    = regs[regop1] <  regs[regop2] ? 0 :
                           regs[regop1] == regs[regop2] ? 1 :
                                                          2 ;
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
