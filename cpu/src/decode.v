module cpu_decoder(code, pc, opcode, regop1, regop2, immop1);
    parameter opsz = cpu_inst_opcode_sz;
    parameter rgsz = cpu_inst_regop_sz;
    parameter imsz = cpu_inst_immop_sz;

    input [cpu_code_sz - 1 : 0] code;
    input [cpu_pc_sz   - 1 : 0] pc;

    output wire [cpu_inst_opcode_sz - 1 : 0] opcode;
    output wire [cpu_inst_regop_sz  - 1 : 0] regop1, regop2;
    output wire [cpu_inst_immop_sz  - 1 : 0] immop1;

    assign opcode = code[pc +: opsz];

    assign regop1 =
        opcode == cpu_inst_opcode_imm ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_out ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_imp ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_ilt ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_ieq ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_igt ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_inc ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_dec ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_add ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_sub ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_mul ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_div ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_and ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_or  ? code[pc + opsz +: rgsz] :
        opcode == cpu_inst_opcode_xor ? code[pc + opsz +: rgsz] :
        'x;

    assign regop2 =
        opcode == cpu_inst_opcode_add ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_sub ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_mul ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_div ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_and ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_or  ? code[pc + opsz + rgsz +: rgsz] :
        opcode == cpu_inst_opcode_xor ? code[pc + opsz + rgsz +: rgsz] :
        'x;

    assign immop1 =
        opcode == cpu_inst_opcode_imm ? code[pc + opsz + rgsz +: imsz] :
        opcode == cpu_inst_opcode_jmp ? code[pc + opsz        +: imsz] :
        opcode == cpu_inst_opcode_jlt ? code[pc + opsz        +: imsz] :
        opcode == cpu_inst_opcode_jeq ? code[pc + opsz        +: imsz] :
        opcode == cpu_inst_opcode_jgt ? code[pc + opsz        +: imsz] :
        'x;
endmodule
