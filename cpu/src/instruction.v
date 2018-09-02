parameter cpu_inst_opcode_sz = 4;
parameter cpu_inst_regop_sz  = 3;
parameter cpu_inst_immop_sz  = 8;

parameter cpu_inst_opcode_imm =  0;

parameter cpu_inst_opcode_out =  1;

parameter cpu_inst_opcode_jmp =  2;
parameter cpu_inst_opcode_imp =  3;
parameter cpu_inst_opcode_jz  =  4;
parameter cpu_inst_opcode_iz  =  5;

parameter cpu_inst_opcode_inc =  6;
parameter cpu_inst_opcode_dec =  7;

parameter cpu_inst_opcode_add =  8;
parameter cpu_inst_opcode_sub =  9;
parameter cpu_inst_opcode_mul = 10;
parameter cpu_inst_opcode_div = 11;
parameter cpu_inst_opcode_and = 12;
parameter cpu_inst_opcode_or  = 13;
parameter cpu_inst_opcode_xor = 14;

parameter cpu_inst_sz_imm = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_immop_sz;

parameter cpu_inst_sz_out = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_jmp = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_imp = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_jz  = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_iz  = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_inc = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_dec = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_add = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_sub = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_mul = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_div = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_and = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_or  = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_xor = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
