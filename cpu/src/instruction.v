parameter cpu_inst_opcode_sz = 5;
parameter cpu_inst_regop_sz  = 3;
parameter cpu_inst_immop_sz  = 8;

parameter cpu_inst_opcode_imm =  0;

parameter cpu_inst_opcode_out =  1;

parameter cpu_inst_opcode_jmp =  2;
parameter cpu_inst_opcode_imp =  3;
parameter cpu_inst_opcode_jlt =  4;
parameter cpu_inst_opcode_ilt =  5;
parameter cpu_inst_opcode_jeq =  6;
parameter cpu_inst_opcode_ieq =  7;
parameter cpu_inst_opcode_jgt =  8;
parameter cpu_inst_opcode_igt =  9;

parameter cpu_inst_opcode_inc = 10;
parameter cpu_inst_opcode_dec = 11;

parameter cpu_inst_opcode_add = 12;
parameter cpu_inst_opcode_sub = 13;
parameter cpu_inst_opcode_mul = 14;
parameter cpu_inst_opcode_div = 15;
parameter cpu_inst_opcode_and = 16;
parameter cpu_inst_opcode_or  = 17;
parameter cpu_inst_opcode_xor = 18;

parameter cpu_inst_sz_imm = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_immop_sz;

parameter cpu_inst_sz_out = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_jmp = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_imp = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_jlt = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_ilt = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_jeq = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_ieq = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_jgt = cpu_inst_opcode_sz + cpu_inst_immop_sz;
parameter cpu_inst_sz_igt = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_inc = cpu_inst_opcode_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_dec = cpu_inst_opcode_sz + cpu_inst_regop_sz;

parameter cpu_inst_sz_add = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_sub = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_mul = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_div = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_and = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_or  = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
parameter cpu_inst_sz_xor = cpu_inst_opcode_sz + cpu_inst_regop_sz + cpu_inst_regop_sz;
