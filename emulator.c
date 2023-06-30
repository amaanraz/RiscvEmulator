#include <stdio.h> // for stderr
#include <stdlib.h> // for exit()
#include "types.h"
#include "utils.h"
#include "riscv.h"

void execute_rtype(Instruction, Processor *);
void execute_itype_except_load(Instruction, Processor *);
void execute_branch(Instruction, Processor *);
void execute_jal(Instruction, Processor *);
void execute_load(Instruction, Processor *, Byte *);
void execute_store(Instruction, Processor *, Byte *);
void execute_ecall(Processor *, Byte *);
void execute_lui(Instruction, Processor *);

void execute_instruction(uint32_t instruction_bits, Processor *processor,Byte *memory) {    
    Instruction instruction = parse_instruction(instruction_bits);
    switch(instruction.opcode) {
        case 0x33:
            execute_rtype(instruction, processor);
            break;
        case 0x13:
            execute_itype_except_load(instruction, processor);
            break;
        case 0x73:
            execute_ecall(processor, memory);
            break;
        case 0x63:
            execute_branch(instruction, processor);
            break;
        case 0x6F:
            execute_jal(instruction, processor);
            break;
        case 0x23:
            execute_store(instruction, processor, memory);
            break;
        case 0x03:
            execute_load(instruction, processor, memory);
            break;
        case 0x37:
            execute_lui(instruction, processor);
            break;
        default: // undefined opcode
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

void execute_rtype(Instruction instruction, Processor *processor) {
    switch (instruction.rtype.funct3){
        case 0x0:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // Add
                    processor->R[instruction.rtype.rd] =
                        ((sWord)processor->R[instruction.rtype.rs1]) +
                        ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x1:
                    // Mul
                    processor->R[instruction.rtype.rd] =
                        ((sWord)processor->R[instruction.rtype.rs1]) *
                        ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x20:
                    // Sub
                    processor->R[instruction.rtype.rd] =
                        ((sWord)processor->R[instruction.rtype.rs1]) -
                        ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x01:
            switch(instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("sll", instruction);
                    // rd = rs1 << rs2
                    processor->R[instruction.rtype.rd] =
                        ((sWord)processor->R[instruction.rtype.rs1]) <<
                        ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x01:
                    // print_rtype("mulh", instruction);
                    // rd = (rs1 * rs2)[63:32]
                    processor->R[instruction.rtype.rd] =
                        (((sDouble)processor->R[instruction.rtype.rs1]) *
                        ((sDouble)processor->R[instruction.rtype.rs2]) >> 32);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x2:
            switch (instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("slt", instruction);
                    // rd = (rs1 < rs2)?1:0
                    if((sWord)processor->R[instruction.rtype.rs1] < (sWord)processor->R[instruction.rtype.rs2]){
                        // rd = 1
                        processor->R[instruction.rtype.rd] = 1;
                    } else {
                        processor->R[instruction.rtype.rd] = 0;
                    }
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x4:
            switch(instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("xor", instruction);
                    // rd = rs1 ˆ rs2
                    processor->R[instruction.rtype.rd] = 
                        (processor->R[instruction.rtype.rs1] ^
                        processor->R[instruction.rtype.rs2]);
                    break;
                case 0x01:
                    // print_rtype("div", instruction);
                    // rd = rs1 / rs2
                    processor->R[instruction.rtype.rd] = 
                        ((sWord)processor->R[instruction.rtype.rs1] /
                        (sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x5:
            switch(instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("srl", instruction);
                    // rd = rs1 >> rs2 (0 extend)
                    processor->R[instruction.rtype.rd] = 
                        (processor->R[instruction.rtype.rs1] >>
                        processor->R[instruction.rtype.rs2]);
                    break;
                case 0x20:
                    // print_rtype("sra", instruction);
                    // rd = rs1 >> rs2 (msb-extends)
                    processor->R[instruction.rtype.rd] = 
                        ((sWord)processor->R[instruction.rtype.rs1] >>
                        (sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x6:
            switch(instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("or", instruction);
                    // rd = rs1 | rs2
                    processor->R[instruction.rtype.rd] = 
                        (processor->R[instruction.rtype.rs1]|
                        processor->R[instruction.rtype.rs2]);
                    break;
                case 0x01:
                    // print_rtype("rem", instruction);
                    // rd = rs1 % rs2
                    processor->R[instruction.rtype.rd] =
                        ((sWord)processor->R[instruction.rtype.rs1] %
                        (sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x7:
            switch(instruction.rtype.funct7){
                case 0x0:
                    // print_rtype("and", instruction);
                    // rd = rs1 & rs2
                    processor->R[instruction.rtype.rd] = 
                        (processor->R[instruction.rtype.rs1] &
                        processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
	/* deal with other cases */
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    // update PC
    processor->PC += 4;
}

void execute_itype_except_load(Instruction instruction, Processor *processor) {
	int shift_imm;
    switch (instruction.itype.funct3) {
        /* YOUR CODE HERE */
        case 0x0:
            // print_itype_except_load("addi", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
                ((sWord)processor->R[instruction.itype.rs1]) +
                (sign_extend_number(instruction.itype.imm, 12));
            break;
        case 0x1:
            // print_itype_except_load("slli", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
                ((sWord)processor->R[instruction.itype.rs1]) <<
                (sign_extend_number(instruction.itype.imm, 12));
            break;
        case 0x2:
            // print_itype_except_load("slti", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
                (((sWord)processor->R[instruction.itype.rs1]) <
                (sign_extend_number(instruction.itype.imm, 12)))?1:0;
            break;
        case 0x3:
            // print_itype_except_load("sltiu", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
                (((sWord)processor->R[instruction.itype.rs1]) <
                (sign_extend_number(instruction.itype.imm, 12)))?1:0;
            break;
        case 0x4:
            // print_itype_except_load("xori", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
                ((sWord)processor->R[instruction.itype.rs1]) ^
                (sign_extend_number(instruction.itype.imm, 12));
            break;
        case 0x5:
            shift_imm = instruction.itype.imm >> 5;
            switch(shift_imm) {
                case 0x00:
                shift_imm = 31;
                shift_imm = shift_imm & instruction.itype.imm;
                // print_itype_except_load("srli", instruction, instruction.itype.imm);
                processor->R[instruction.itype.rd] = 
                    ((sWord)processor->R[instruction.itype.rs1]) >>
                    (sign_extend_number(instruction.itype.imm, 12));
                    break;
                case 0x20:
                    // print_itype_except_load("srai", instruction, shift_imm);
                shift_imm = 31;
                shift_imm = shift_imm & instruction.itype.imm;
                processor->R[instruction.itype.rd] = 
                    ((sWord)processor->R[instruction.itype.rs1]) >>
                    (sign_extend_number(instruction.itype.imm, 12));
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    break;
            }
            break;
        case 0x6:
            // print_itype_except_load("ori", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
            ((sWord)processor->R[instruction.itype.rs1]) |
            (sign_extend_number(instruction.itype.imm, 12));
            break;
        case 0x7:
            // print_itype_except_load("andi", instruction, instruction.itype.imm);
            processor->R[instruction.itype.rd] = 
            ((sWord)processor->R[instruction.itype.rs1]) &
            (sign_extend_number(instruction.itype.imm, 12));
            break;
      /* call print_itype_except_load */
        default:
            handle_invalid_instruction(instruction);
            break;  
    }
    // update PC
    processor->PC += 4;
}

void execute_ecall(Processor *p, Byte *memory) {
    Register i;
    
    // syscall number is given by a0 (x10)
    // argument is given by a1
    switch(p->R[10]) {
        case 1: // print an integer
            printf("%d",p->R[11]);
            p->PC += 4;
            break;
        case 4: // print a string
            for(i=p->R[11];i<MEMORY_SPACE && load(memory,i,LENGTH_BYTE);i++) {
                printf("%c",load(memory,i,LENGTH_BYTE));
            }
            p->PC += 4;
            break;
        case 10: // exit
            printf("exiting the simulator\n");
            exit(0);
            break;
        case 11: // print a character
            printf("%c",p->R[11]);
            p->PC += 4;
            break;
        default: // undefined ecall
            printf("Illegal ecall number %d\n", p->R[10]);
            exit(-1);
            break;
    }
}

void execute_branch(Instruction instruction, Processor *processor) {
    switch (instruction.sbtype.funct3) {
        case 0x0:
            // print_branch("beq", instruction);
            // if(rs1 == rs2) PC += imm
            // int imm = (instruction.sbtype.imm7 << 5) | instruction.sbtype.imm5;

            if(processor->R[instruction.sbtype.rs1] == processor->R[instruction.sbtype.rs2]){
                // PC += imm
                processor->PC += get_branch_offset(instruction);
            } else {
                // update PC
                processor->PC += 4;
            }
            break;
        case 0x1:
            // print_branch("bne", instruction);
            if(processor->R[instruction.sbtype.rs1] != processor->R[instruction.sbtype.rs2]){
                // PC += imm
                processor->PC += get_branch_offset(instruction);
            } else {
                // update PC
                processor->PC += 4;
            }
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }

    
}

void execute_load(Instruction instruction, Processor *processor, Byte *memory) {
    switch (instruction.itype.funct3) {
        case 0x0:
            // lb load byte
            // rd = M[rs1+imm][0:7]
            processor->R[instruction.itype.rd] = load(memory,
                (sWord)processor->R[instruction.itype.rs1] + (sWord)instruction.itype.imm,
                LENGTH_BYTE);
            break;
        case 0x1:
            // print_load("lh", instruction);
            processor->R[instruction.itype.rd] = load(memory,
                (sWord)processor->R[instruction.itype.rs1] + (sWord)instruction.itype.imm,
                LENGTH_HALF_WORD);
            break;
        case 0x2:
            // print_load("lw", instruction);
            processor->R[instruction.itype.rd] = load(memory,
                (sWord)processor->R[instruction.itype.rs1] + (sWord)instruction.itype.imm,
                LENGTH_WORD);
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }

    // update PC
    processor->PC += 4;
}

void execute_store(Instruction instruction, Processor *processor, Byte *memory) {
    int mem_address;
    switch (instruction.stype.funct3) {
        /* YOUR CODE HERE */
        case 0x0:
            mem_address = 
                ((sWord)processor->R[instruction.stype.rs1]) +
                (get_store_offset(instruction));
            // processor->R[instruction.stype.rs2] = load(memory, mem_address, LENGTH_BYTE);
            store(memory, mem_address, LENGTH_BYTE, processor->R[instruction.stype.rs2]);
            break;
        case 0x1:
            mem_address = 
                ((sWord)processor->R[instruction.stype.rs1]) +
                (get_store_offset(instruction));
            // processor->R[instruction.stype.rs2] = load(memory, mem_address, LENGTH_HALF_WORD);
            store(memory, mem_address, LENGTH_HALF_WORD, processor->R[instruction.stype.rs2]);
            break;
        case 0x2:
            mem_address = 
                ((sWord)processor->R[instruction.stype.rs1]) +
                (get_store_offset(instruction));
            // processor->R[instruction.stype.rs2] = load(memory, mem_address, LENGTH_WORD);
            store(memory, mem_address, LENGTH_WORD, processor->R[instruction.stype.rs2]);

            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    // update PC
    processor->PC += 4;
}

void execute_jal(Instruction instruction, Processor *processor) {
    // rd = PC+4; PC += imm

    processor->R[instruction.ujtype.rd] = processor->PC + 4;

    processor->PC = processor->PC + (sWord)get_jump_offset(instruction) & (~1);

}

void execute_lui(Instruction instruction, Processor *processor) {
    /* YOUR CODE HERE */
    switch (instruction.utype.opcode){
        case 0x17:
            processor->R[instruction.utype.rd] = 
            (processor->PC) +
            ((sign_extend_number(instruction.itype.imm, 20)) << 12);
            break;
        case 0x37:
            processor->R[instruction.utype.rd] = 
            (sign_extend_number(instruction.utype.imm, 20)) << 12;
            break;
	/* deal with other cases */
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    // update PC
    processor->PC += 4;
}

void store(Byte *memory, Address address, Alignment alignment, Word value) {
    if(alignment == LENGTH_BYTE) {
        memory[address]= value;
    } else if(alignment == LENGTH_HALF_WORD) {
        int bit8 = value >> 8;
        memory[address] = value;
        memory[address+1] = bit8;
    } else if(alignment == LENGTH_WORD) {
        int bit0 = value;
        int bit8 = bit0 >> 8;
        int bit16 = bit8 >> 8;
        int bit24 = bit16 >> 8;
        memory[address] = value;
        memory[address+1] = bit8;
        memory[address+2] = bit16;
        memory[address+3] = bit24;
    } else {
        printf("Error: Unrecognized alignment %d\n", alignment);
        exit(-1);
    }
}

Word load(Byte *memory, Address address, Alignment alignment) {
    if(alignment == LENGTH_BYTE) {
        return memory[address];
    } else if(alignment == LENGTH_HALF_WORD) {
        return (memory[address+1] << 8) + memory[address];
    } else if(alignment == LENGTH_WORD) {
        return (memory[address+3] << 24) + (memory[address+2] << 16)
               + (memory[address+1] << 8) + memory[address];
    } else {
        printf("Error: Unrecognized alignment %d\n", alignment);
        exit(-1);
    }
}
