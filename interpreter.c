#include <stdlib.h>
#include <stdio.h>

#define MEMSIZE 1048576

static int little_endian, icount, *instruction;
static int mem[MEMSIZE / 4];

  //Use an array to hold the counts.
static int branchCount = 0, memoryCount = 0;
static int zeroCount = 0, cycleCount = 0, registerAccess = 0;


static int Convert(unsigned int x)
{
  return (x >> 24) | ((x >> 8) & 0xff00) | ((x << 8) & 0xff0000) | (x << 24);
}

static int Fetch(int pc)
{
  pc = (pc - 0x00400000) >> 2;
  if ((unsigned)pc >= icount) {
    fprintf(stderr, "instruction fetch out of range\n");
    exit(-1);
  }
  return instruction[pc];
}

static int LoadWord(int addr)
{
  if (addr & 3 != 0) {
    fprintf(stderr, "unaligned data access\n");
    exit(-1);
  }
  addr -= 0x10000000;
  if ((unsigned)addr >= MEMSIZE) {
    fprintf(stderr, "data access out of range\n");
    exit(-1);
  }
  return mem[addr / 4];
}

static void StoreWord(int data, int addr)
{
  if (addr & 3 != 0) {
    fprintf(stderr, "unaligned data access\n");
    exit(-1);
  }
  addr -= 0x10000000;
  if ((unsigned)addr >= MEMSIZE) {
    fprintf(stderr, "data access out of range\n");
    exit(-1);
  }
  mem[addr / 4] = data;
}

static void dataDependencies(int value, int *readCount,int *writeData){
    //ignore the 0 case
    if(value == 0)
      return;

  //traverse through writeData
  int i;
  for (i = 0; i < 8 ; i++){
    if (value == writeData[i]){
      readCount[i]++;
      break;
    }
  }
  printf("read: %d\n", readCount[0]);
  int k;
  for(k = 0; k < 8; k++){
    printf("%d\n", k);
  printf("write in DD: %d\n", writeData[k]);
}
}
static void writeDependencies(int wb, int count, int *writeData){
  // int i = count % 8;
  // writeData[i] = wb;
  //   printf("write: %d\n", writeData[i]);
  int i;
  for(i = 7; i >=0; i--){
    writeData[i] = writeData[i-1];
  }
  writeData[0]=wb;
}

static void Interpret(int start)
{
  register int instr, opcode, rs, rt, rd, shamt, funct, uimm, simm, addr;
  register int pc, hi, lo;
  int reg[32];
  register int cont = 1, count = 0, i;
  register long long wide;

  lo = hi = 0;
  pc = start;
  for (i = 1; i < 32; i++) reg[i] = 0;
  reg[28] = 0x10008000;  // gp
  reg[29] = 0x10000000 + MEMSIZE;  // sp
  int readCount[8] = {0}; //For each instruction read
  int writeData[8] = {0}; //Stores the write intructions

  while (cont) {
    count++;
    instr = Fetch(pc);
    pc += 4;
    reg[0] = 0;  // $zero

    opcode = (unsigned)instr >> 26;
    rs = (instr >> 21) & 0x1f;
    rt = (instr >> 16) & 0x1f;
    rd = (instr >> 11) & 0x1f;
    shamt = (instr >> 6) & 0x1f;
    funct = instr & 0x3f;
    uimm = instr & 0xffff;
    simm = ((signed)uimm << 16) >> 16;
    addr = instr & 0x3ffffff;


    switch (opcode) {
      case 0x00:
        switch (funct) {
          case 0x00:  reg[rd] = reg[rs] << shamt;
                      dataDependencies(rs, readCount, writeData);
                      writeDependencies(rd, count, writeData);
                      cycleCount += 2;
                      registerAccess++;
                      if ( rs == 0 )
                        zeroCount ++;
          break;/* sll */


          case 0x03:  reg[rd] = reg[rs] >> (signed)shamt;
                      dataDependencies(rs, readCount, writeData);
                      writeDependencies(rd, count, writeData);
                      cycleCount += 2;
                      registerAccess++;
                      if ( rs == 0 )
                        zeroCount++;
          break;/* sra */


          case 0x08:  pc = reg[rs];
                      dataDependencies(rs, readCount, writeData);
                      writeDependencies(0, count, writeData);
                      cycleCount += 2;
                      registerAccess++;
                      if ( rs == 0 )
                        zeroCount++;
          break;/* jr */


          case 0x10:  reg[rd] = hi;
                      writeDependencies(rd, count, writeData);
                      cycleCount += 2;
                      registerAccess++;
          break; /* mfhi */


          case 0x12:  reg[rd] = lo;
                      writeDependencies(rd, count, writeData);
                      cycleCount += 2;
                      registerAccess++;
          break;/* mflo */


          case 0x18:  wide = reg[rs];
                      if ( rs == 0 )
                        zeroCount++;
                      dataDependencies(rs, readCount, writeData);
                      wide *= reg[rt];
                      dataDependencies(rt, readCount, writeData);
                      if ( rt == 0 )
                        zeroCount++;
                      lo = wide & 0xffffffff;
                      hi = wide >> 32;
                      cycleCount += 32;
                      registerAccess += 2;
                      writeDependencies(0, count, writeData);
          break;/* mult */


          case 0x1a:  if (reg[rt] == 0) {
                        fprintf(stderr, "division by zero: pc = 0x%x\n", pc-4);
                        cont = 0;
                      }else {
                        lo = reg[rs] / reg[rt];
                        dataDependencies(rs, readCount, writeData);
                        dataDependencies(rt, readCount, writeData);
                        hi = reg[rs] % reg[rt];
                        cycleCount += 32;
                        registerAccess += 2;
                        writeDependencies(0, count, writeData);
                        if ( rs == 0)
                          zeroCount +=1;
                      }



          break;/* div */


          case 0x21:  reg[rd] = reg[rs] + reg[rt];
                      cycleCount += 1;
                      registerAccess += 2;
                      if ( rs == 0 )
                        zeroCount++;
                      if ( rt == 0)
                        zeroCount++;
                      dataDependencies(rs, readCount, writeData);
                      dataDependencies(rt, readCount, writeData);
                      writeDependencies(rd, count, writeData);
          break;/* addu */


          case 0x23:  reg[rd] = reg[rs] - reg[rt];
                      cycleCount++;
                      registerAccess += 2;
                      if ( rs == 0 )
                        zeroCount++;
                      if  ( rt == 0)
                        zeroCount++;
                      dataDependencies(rs, readCount, writeData);
                      dataDependencies(rt, readCount, writeData);
                      writeDependencies(rd, count, writeData);
          break;/* subu */

          case 0x2a:  if(reg[rs] < reg[rt]){
                        reg[rd] = 1;
                      }else{
                        reg[rd]=0;
                      }
                      dataDependencies(rs, readCount, writeData);
                      dataDependencies(rt, readCount, writeData);
                      writeDependencies(rd, count, writeData);
                      if ( rs == 0 )
                        zeroCount++;
                      if  ( rt == 0)
                        zeroCount++;
                      cycleCount++;
                      registerAccess += 2;
          break;/* slt */


          default: fprintf(stderr, "unimplemented instruction: pc = 0x%x\n", pc-4); cont = 0;
        }
        break;


      case 0x02:  pc = (pc & 0xf0000000) + addr * 4;
                  cycleCount += 2;
                  writeDependencies(0, count, writeData);
      break;/* j */


      case 0x03: reg[31] = pc; pc = (pc & 0xf0000000) + addr * 4;
                 cycleCount += 2;
                 writeDependencies(31, count, writeData);
      break;/* jal */


      case 0x04:  if(reg[rs] == reg[rt]){
                    pc = pc + simm * 4;
                    cycleCount += 2; //taken
                  }
                  else{
                    cycleCount++; //not taken
                  }
                  dataDependencies(rs, readCount, writeData);
                  dataDependencies(rt, readCount, writeData);
                  branchCount++;
                  registerAccess += 2;
                  if ( rs == 0 )
                    zeroCount ++;
                  if ( rt == 0)
                    zeroCount++;
                  writeDependencies(0, count, writeData);
      break;/* beq */


      case 0x05:  if(reg[rs] != reg[rt]){
                    pc = pc + simm * 4;
                    cycleCount += 2; //taken
                  }
                  else{
                    cycleCount++; //not taken
                  }
                  dataDependencies(rs, readCount, writeData);
                  dataDependencies(rt, readCount, writeData);
                  branchCount++;
                  registerAccess += 2;
                  if ( rs == 0 )
                    zeroCount++;
                  if ( rt == 0)
                    zeroCount++;
                  writeDependencies(0, count, writeData);
      break;/* bne */


      case 0x09:  reg[rt] = reg[rs] + simm;
                  cycleCount += 1;
                  registerAccess++;
                  if ( rs == 0 )
                    zeroCount++;
                  dataDependencies(rs, readCount, writeData);
                  writeDependencies(rt, count, writeData);
      break;/* addiu */


      case 0x0c:  reg[rt] = reg[rs] &  uimm;
                  cycleCount++;
                  if ( rs == 0 )
                    zeroCount++;
                  registerAccess++;
                  dataDependencies(rs, readCount, writeData);
                  writeDependencies(rt, count, writeData);
      break;/* andi */


      case 0x0f:  reg[rt] = simm << 16;
                  cycleCount++;
                  writeDependencies(rt, count, writeData);
      break;/* lui */


      case 0x1a: /* trap */


        switch (addr & 0xf) {
          case 0x00: printf("\n");
                     cycleCount += 2;
                     writeDependencies(rt, count, writeData);
          break;


          case 0x01: printf(" %d ", reg[rs]);
                     cycleCount += 2;
                     if ( rs == 0 )
                       zeroCount++;
                     registerAccess++;
                     dataDependencies(rs, readCount, writeData);
                     writeDependencies(rt, count, writeData);
          break;


          case 0x05: printf("\n? "); fflush(stdout); scanf("%d", &reg[rt]);
                     cycleCount += 2;
                     writeDependencies(rt, count, writeData);
          break;


          case 0x0a: cont = 0;
                     cycleCount += 2;
                     writeDependencies(0, count, writeData);
          break;


          default: fprintf(stderr, "unimplemented trap: pc = 0x%x\n", pc-4); cont = 0; cycleCount += 2;
        }
        break;


      case 0x23:  reg[rt] = LoadWord(reg[rs] + simm);
                  cycleCount += 6;
                  registerAccess++;
                  memoryCount++;
                  if ( rs == 0 )
                    zeroCount++;
                  dataDependencies(rs, readCount, writeData);
                  writeDependencies(rt, count, writeData);
      break;  /* lw */ // call LoadWord function

      // instruction counter


      case 0x2b:  StoreWord(reg[rt], reg[rs] + simm);
                  cycleCount += 6;
                  registerAccess += 2;
                  memoryCount += 1;
                  if ( rs == 0 )
                    zeroCount++;
                  dataDependencies(rs, readCount, writeData);
                  dataDependencies(rt, readCount, writeData);
                  writeDependencies(0, count, writeData);
      break;  /* sw */ // call StoreWord function


      default: fprintf(stderr, "unimplemented instruction: pc = 0x%x\n", pc-4); cont = 0;
    }

  }

  printf("\nprogram finished at pc = 0x%x  (%d instructions executed)\n", pc, count);

  printf("br/instr: %.1f%%\n", 100.0 * branchCount/count);
  printf("mem/instr: %.1f%%\n", 100.0 * memoryCount/count);
  printf("zero/instr: %.1f%%\n", 100.0 * zeroCount/registerAccess);
  printf("cycle: %d\n", cycleCount);
  int j;
  for(j =0; j < 8; j++){
      printf("number of inputs produced %d", (j+1) );
      printf( " instruction ahead: %d\n", readCount[j]);
  }

}

int main(int argc, char *argv[])
{
  int c, start;
  FILE *f;

  printf("CS3339 -- MIPS Interpreter\n");
  if (argc != 2) {fprintf(stderr, "usage: %s executable\n", argv[0]); exit(-1);}
  if (sizeof(int) != 4) {fprintf(stderr, "error: need 4-byte integers\n"); exit(-1);}
  if (sizeof(long long) != 8) {fprintf(stderr, "error: need 8-byte long longs\n"); exit(-1);}

  c = 1;
  little_endian = *((char *)&c);
  f = fopen(argv[1], "r+b");
  if (f == NULL) {fprintf(stderr, "error: could not open file %s\n", argv[1]); exit(-1);}
  c = fread(&icount, 4, 1, f);
  if (c != 1) {fprintf(stderr, "error: could not read count from file %s\n", argv[1]); exit(-1);}
  if (little_endian) {
    icount = Convert(icount);
  }
  c = fread(&start, 4, 1, f);
  if (c != 1) {fprintf(stderr, "error: could not read start from file %s\n", argv[1]); exit(-1);}
  if (little_endian) {
    start = Convert(start);
  }

  instruction = (int *)(malloc(icount * 4));
  if (instruction == NULL) {fprintf(stderr, "error: out of memory\n"); exit(-1);}
  c = fread(instruction, 4, icount, f);
  if (c != icount) {fprintf(stderr, "error: could not read (all) instructions from file %s\n", argv[1]); exit(-1);}
  fclose(f);
  if (little_endian) {
    for (c = 0; c < icount; c++) {
      instruction[c] = Convert(instruction[c]);
    }
  }

  printf("running %s\n\n", argv[1]);
  Interpret(start);

}

