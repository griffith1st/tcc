#include "common.h"

#define GP_MAX 8

static FILE *OutputFile;
static int DepthCount;
static ObjectClass *CurrentFunction;

static void generateExpression(NodeClass *Node);
static void generateStatement(NodeClass *Node);

static void printLine(const char *FormatString, ...) {
  va_list VariadicArguments;

  va_start(VariadicArguments, FormatString);
  vfprintf(OutputFile, FormatString, VariadicArguments);
  va_end(VariadicArguments);

  fprintf(OutputFile, "\n");
}

static int count() {
  static int Index = 1;
  return Index++;
}

static void push() {
  printLine("  addi sp, sp, -4");
  printLine("  sw a0, 0(sp)");
  DepthCount++;
}

static void pop(int Register) {
  printLine("  lw a%d, 0(sp)", Register);
  printLine("  addi sp, sp, 4");
  DepthCount--;
}

int alignTo(int Bytes, int AlignBytes) {
  return (Bytes + AlignBytes - 1) / AlignBytes * AlignBytes;
}

static void generateAddress(NodeClass *Node) {
  switch (Node->Kind) {
  case NODE_VARIABLE:
    if (Node->Variable->IsLocalVariable) {
      printLine("  addi a0, fp, %d", Node->Variable->OffsetBytes);
    } else {
      printLine("  la a0, %s", Node->Variable->Name);
    }
    return;
  case NODE_DEREFERENCE:
    generateExpression(Node->Left);
    return;
  case NODE_COMMA:
    generateExpression(Node->Left);
    generateAddress(Node->Right);
    return;
  default:
    break;
  }
}

static void load(TypeClass *Type) {
  if (Type->Size == 1) {
    printLine("  lb a0, 0(a0)");
  } else if (Type->Size == 2) {
    printLine("  lh a0, 0(a0)");
  } else if (Type->Size == 4) {
    printLine("  lw a0, 0(a0)");
  } else {
    printLine("  lw a0, 0(a0)");
  }
}

static void store(TypeClass *Type) {
  pop(1);

  if (Type->Size == 1) {
    printLine("  sb a0, 0(a1)");
  } else if (Type->Size == 2) {
    printLine("  sh a0, 0(a1)");
  } else if (Type->Size == 4) {
    printLine("  sw a0, 0(a1)");
  } else {
    printLine("  sw a0, 0(a1)");
  }
};

enum { INT8, INT16, INT32, INT64 };

static int getTypeID(TypeClass *Type) {
  switch (Type->Kind) {
  case TYPE_INTEGER:
    return INT32;
  default:
    return INT64;
  }
}

static char Int64_To_Int8[] = "  slli a0, a0, 56\n"
                              "  srai a0, a0, 56";
static char Int64_To_Int16[] = "  slli a0, a0, 48\n"
                               "  srai a0, a0, 48";
static char I64_To_Int32[] = "  slli a0, a0, 32\n"
                             "  srai a0, a0, 32";

static char *CastTable[10][10] = {

    {NULL, NULL, NULL, NULL},
    {Int64_To_Int8, NULL, NULL, NULL},
    {Int64_To_Int8, Int64_To_Int16, NULL, NULL},
    {Int64_To_Int8, Int64_To_Int16, I64_To_Int32, NULL},
};

static void cast(TypeClass *From, TypeClass *To) {
  if (To->Kind == TYPE_VOID) {
    return;
  }

  int Type1 = getTypeID(From);
  int Type2 = getTypeID(To);
  if (CastTable[Type1][Type2]) {
    printLine("%s", CastTable[Type1][Type2]);
  }
}

static void pushArguments2(NodeClass *Arguments, bool FirstPass) {
  if (!Arguments) {
    return;
  }
  pushArguments2(Arguments->Next, FirstPass);
  if ((FirstPass && !Arguments->PassByStack) || (!FirstPass && Arguments->PassByStack)) {
    return;
  }
  generateExpression(Arguments);
  push();
}

static int pushArguments(NodeClass *Arguments) {
  int Stack = 0, GeneralPorpuseRegsiter = 0;
  for (NodeClass *Argument = Arguments; Argument; Argument = Argument->Next) {
    if (GeneralPorpuseRegsiter < GP_MAX) {
      GeneralPorpuseRegsiter++;
    } else {
      Argument->PassByStack = true;
      Stack++;
    }
  }
  if ((DepthCount * 4 + Stack * 4) % 16 != 0) {
    printLine("  addi sp, sp, -4");
    DepthCount++;
    Stack++;
  }
  pushArguments2(Arguments, true);
  pushArguments2(Arguments, false);
  return Stack;
}

static void generateExpression(NodeClass *Node) {
  switch (Node->Kind) {
  case NODE_NUMBER:
    printLine("  li a0, %ld", Node->Value);
    return;
  case NODE_NEGATION:
    generateExpression(Node->Left);
    printLine("  neg a0, a0");
    return;
  case NODE_VARIABLE:
    generateAddress(Node);
    load(Node->Type);
    return;
  case NODE_DEREFERENCE:
    generateExpression(Node->Left);
    load(Node->Type);
    return;
  case NODE_ADDRESS:
    generateAddress(Node->Left);
    return;
  case NODE_ASSIGNMENT:
    generateAddress(Node->Left);
    push();
    generateExpression(Node->Right);
    store(Node->Type);
    return;
  case NODE_STATEMENT_EXPRESSION:
    for (NodeClass *SubNode = Node->Body; SubNode; SubNode = SubNode->Next) {
      generateStatement(SubNode);
    }
    return;
  case NODE_COMMA:
    generateExpression(Node->Left);
    generateExpression(Node->Right);
    return;
  case NODE_CAST:
    generateExpression(Node->Left);
    cast(Node->Left->Type, Node->Type);
    return;
  case NODE_CONDITION: {
    int CountNumber = count();
    generateExpression(Node->Condition);
    printLine("  beqz a0, .L.else.%d", CountNumber);
    generateExpression(Node->Then);
    printLine("  j .L.end.%d", CountNumber);
    printLine(".L.else.%d:", CountNumber);
    generateExpression(Node->Else);
    printLine(".L.end.%d:", CountNumber);
    return;
  }
  case NODE_NOT:
    generateExpression(Node->Left);
    printLine("  seqz a0, a0");
    return;
  case NODE_LOGICAL_AND: {
    int CountNumber = count();
    generateExpression(Node->Left);
    printLine("  beqz a0, .L.false.%d", CountNumber);
    generateExpression(Node->Right);
    printLine("  beqz a0, .L.false.%d", CountNumber);
    printLine("  li a0, 1");
    printLine("  j .L.end.%d", CountNumber);
    printLine(".L.false.%d:", CountNumber);
    printLine("  li a0, 0");
    printLine(".L.end.%d:", CountNumber);
    return;
  }
  case NODE_LOGICAL_OR: {
    int CountNumber = count();
    generateExpression(Node->Left);
    printLine("  bnez a0, .L.true.%d", CountNumber);
    generateExpression(Node->Right);
    printLine("  bnez a0, .L.true.%d", CountNumber);
    printLine("  li a0, 0");
    printLine("  j .L.end.%d", CountNumber);
    printLine(".L.true.%d:", CountNumber);
    printLine("  li a0, 1");
    printLine(".L.end.%d:", CountNumber);
    return;
  }
  case NODE_BITWISE_NOT:
    generateExpression(Node->Left);
    printLine("  not a0, a0");
    return;
  case NODE_FUNCTION_CALL: {
    int StackArguments = pushArguments(Node->Arguments);
    int GeneralPurposeRegister = 0;
    TypeClass *ThisArgument = Node->FunctionType->Parameters;
    for (NodeClass *Argument = Node->Arguments; Argument; Argument = Argument->Next) {
      ThisArgument = ThisArgument->Next;
      if (GeneralPurposeRegister < GP_MAX) {
        pop(GeneralPurposeRegister++);
      }
    }
    printLine("  call %s", Node->FunctionName);
    if (StackArguments) {
      DepthCount -= StackArguments;
      printLine("  addi sp, sp, %d", StackArguments * 4);
    }
    return;
  }
  default:
    break;
  }

  generateExpression(Node->Right);
  push();
  generateExpression(Node->Left);
  pop(1);

  switch (Node->Kind) {
  case NODE_ADDITION:
    printLine("  add a0, a0, a1");
    return;
  case NODE_SUBTRACTION:
    printLine("  sub a0, a0, a1");
    return;
  case NODE_MULTIPLICATION:
    printLine("  mul a0, a0, a1");
    return;
  case NODE_DIVISION:
    printLine("  div a0, a0, a1");
    return;
  case NODE_MODULUS:
    printLine("  rem a0, a0, a1");
    return;
  case NODE_EQUAL:
  case NODE_NOT_EQUAL:
    printLine("  xor a0, a0, a1");

    if (Node->Kind == NODE_EQUAL) {
      printLine("  seqz a0, a0");
    } else {
      printLine("  snez a0, a0");
    }
    return;
  case NODE_LESS_THAN:
    printLine("  slt a0, a0, a1");
    return;
  case NODE_LESS_THAN_OR_EQUAL:
    printLine("  slt a0, a1, a0");
    printLine("  xori a0, a0, 1");
    return;
  default:
    break;
  }
}

static void generateStatement(NodeClass *Node) {
  switch (Node->Kind) {
  case NODE_IF: {
    int CountNumber = count();
    generateExpression(Node->Condition);
    printLine("  beqz a0, .L.else.%d", CountNumber);
    generateStatement(Node->Then);
    printLine("  j .L.end.%d", CountNumber);
    printLine(".L.else.%d:", CountNumber);
    if (Node->Else) {
      generateStatement(Node->Else);
    }
    printLine(".L.end.%d:", CountNumber);
    return;
  }
  case NODE_FOR: {
    int CountNumber = count();
    if (Node->Initialize) {
      generateStatement(Node->Initialize);
    }
    printLine(".L.begin.%d:", CountNumber);
    if (Node->Condition) {
      generateExpression(Node->Condition);
      printLine("  beqz a0, %s", Node->BreakLabel);
    }
    generateStatement(Node->Then);
    printLine("%s:", Node->ContinueLabel);
    if (Node->Increment) {
      generateExpression(Node->Increment);
    }
    printLine("  j .L.begin.%d", CountNumber);
    printLine("%s:", Node->BreakLabel);
    return;
  }
  case NODE_SWITCH:
    generateExpression(Node->Condition);

    for (NodeClass *SubNode = Node->CaseNext; SubNode; SubNode = SubNode->CaseNext) {
      printLine("  li t0, %ld", SubNode->Value);
      printLine("  beq a0, t0, %s", SubNode->Label);
    }

    if (Node->DefaultCase) {
      printLine("  j %s", Node->DefaultCase->Label);
    }

    printLine("  j %s", Node->BreakLabel);
    generateStatement(Node->Then);
    printLine("%s:", Node->BreakLabel);
    return;
  case NODE_CASE:
    printLine("%s:", Node->Label);
    generateStatement(Node->Left);
    return;
  case NODE_BLOCK:
    for (NodeClass *SubNode = Node->Body; SubNode; SubNode = SubNode->Next) {
      generateStatement(SubNode);
    }
    return;
  case NODE_GOTO:
    printLine("  j %s", Node->UniqueLabel);
    return;
  case NODE_RETURN:
    if (Node->Left) {
      generateExpression(Node->Left);
    }
    printLine("  j .L.return.%s", CurrentFunction->Name);
    return;
  case NODE_EXPRESSION_STATEMENT:
    generateExpression(Node->Left);
    return;
  default:
    break;
  }
}

static void assignLocalVariableOffsets(ObjectClass *Program) {
  for (ObjectClass *Function = Program; Function; Function = Function->Next) {
    if (!Function->IsFunction) {
      continue;
    }

    int ReverseOffset = 8;

    int GeneralPurposeRegister = 0;
    for (ObjectClass *Variable = Function->Parameters; Variable; Variable = Variable->Next) {
      if (GeneralPurposeRegister < GP_MAX) {
        GeneralPurposeRegister++;
        continue;
      }

      ReverseOffset = alignTo(ReverseOffset, 4);
      Variable->OffsetBytes = ReverseOffset;
      ReverseOffset += Variable->Type->Size;
    }

    int Offset = 0;
    for (ObjectClass *Variable = Function->Locals; Variable; Variable = Variable->Next) {
      if (Variable->OffsetBytes) {
        continue;
      }

      Offset += Variable->Type->Size;
      Offset = alignTo(Offset, Variable->Type->AlignBytes);
      Variable->OffsetBytes = -Offset;
    }
    Function->StackSize = alignTo(Offset, 16);
  }
}

static void emitData(ObjectClass *Program) {
  for (ObjectClass *Variable = Program; Variable; Variable = Variable->Next) {
    if (Variable->IsFunction || !Variable->IsDefinition) {
      continue;
    }

    printLine("  .data");
    if (Variable->InitializeData) {
      printLine("%s:", Variable->Name);
      for (int Index = 0; Index < Variable->Type->Size; ++Index) {
        char Count = Variable->InitializeData[Index];
        printLine("  .byte %d", Count);
      }
    } else {
      printLine("  .globl %s", Variable->Name);
      printLine("%s:", Variable->Name);
      printLine("  .zero %d", Variable->Type->Size);
    }
  }
}

static void storeGeneral(int Register, int OffsetBytes, int Size) {
  printLine("  li t0, %d", OffsetBytes);
  printLine("  add t0, fp, t0");
  switch (Size) {
  case 1:
    printLine("  sb a%d, 0(t0)", Register);
    return;
  case 2:
    printLine("  sh a%d, 0(t0)", Register);
    return;
  case 4:
    printLine("  sw a%d, 0(t0)", Register);
    return;
  case 8:
    printLine("  sd a%d, 0(t0)", Register);
    return;
  }
}

void emitText(ObjectClass *Program) {
  for (ObjectClass *Function = Program; Function; Function = Function->Next) {
    if (!Function->IsFunction || !Function->IsDefinition) {
      continue;
    }

    printLine("  .globl %s", Function->Name);

    printLine("  .text");
    printLine("%s:", Function->Name);
    CurrentFunction = Function;

    printLine("  addi sp, sp, -8");
    printLine("  sw ra, 4(sp)");
    printLine("  sw fp, 0(sp)");
    printLine("  mv fp, sp");

    printLine("  addi sp, sp, -%d", Function->StackSize);

    int GP = 0;
    for (ObjectClass *Variable = Function->Parameters; Variable; Variable = Variable->Next) {
      if (Variable->OffsetBytes > 0) {
        continue;
      }

      storeGeneral(GP++, Variable->OffsetBytes, Variable->Type->Size);
    }

    generateStatement(Function->Body);
    assert(DepthCount == 0);

    printLine(".L.return.%s:", Function->Name);
    printLine("  mv sp, fp");
    printLine("  lw fp, 0(sp)");
    printLine("  lw ra, 4(sp)");
    printLine("  addi sp, sp, 8");
    printLine("  ret");
  }
}

void codeGenerate(ObjectClass *Program, FILE *File) {
  OutputFile = File;

  assignLocalVariableOffsets(Program);
  emitData(Program);
  emitText(Program);
}
