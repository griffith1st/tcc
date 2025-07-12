#include "common.h"

TypeClass *TypeVoid = new TypeClass{TYPE_VOID, 1, 1, nullptr, nullptr, nullptr, nullptr, nullptr};
TypeClass *TypeInteger = new TypeClass{TYPE_INTEGER, 4, 4, nullptr, nullptr, nullptr, nullptr, nullptr};

static TypeClass *newType(TypeKind Kind, int Size, int AlignBytes) {
  TypeClass *Type = new TypeClass{};
  Type->Kind = Kind;
  Type->Size = Size;
  Type->AlignBytes = AlignBytes;
  return Type;
}

bool isInteger(TypeClass *Type) {
  return Type->Kind == TYPE_INTEGER;
}

TypeClass *copyType(TypeClass *Type) {
  TypeClass *NewType = new TypeClass{};
  *NewType = *Type;
  return NewType;
}

TypeClass *pointerTo(TypeClass *BaseType) {
  TypeClass *Type = newType(TYPE_POINTER, 8, 8);
  Type->BaseType = BaseType;
  return Type;
}

TypeClass *functionType(TypeClass *ReturnType) {
  TypeClass *Type = new TypeClass{};
  Type->Kind = TYPE_FUNCTION;
  Type->ReturnType = ReturnType;
  return Type;
}

static TypeClass *getCommonType(TypeClass *Type1, TypeClass *Type2) {
  if (Type1->BaseType) {
    return pointerTo(Type1->BaseType);
  }

  if (Type1->Size != Type2->Size) {
    return (Type1->Size < Type2->Size) ? Type2 : Type1;
  }

  return TypeInteger;
}

static void usualArithConv(NodeClass **Left, NodeClass **Right) {
  TypeClass *Type = getCommonType((*Left)->Type, (*Right)->Type);
  *Left = newCast(*Left, Type);
  *Right = newCast(*Right, Type);
}

void addType(NodeClass *Node) {
  if (!Node || Node->Type) {
    return;
  }

  addType(Node->Left);
  addType(Node->Right);
  addType(Node->Condition);
  addType(Node->Then);
  addType(Node->Else);
  addType(Node->Initialize);
  addType(Node->Increment);

  for (NodeClass *SubNode = Node->Body; SubNode; SubNode = SubNode->Next) {
    addType(SubNode);
  }
  for (NodeClass *SubNode = Node->Arguments; SubNode; SubNode = SubNode->Next) {
    addType(SubNode);
  }

  switch (Node->Kind) {
  case NODE_NUMBER:
    Node->Type = TypeInteger;
    return;
  case NODE_ADDITION:
  case NODE_SUBTRACTION:
  case NODE_MULTIPLICATION:
  case NODE_DIVISION:
  case NODE_MODULUS:
    usualArithConv(&Node->Left, &Node->Right);
    Node->Type = Node->Left->Type;
    return;
  case NODE_NEGATION: {
    TypeClass *Type = getCommonType(TypeInteger, Node->Left->Type);
    Node->Left = newCast(Node->Left, Type);
    Node->Type = Type;
    return;
  }
  case NODE_ASSIGNMENT:
    Node->Type = Node->Left->Type;
    return;
  case NODE_EQUAL:
  case NODE_NOT_EQUAL:
  case NODE_LESS_THAN:
  case NODE_LESS_THAN_OR_EQUAL:
    usualArithConv(&Node->Left, &Node->Right);
    Node->Type = TypeInteger;
    return;
  case NODE_FUNCTION_CALL:
    Node->Type = Node->FunctionType->ReturnType;
    return;
  case NODE_NOT:
  case NODE_LOGICAL_OR:
  case NODE_LOGICAL_AND:
    Node->Type = TypeInteger;
    return;
  case NODE_BITWISE_NOT:
    Node->Type = Node->Left->Type;
    return;
  case NODE_VARIABLE:
    Node->Type = Node->Variable->Type;
    return;
  case NODE_CONDITION:
    if (Node->Then->Type->Kind == TYPE_VOID || Node->Else->Type->Kind == TYPE_VOID) {
      Node->Type = TypeVoid;
    } else {
      usualArithConv(&Node->Then, &Node->Else);
      Node->Type = Node->Then->Type;
    }
    return;
  case NODE_COMMA:
    Node->Type = Node->Right->Type;
    return;
  case NODE_ADDRESS: {
    TypeClass *Type = Node->Left->Type;
    Node->Type = pointerTo(Type);
    return;
  }
  case NODE_DEREFERENCE:
    Node->Type = Node->Left->Type->BaseType;
    return;
  case NODE_STATEMENT_EXPRESSION:
    if (Node->Body) {
      NodeClass *Statement = Node->Body;
      while (Statement->Next) {
        Statement = Statement->Next;
      }
      if (Statement->Kind == NODE_EXPRESSION_STATEMENT) {
        Node->Type = Statement->Left->Type;
        return;
      }
    }
    return;
  default:
    break;
  }
}
