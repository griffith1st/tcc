#include "common.h"

static bool isDeadCode(NodeClass *Node) {
  if (Node == NULL) {
    return true;
  }

  if (Node->Kind == NODE_IF) {
    if (Node->Condition->Kind == NODE_NUMBER && Node->Condition->Value == 0) {
      return true;
    }
  }

  if (Node->Kind == NODE_EXPRESSION_STATEMENT) {
    if (Node->Left->Kind == NODE_NUMBER && Node->Right == NULL) {
      return true;
    }
  }

  return false;
}

void optimizeDeadCode(NodeClass *Node) {
  NodeClass *This = (NodeClass *)malloc(sizeof(NodeClass)), *Head = This;
  This->Next = Node->Body;

  while (This && This->Next) {
    if (isDeadCode(This->Next)) {
      if (This->Next->Else) {
        This->Next = This->Next->Else;
      } else {
        This->Next = This->Next->Next;
      }
    }
    This = This->Next;
  }
  Node->Body = Head->Next;

  free(Head);
}

static void foldConstant(NodeClass *Node) {
  if (Node->Kind == NODE_ADDITION) {
    if (Node->Left->Kind == NODE_NUMBER && Node->Right->Kind == NODE_NUMBER) {
      Node->Value = Node->Left->Value + Node->Right->Value;
      Node->Kind = NODE_NUMBER;
      Node->Left = Node->Right = NULL;
    }
  }

  if (Node->Kind == NODE_SUBTRACTION) {
    if (Node->Left->Kind == NODE_NUMBER && Node->Right->Kind == NODE_NUMBER) {
      Node->Value = Node->Left->Value - Node->Right->Value;
      Node->Kind = NODE_NUMBER;
      Node->Left = Node->Right = NULL;
    }
  }

  if (Node->Kind == NODE_MULTIPLICATION) {
    if (Node->Left->Kind == NODE_NUMBER && Node->Right->Kind == NODE_NUMBER) {
      Node->Value = Node->Left->Value * Node->Right->Value;
      Node->Kind = NODE_NUMBER;
      Node->Left = Node->Right = NULL;
    }
  }

  if (Node->Kind == NODE_DIVISION) {
    if (Node->Left->Kind == NODE_NUMBER && Node->Right->Kind == NODE_NUMBER) {
      if (Node->Right->Value != 0) {
        Node->Value = Node->Left->Value / Node->Right->Value;
        Node->Kind = NODE_NUMBER;
        Node->Left = Node->Right = NULL;
      }
    }
  }
}

static void optimizeConstantFolding(NodeClass *Node) {
  if (Node == NULL) {
    return;
  }

  optimizeConstantFolding(Node->Left);
  optimizeConstantFolding(Node->Right);
  optimizeConstantFolding(Node->Body);
  optimizeConstantFolding(Node->Next);
  optimizeConstantFolding(Node->Condition);
  optimizeConstantFolding(Node->Then);
  optimizeConstantFolding(Node->Else);
  optimizeConstantFolding(Node->Initialize);
  optimizeConstantFolding(Node->Increment);

  foldConstant(Node);
}

void optimize(ObjectClass *Program) {
  for (ObjectClass *Function = Program; Function; Function = Function->Next) {
    optimizeDeadCode(Function->Body);
    optimizeConstantFolding(Function->Body->Body);
  }
}
