#include "common.h"

struct VariableScopeClass;
struct VariableScopeClass {
  VariableScopeClass *Next;
  const char *Name;
  ObjectClass *Variable;
  TypeClass *Typedef;
};

struct ScopeClass;
struct ScopeClass {
  ScopeClass *Next;
  VariableScopeClass *Variables;
};

ObjectClass *Locals;
ObjectClass *Globals;
static ScopeClass *CurrentScope = new ScopeClass{};

static ObjectClass *CurrentFunction;

static char *BreakLabel;
static char *ContinueLabel;

static NodeClass *CurrentSwitch;

static bool isTypename(TokenClass *Token);
static TypeClass *declarationSpecification(TokenClass **RestTokens, TokenClass *Token);
static TypeClass *typeSuffix(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type);
static TypeClass *declarator(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type);
static NodeClass *declaration(TokenClass **RestTokens, TokenClass *Token, TypeClass *BaseType);
static NodeClass *compoundStatement(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *statement(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *expressionStatement(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *expression(TokenClass **RestTokens, TokenClass *Token);
static int64_t constExpression(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *assign(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *Conditionitional(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *logicalOr(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *logicalAnd(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *bitwiseOr(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *bitwiseExclusiveOr(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *bitwiseAnd(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *equality(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *relational(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *shift(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *addition(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *newAddition(NodeClass *Left, NodeClass *Right, TokenClass *Token);
static NodeClass *newSubtraction(NodeClass *Left, NodeClass *Right, TokenClass *Token);
static NodeClass *multiplication(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *cast(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *unary(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *postfix(TokenClass **RestTokens, TokenClass *Token);
static NodeClass *primary(TokenClass **RestTokens, TokenClass *Token);

static void enterScope() {
  ScopeClass *Scope = new ScopeClass{};
  Scope->Next = CurrentScope;
  CurrentScope = Scope;
}

static void leaveScope() {
  CurrentScope = CurrentScope->Next;
}

static VariableScopeClass *findVariable(TokenClass *Token) {
  for (ScopeClass *Scope = CurrentScope; Scope; Scope = Scope->Next) {
    for (VariableScopeClass *SubScope = Scope->Variables; SubScope; SubScope = SubScope->Next) {
      if (is(Token, SubScope->Name)) {
        return SubScope;
      }
    }
  }
  return NULL;
}

static NodeClass *newNode(NodeKindClass Kind, TokenClass *Token) {
  NodeClass *Node = new NodeClass{};
  Node->Kind = Kind;
  Node->Token = Token;
  return Node;
}

static NodeClass *newUnary(NodeKindClass Kind, NodeClass *Expression, TokenClass *Token) {
  NodeClass *Node = newNode(Kind, Token);
  Node->Left = Expression;
  return Node;
}

static NodeClass *newBinary(NodeKindClass Kind, NodeClass *Left, NodeClass *Right, TokenClass *Token) {
  NodeClass *Node = newNode(Kind, Token);
  Node->Left = Left;
  Node->Right = Right;
  return Node;
}

static NodeClass *newNumber(int64_t Value, TokenClass *Token) {
  NodeClass *Node = newNode(NODE_NUMBER, Token);
  Node->Value = Value;
  return Node;
}

static NodeClass *newInteger(int64_t Value, TokenClass *Token) {
  NodeClass *Node = newNode(NODE_NUMBER, Token);
  Node->Value = Value;
  Node->Type = TypeInteger;
  return Node;
}

static NodeClass *newVariableNode(ObjectClass *Variable, TokenClass *Token) {
  NodeClass *Node = newNode(NODE_VARIABLE, Token);
  Node->Variable = Variable;
  return Node;
}

NodeClass *newCast(NodeClass *Expression, TypeClass *Type) {
  addType(Expression);

  NodeClass *Node = new NodeClass{};
  Node->Kind = NODE_CAST;
  Node->Token = Expression->Token;
  Node->Left = Expression;
  Node->Type = copyType(Type);
  return Node;
}

static VariableScopeClass *pushScope(const char *Name) {
  VariableScopeClass *Scope = new VariableScopeClass{};
  Scope->Name = Name;
  Scope->Next = CurrentScope->Variables;
  CurrentScope->Variables = Scope;
  return Scope;
}

static ObjectClass *newVariable(const char *Name, TypeClass *Type) {
  ObjectClass *Variable = new ObjectClass{};
  Variable->Name = Name;
  Variable->Type = Type;
  pushScope(Name)->Variable = Variable;
  return Variable;
}

static ObjectClass *newLocalVariable(const char *Name, TypeClass *Type) {
  ObjectClass *Variable = newVariable(Name, Type);
  Variable->IsLocalVariable = true;
  Variable->Next = Locals;
  Locals = Variable;
  return Variable;
}

static ObjectClass *newGlobalVariable(char *Name, TypeClass *Type) {
  ObjectClass *Variable = newVariable(Name, Type);
  Variable->Next = Globals;
  Globals = Variable;
  return Variable;
}

static char *format(const char *FormatString, ...) {
  char *BufferArea;
  size_t BufferLenth;
  FILE *OutputFile = open_memstream(&BufferArea, &BufferLenth);

  va_list VariadicArguments;
  va_start(VariadicArguments, FormatString);
  vfprintf(OutputFile, FormatString, VariadicArguments);
  va_end(VariadicArguments);

  fclose(OutputFile);
  return BufferArea;
}

static char *newUniqueName() {
  static int NameIndex = 0;
  return format(".L..%d", NameIndex++);
}

static ObjectClass *newAnonGlobalVariable(TypeClass *Type) {
  return newGlobalVariable(newUniqueName(), Type);
}

static ObjectClass *newStringLiteral(char *String, TypeClass *Type) {
  ObjectClass *Variable = newAnonGlobalVariable(Type);
  Variable->InitializeData = String;
  return Variable;
}

static char *getIdentifier(TokenClass *Token) {
  return strndup(Token->Location, Token->Length);
}

static TypeClass *findTypedef(TokenClass *Token) {
  if (Token->Kind == TOKEN_IDENTIFIER) {
    VariableScopeClass *Scope = findVariable(Token);
    if (Scope) {
      return Scope->Typedef;
    }
  }
  return NULL;
}

static TypeClass *declarationSpecification(TokenClass **RestTokens, TokenClass *Token) {
  enum { VOID, INT, OTHER };

  TypeClass *Type = TypeInteger;
  int Counter = 0;
  while (isTypename(Token)) {
    if (is(Token, "typedef") || is(Token, "static")) {
      Token = Token->Next;
      continue;
    }

    if (is(Token, "void")) {
      Counter += VOID;
    } else if (is(Token, "int")) {
      Counter += INT;
    }

    switch (Counter) {
    case VOID:
      Type = TypeVoid;
      break;
    case INT:
      Type = TypeInteger;
      break;
    default:;
    }

    Token = Token->Next;
  }
  *RestTokens = Token;
  return Type;
}

static TypeClass *functionParameters(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type) {
  TypeClass Head = {};
  TypeClass *This = &Head;

  while (!is(Token, ")")) {
    if (This != &Head) {
      Token = ignore(Token, ",");
    }
    TypeClass *Type2 = declarationSpecification(&Token, Token);
    Type2 = declarator(&Token, Token, Type2);

    This->Next = copyType(Type2);
    This = This->Next;
  }

  Type = functionType(Type);
  Type->Parameters = Head.Next;
  *RestTokens = Token->Next;
  return Type;
}

static TypeClass *typeSuffix(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type) {
  if (is(Token, "(")) {
    return functionParameters(RestTokens, Token->Next, Type);
  }

  *RestTokens = Token;
  return Type;
}

static TypeClass *declarator(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type) {
  while (remove(&Token, Token, "*")) {
    Type = pointerTo(Type);
  }

  if (is(Token, "(")) {
    TokenClass *Start = Token;
    TypeClass DummyType = {};
    declarator(&Token, Start->Next, &DummyType);
    Token = ignore(Token, ")");
    Type = typeSuffix(RestTokens, Token, Type);
    return declarator(&Token, Start->Next, Type);
  }

  Type = typeSuffix(RestTokens, Token->Next, Type);
  Type->Name = Token;
  return Type;
}

static TypeClass *abstractDeclarator(TokenClass **RestTokens, TokenClass *Token, TypeClass *Type) {
  while (is(Token, "*")) {
    Type = pointerTo(Type);
    Token = Token->Next;
  }

  if (is(Token, "(")) {
    TokenClass *Start = Token;
    TypeClass DummyType = {};
    abstractDeclarator(&Token, Start->Next, &DummyType);
    Token = ignore(Token, ")");
    Type = typeSuffix(RestTokens, Token, Type);
    return abstractDeclarator(&Token, Start->Next, Type);
  }

  return typeSuffix(RestTokens, Token, Type);
}

static TypeClass *typeName(TokenClass **RestTokens, TokenClass *Token) {
  TypeClass *Type = declarationSpecification(&Token, Token);
  return abstractDeclarator(RestTokens, Token, Type);
}

static NodeClass *declaration(TokenClass **RestTokens, TokenClass *Token, TypeClass *BaseType) {
  NodeClass Head = {};
  NodeClass *This = &Head;
  int Index = 0;

  while (!is(Token, ";")) {
    if (Index++ > 0) {
      Token = ignore(Token, ",");
    }

    TypeClass *Type = declarator(&Token, Token, BaseType);
    ObjectClass *Variable = newLocalVariable(getIdentifier(Type->Name), Type);

    if (!is(Token, "=")) {
      continue;
    }

    NodeClass *Left = newVariableNode(Variable, Type->Name);
    NodeClass *Right = assign(&Token, Token->Next);
    NodeClass *Node = newBinary(NODE_ASSIGNMENT, Left, Right, Token);
    This->Next = newUnary(NODE_EXPRESSION_STATEMENT, Node, Token);
    This = This->Next;
  }

  NodeClass *Node = newNode(NODE_BLOCK, Token);
  Node->Body = Head.Next;
  *RestTokens = Token->Next;
  return Node;
}

static bool isTypename(TokenClass *Token) {
  static const char *Keywords[] = {"void", "int"};

  for (unsigned Index = 0; Index < sizeof(Keywords) / sizeof(*Keywords); ++Index) {
    if (is(Token, Keywords[Index])) {
      return true;
    }
  }
  return findTypedef(Token);
}

static NodeClass *statement(TokenClass **RestTokens, TokenClass *Token) {
  if (is(Token, "return")) {
    NodeClass *Node = newNode(NODE_RETURN, Token);

    if (remove(RestTokens, Token->Next, ";")) {
      return Node;
    }

    NodeClass *Expression = expression(&Token, Token->Next);
    *RestTokens = ignore(Token, ";");

    addType(Expression);
    Node->Left = newCast(Expression, CurrentFunction->Type->ReturnType);
    return Node;
  }

  if (is(Token, "if")) {
    NodeClass *Node = newNode(NODE_IF, Token);
    Token = ignore(Token->Next, "(");
    Node->Condition = expression(&Token, Token);
    Token = ignore(Token, ")");
    Node->Then = statement(&Token, Token);
    if (is(Token, "else")) {
      Node->Else = statement(&Token, Token->Next);
    }
    *RestTokens = Token;
    return Node;
  }

  if (is(Token, "switch")) {
    NodeClass *Node = newNode(NODE_SWITCH, Token);
    Token = ignore(Token->Next, "(");
    Node->Condition = expression(&Token, Token);
    Token = ignore(Token, ")");

    NodeClass *Switch = CurrentSwitch;
    CurrentSwitch = Node;

    char *Break = BreakLabel;
    BreakLabel = Node->BreakLabel = newUniqueName();

    Node->Then = statement(RestTokens, Token);

    CurrentSwitch = Switch;
    BreakLabel = Break;
    return Node;
  }

  if (is(Token, "case")) {
    NodeClass *Node = newNode(NODE_CASE, Token);
    int Value = constExpression(&Token, Token->Next);
    Token = ignore(Token, ":");
    Node->Label = newUniqueName();
    Node->Left = statement(RestTokens, Token);
    Node->Value = Value;
    Node->CaseNext = CurrentSwitch->CaseNext;
    CurrentSwitch->CaseNext = Node;
    return Node;
  }

  if (is(Token, "default")) {
    NodeClass *Node = newNode(NODE_CASE, Token);
    Token = ignore(Token->Next, ":");
    Node->Label = newUniqueName();
    Node->Left = statement(RestTokens, Token);
    CurrentSwitch->DefaultCase = Node;
    return Node;
  }

  if (is(Token, "for")) {
    NodeClass *Node = newNode(NODE_FOR, Token);
    Token = ignore(Token->Next, "(");

    enterScope();

    char *Break = BreakLabel;
    char *Continue = ContinueLabel;
    BreakLabel = Node->BreakLabel = newUniqueName();
    ContinueLabel = Node->ContinueLabel = newUniqueName();

    if (isTypename(Token)) {
      TypeClass *BaseType = declarationSpecification(&Token, Token);
      Node->Initialize = declaration(&Token, Token, BaseType);
    } else {
      Node->Initialize = expressionStatement(&Token, Token);
    }

    if (!is(Token, ";")) {
      Node->Condition = expression(&Token, Token);
    }
    Token = ignore(Token, ";");

    if (!is(Token, ")")) {
      Node->Increment = expression(&Token, Token);
    }
    Token = ignore(Token, ")");

    Node->Then = statement(RestTokens, Token);
    leaveScope();
    BreakLabel = Break;
    ContinueLabel = Continue;
    return Node;
  }

  if (is(Token, "while")) {
    NodeClass *Node = newNode(NODE_FOR, Token);
    Token = ignore(Token->Next, "(");
    Node->Condition = expression(&Token, Token);
    Token = ignore(Token, ")");

    char *Break = BreakLabel;
    char *Continue = ContinueLabel;
    BreakLabel = Node->BreakLabel = newUniqueName();
    ContinueLabel = Node->ContinueLabel = newUniqueName();
    Node->Then = statement(RestTokens, Token);
    BreakLabel = Break;
    ContinueLabel = Continue;
    return Node;
  }

  if (is(Token, "break")) {
    NodeClass *Node = newNode(NODE_GOTO, Token);
    Node->UniqueLabel = BreakLabel;
    *RestTokens = ignore(Token->Next, ";");
    return Node;
  }

  if (is(Token, "continue")) {
    NodeClass *Node = newNode(NODE_GOTO, Token);
    Node->UniqueLabel = ContinueLabel;
    *RestTokens = ignore(Token->Next, ";");
    return Node;
  }

  if (is(Token, "{")) {
    return compoundStatement(RestTokens, Token->Next);
  }

  return expressionStatement(RestTokens, Token);
}

static NodeClass *compoundStatement(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = newNode(NODE_BLOCK, Token);

  NodeClass Head = {};
  NodeClass *This = &Head;

  enterScope();

  while (!is(Token, "}")) {
    if (isTypename(Token) && !is(Token->Next, ":")) {
      TypeClass *BaseType = declarationSpecification(&Token, Token);

      This->Next = declaration(&Token, Token, BaseType);
    } else {
      This->Next = statement(&Token, Token);
    }
    This = This->Next;
    addType(This);
  }

  leaveScope();

  Node->Body = Head.Next;
  *RestTokens = Token->Next;
  return Node;
}

static NodeClass *expressionStatement(TokenClass **RestTokens, TokenClass *Token) {
  if (is(Token, ";")) {
    *RestTokens = Token->Next;
    return newNode(NODE_BLOCK, Token);
  }

  NodeClass *Node = newNode(NODE_EXPRESSION_STATEMENT, Token);
  Node->Left = expression(&Token, Token);
  *RestTokens = ignore(Token, ";");
  return Node;
}

static NodeClass *expression(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = assign(&Token, Token);

  if (is(Token, ",")) {
    return newBinary(NODE_COMMA, Node, expression(RestTokens, Token->Next), Token);
  }

  *RestTokens = Token;
  return Node;
}

static int64_t eval(NodeClass *Node) {
  addType(Node);

  switch (Node->Kind) {
  case NODE_ADDITION:
    return eval(Node->Left) + eval(Node->Right);
  case NODE_SUBTRACTION:
    return eval(Node->Left) - eval(Node->Right);
  case NODE_MULTIPLICATION:
    return eval(Node->Left) * eval(Node->Right);
  case NODE_DIVISION:
    return eval(Node->Left) / eval(Node->Right);
  case NODE_NEGATION:
    return -eval(Node->Left);
  case NODE_MODULUS:
    return eval(Node->Left) % eval(Node->Right);
  case NODE_EQUAL:
    return eval(Node->Left) == eval(Node->Right);
  case NODE_NOT_EQUAL:
    return eval(Node->Left) != eval(Node->Right);
  case NODE_LESS_THAN:
    return eval(Node->Left) < eval(Node->Right);
  case NODE_LESS_THAN_OR_EQUAL:
    return eval(Node->Left) <= eval(Node->Right);
  case NODE_CONDITION:
    return eval(Node->Condition) ? eval(Node->Then) : eval(Node->Else);
  case NODE_COMMA:
    return eval(Node->Right);
  case NODE_NOT:
    return !eval(Node->Left);
  case NODE_BITWISE_NOT:
    return ~eval(Node->Left);
  case NODE_LOGICAL_AND:
    return eval(Node->Left) && eval(Node->Right);
  case NODE_LOGICAL_OR:
    return eval(Node->Left) || eval(Node->Right);
  case NODE_CAST:
    if (isInteger(Node->Type)) {
      switch (Node->Type->Size) {
      case 1:
        return (uint8_t)eval(Node->Left);
      case 2:
        return (uint16_t)eval(Node->Left);
      case 4:
        return (uint32_t)eval(Node->Left);
      }
    }
    return eval(Node->Left);
  case NODE_NUMBER:
    return Node->Value;
  default:
    break;
  }

  return -1;
}

static int64_t constExpression(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = Conditionitional(RestTokens, Token);
  return eval(Node);
}

static NodeClass *toAssign(NodeClass *Binary) {
  addType(Binary->Left);
  addType(Binary->Right);
  TokenClass *Token = Binary->Token;

  ObjectClass *Variable = newLocalVariable("", pointerTo(Binary->Left->Type));

  NodeClass *Expression1 =
      newBinary(NODE_ASSIGNMENT, newVariableNode(Variable, Token), newUnary(NODE_ADDRESS, Binary->Left, Token), Token);

  NodeClass *Expression2 =
      newBinary(NODE_ASSIGNMENT, newUnary(NODE_DEREFERENCE, newVariableNode(Variable, Token), Token),
                newBinary(Binary->Kind, newUnary(NODE_DEREFERENCE, newVariableNode(Variable, Token), Token),
                          Binary->Right, Token),
                Token);

  return newBinary(NODE_COMMA, Expression1, Expression2, Token);
}

static NodeClass *assign(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = Conditionitional(&Token, Token);

  if (is(Token, "=")) {
    return Node = newBinary(NODE_ASSIGNMENT, Node, assign(RestTokens, Token->Next), Token);
  }

  if (is(Token, "+=")) {
    return toAssign(newAddition(Node, assign(RestTokens, Token->Next), Token));
  }

  if (is(Token, "-=")) {
    return toAssign(newSubtraction(Node, assign(RestTokens, Token->Next), Token));
  }

  if (is(Token, "*=")) {
    return toAssign(newBinary(NODE_MULTIPLICATION, Node, assign(RestTokens, Token->Next), Token));
  }

  if (is(Token, "/=")) {
    return toAssign(newBinary(NODE_DIVISION, Node, assign(RestTokens, Token->Next), Token));
  }

  if (is(Token, "%=")) {
    return toAssign(newBinary(NODE_MODULUS, Node, assign(RestTokens, Token->Next), Token));
  }

  *RestTokens = Token;
  return Node;
}

static NodeClass *Conditionitional(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Condition = logicalOr(&Token, Token);

  if (!is(Token, "?")) {
    *RestTokens = Token;
    return Condition;
  }

  NodeClass *Node = newNode(NODE_CONDITION, Token);
  Node->Condition = Condition;
  Node->Then = expression(&Token, Token->Next);
  Token = ignore(Token, ":");
  Node->Else = Conditionitional(RestTokens, Token);
  return Node;
}

static NodeClass *logicalOr(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = logicalAnd(&Token, Token);
  while (is(Token, "||")) {
    TokenClass *Start = Token;
    Node = newBinary(NODE_LOGICAL_OR, Node, logicalAnd(&Token, Token->Next), Start);
  }
  *RestTokens = Token;
  return Node;
}

static NodeClass *logicalAnd(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = bitwiseOr(&Token, Token);
  while (is(Token, "&&")) {
    TokenClass *Start = Token;
    Node = newBinary(NODE_LOGICAL_AND, Node, bitwiseOr(&Token, Token->Next), Start);
  }
  *RestTokens = Token;
  return Node;
}

static NodeClass *bitwiseOr(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = bitwiseExclusiveOr(&Token, Token);
  *RestTokens = Token;
  return Node;
}

static NodeClass *bitwiseExclusiveOr(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = bitwiseAnd(&Token, Token);
  *RestTokens = Token;
  return Node;
}

static NodeClass *bitwiseAnd(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = equality(&Token, Token);
  *RestTokens = Token;
  return Node;
}

static NodeClass *equality(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = relational(&Token, Token);

  while (true) {
    TokenClass *Start = Token;

    if (is(Token, "==")) {
      Node = newBinary(NODE_EQUAL, Node, relational(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, "!=")) {
      Node = newBinary(NODE_NOT_EQUAL, Node, relational(&Token, Token->Next), Start);
      continue;
    }

    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *relational(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = shift(&Token, Token);

  while (true) {
    TokenClass *Start = Token;

    if (is(Token, "<")) {
      Node = newBinary(NODE_LESS_THAN, Node, shift(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, "<=")) {
      Node = newBinary(NODE_LESS_THAN_OR_EQUAL, Node, shift(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, ">")) {
      Node = newBinary(NODE_LESS_THAN, shift(&Token, Token->Next), Node, Start);
      continue;
    }

    if (is(Token, ">=")) {
      Node = newBinary(NODE_LESS_THAN_OR_EQUAL, shift(&Token, Token->Next), Node, Start);
      continue;
    }

    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *shift(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = addition(&Token, Token);

  while (true) {
    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *newAddition(NodeClass *Left, NodeClass *Right, TokenClass *Token) {
  addType(Left);
  addType(Right);

  if (isInteger(Left->Type) && isInteger(Right->Type)) {
    return newBinary(NODE_ADDITION, Left, Right, Token);
  }

  if (!Left->Type->BaseType && Right->Type->BaseType) {
    NodeClass *Temp = Left;
    Left = Right;
    Right = Temp;
  }

  Right = newBinary(NODE_MULTIPLICATION, Right, newInteger(Left->Type->BaseType->Size, Token), Token);
  return newBinary(NODE_ADDITION, Left, Right, Token);
}

static NodeClass *newSubtraction(NodeClass *Left, NodeClass *Right, TokenClass *Token) {
  addType(Left);
  addType(Right);

  if (isInteger(Left->Type) && isInteger(Right->Type)) {
    return newBinary(NODE_SUBTRACTION, Left, Right, Token);
  }

  if (Left->Type->BaseType && isInteger(Right->Type)) {
    Right = newBinary(NODE_MULTIPLICATION, Right, newInteger(Left->Type->BaseType->Size, Token), Token);
    addType(Right);
    NodeClass *Node = newBinary(NODE_SUBTRACTION, Left, Right, Token);
    Node->Type = Left->Type;
    return Node;
  }

  if (Left->Type->BaseType && Right->Type->BaseType) {
    NodeClass *Node = newBinary(NODE_SUBTRACTION, Left, Right, Token);
    Node->Type = TypeInteger;
    return newBinary(NODE_DIVISION, Node, newNumber(Left->Type->BaseType->Size, Token), Token);
  }

  return NULL;
}

static NodeClass *addition(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = multiplication(&Token, Token);

  while (true) {
    TokenClass *Start = Token;

    if (is(Token, "+")) {
      Node = newAddition(Node, multiplication(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, "-")) {
      Node = newSubtraction(Node, multiplication(&Token, Token->Next), Start);
      continue;
    }

    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *multiplication(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = cast(&Token, Token);

  while (true) {
    TokenClass *Start = Token;

    if (is(Token, "*")) {
      Node = newBinary(NODE_MULTIPLICATION, Node, cast(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, "/")) {
      Node = newBinary(NODE_DIVISION, Node, cast(&Token, Token->Next), Start);
      continue;
    }

    if (is(Token, "%")) {
      Node = newBinary(NODE_MODULUS, Node, cast(&Token, Token->Next), Start);
      continue;
    }

    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *cast(TokenClass **RestTokens, TokenClass *Token) {
  if (is(Token, "(") && isTypename(Token->Next)) {
    TokenClass *Start = Token;
    TypeClass *Type = typeName(&Token, Token->Next);
    Token = ignore(Token, ")");
    NodeClass *Node = newCast(cast(RestTokens, Token), Type);
    Node->Token = Start;
    return Node;
  }

  return unary(RestTokens, Token);
}

static NodeClass *unary(TokenClass **RestTokens, TokenClass *Token) {
  if (is(Token, "+")) {
    return cast(RestTokens, Token->Next);
  }

  if (is(Token, "-")) {
    return newUnary(NODE_NEGATION, cast(RestTokens, Token->Next), Token);
  }

  if (is(Token, "&")) {
    return newUnary(NODE_ADDRESS, cast(RestTokens, Token->Next), Token);
  }

  if (is(Token, "*")) {
    return newUnary(NODE_DEREFERENCE, cast(RestTokens, Token->Next), Token);
  }

  if (is(Token, "!")) {
    return newUnary(NODE_NOT, cast(RestTokens, Token->Next), Token);
  }

  if (is(Token, "~")) {
    return newUnary(NODE_BITWISE_NOT, cast(RestTokens, Token->Next), Token);
  }

  if (is(Token, "++")) {
    return toAssign(newAddition(unary(RestTokens, Token->Next), newNumber(1, Token), Token));
  }

  if (is(Token, "--")) {
    return toAssign(newSubtraction(unary(RestTokens, Token->Next), newNumber(1, Token), Token));
  }

  return postfix(RestTokens, Token);
}

static NodeClass *newIncDec(NodeClass *Node, TokenClass *Token, int Addend) {
  addType(Node);
  return newCast(
      newAddition(toAssign(newAddition(Node, newNumber(Addend, Token), Token)), newNumber(-Addend, Token), Token),
      Node->Type);
}

static NodeClass *postfix(TokenClass **RestTokens, TokenClass *Token) {
  NodeClass *Node = primary(&Token, Token);

  while (true) {
    if (is(Token, "[")) {
      TokenClass *Start = Token;
      NodeClass *Index = expression(&Token, Token->Next);
      Token = ignore(Token, "]");
      Node = newUnary(NODE_DEREFERENCE, newAddition(Node, Index, Start), Start);
      continue;
    }

    if (is(Token, "++")) {
      Node = newIncDec(Node, Token, 1);
      Token = Token->Next;
      continue;
    }

    if (is(Token, "--")) {
      Node = newIncDec(Node, Token, -1);
      Token = Token->Next;
      continue;
    }

    *RestTokens = Token;
    return Node;
  }
}

static NodeClass *functionCall(TokenClass **RestTokens, TokenClass *Token) {
  TokenClass *Start = Token;
  Token = Token->Next->Next;

  VariableScopeClass *Scope = findVariable(Start);
  TypeClass *Type = Scope->Variable->Type;
  TypeClass *ParamType = Type->Parameters;

  NodeClass Head = {};
  NodeClass *This = &Head;

  while (!is(Token, ")")) {
    if (This != &Head) {
      Token = ignore(Token, ",");
    }
    NodeClass *Argument = assign(&Token, Token);
    addType(Argument);

    if (ParamType) {
      Argument = newCast(Argument, ParamType);
      ParamType = ParamType->Next;
    }
    This->Next = Argument;
    This = This->Next;
    addType(This);
  }

  *RestTokens = ignore(Token, ")");

  NodeClass *Node = newNode(NODE_FUNCTION_CALL, Start);
  Node->FunctionName = strndup(Start->Location, Start->Length);
  Node->FunctionType = Type;
  Node->Type = Type->ReturnType;
  Node->Arguments = Head.Next;
  return Node;
}

static NodeClass *primary(TokenClass **RestTokens, TokenClass *Token) {
  if (is(Token, "(") && is(Token->Next, "{")) {
    NodeClass *Node = newNode(NODE_STATEMENT_EXPRESSION, Token);
    Node->Body = compoundStatement(&Token, Token->Next->Next)->Body;
    *RestTokens = ignore(Token, ")");
    return Node;
  }

  if (is(Token, "(")) {
    NodeClass *Node = expression(&Token, Token->Next);
    *RestTokens = ignore(Token, ")");
    return Node;
  }

  if (Token->Kind == TOKEN_IDENTIFIER) {
    if (is(Token->Next, "(")) {
      return functionCall(RestTokens, Token);
    }

    VariableScopeClass *Scope = findVariable(Token);
    NodeClass *Node;
    if (Scope->Variable) {
      Node = newVariableNode(Scope->Variable, Token);
    }

    *RestTokens = Token->Next;
    return Node;
  }

  if (Token->Kind == TOKEN_STRING) {
    ObjectClass *Variable = newStringLiteral(Token->String, Token->Type);
    *RestTokens = Token->Next;
    return newVariableNode(Variable, Token);
  }

  if (Token->Kind == TOKEN_NUMBER) {
    NodeClass *Node = newNumber(Token->Value, Token);
    *RestTokens = Token->Next;
    return Node;
  }

  return NULL;
}

static void createParameterLocalVariables(TypeClass *Param) {
  if (Param) {
    createParameterLocalVariables(Param->Next);
    newLocalVariable(getIdentifier(Param->Name), Param);
  }
}

static TokenClass *function(TokenClass *Token, TypeClass *BaseType) {
  TypeClass *Type = declarator(&Token, Token, BaseType);

  ObjectClass *Function = newGlobalVariable(getIdentifier(Type->Name), Type);
  Function->IsFunction = true;
  Function->IsDefinition = !remove(&Token, Token, ";");

  if (!Function->IsDefinition) {
    return Token;
  }

  CurrentFunction = Function;
  Locals = NULL;
  enterScope();
  createParameterLocalVariables(Type->Parameters);
  Function->Parameters = Locals;

  Token = ignore(Token, "{");
  Function->Body = compoundStatement(&Token, Token);
  Function->Locals = Locals;
  leaveScope();
  return Token;
}

static TokenClass *globalVariable(TokenClass *Token, TypeClass *BaseType) {
  bool First = true;

  while (!remove(&Token, Token, ";")) {
    if (!First) {
      Token = ignore(Token, ",");
    }
    First = false;

    TypeClass *Type = declarator(&Token, Token, BaseType);
    newGlobalVariable(getIdentifier(Type->Name), Type);
  }
  return Token;
}

static bool isFunction(TokenClass *Token) {
  if (is(Token, ";")) {
    return false;
  }

  TypeClass DummyType = {};
  TypeClass *Type = declarator(&Token, Token, &DummyType);
  return Type->Kind == TYPE_FUNCTION;
}

ObjectClass *parse(TokenClass *Token) {
  Globals = NULL;

  while (Token->Kind != TOKEN_END_OF_FILE) {
    TypeClass *BaseType = declarationSpecification(&Token, Token);

    if (isFunction(Token)) {
      Token = function(Token, BaseType);
      continue;
    }

    Token = globalVariable(Token, BaseType);
  }

  return Globals;
}
