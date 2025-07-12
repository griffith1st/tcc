#define _POSIX_C_SOURCE 200809L
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

struct TypeClass;
struct NodeClass;

enum TokenKindClass {
  TOKEN_IDENTIFIER,
  TOKEN_PUNCTION,
  TOKEN_KEYWORD,
  TOKEN_STRING,
  TOKEN_NUMBER,
  TOKEN_END_OF_FILE,
};

struct TokenClass;
struct TokenClass {
  TokenKindClass Kind;
  TokenClass *Next;
  int64_t Value;
  char *Location;
  int Length;
  TypeClass *Type;
  char *String;
};

bool is(TokenClass *Token, const char *String);
TokenClass *ignore(TokenClass *Token, const char *String);
bool remove(TokenClass **RestTokens, TokenClass *Token, const char *String);
TokenClass *tokenizeFilePath(char *FilePath);

struct ObjectClass;
struct ObjectClass {
  ObjectClass *Next;
  const char *Name;
  TypeClass *Type;
  bool IsLocalVariable;
  int OffsetBytes;
  bool IsFunction;
  bool IsDefinition;
  char *InitializeData;

  ObjectClass *Parameters;
  NodeClass *Body;
  ObjectClass *Locals;
  int StackSize;
  int Alignment;
};

enum NodeKindClass {
  NODE_ADDITION,
  NODE_SUBTRACTION,
  NODE_MULTIPLICATION,
  NODE_DIVISION,
  NODE_NEGATION,
  NODE_MODULUS,
  NODE_EQUAL,
  NODE_NOT_EQUAL,
  NODE_LESS_THAN,
  NODE_LESS_THAN_OR_EQUAL,
  NODE_ASSIGNMENT,
  NODE_CONDITION,
  NODE_COMMA,
  NODE_ADDRESS,
  NODE_DEREFERENCE,
  NODE_NOT,
  NODE_BITWISE_NOT,
  NODE_LOGICAL_AND,
  NODE_LOGICAL_OR,
  NODE_RETURN,
  NODE_IF,
  NODE_FOR,
  NODE_SWITCH,
  NODE_CASE,
  NODE_BLOCK,
  NODE_GOTO,
  NODE_FUNCTION_CALL,
  NODE_EXPRESSION_STATEMENT,
  NODE_STATEMENT_EXPRESSION,
  NODE_VARIABLE,
  NODE_NUMBER,
  NODE_CAST,
};

struct NodeClass {
  NodeKindClass Kind;
  NodeClass *Next;
  TokenClass *Token;
  TypeClass *Type;
  NodeClass *Left;
  NodeClass *Right;
  NodeClass *Condition;
  NodeClass *Then;
  NodeClass *Else;
  NodeClass *Initialize;
  NodeClass *Increment;
  char *BreakLabel;
  char *ContinueLabel;
  bool PassByStack;

  NodeClass *Body;

  char *FunctionName;
  TypeClass *FunctionType;
  NodeClass *Arguments;
  char *Label;
  char *UniqueLabel;

  NodeClass *CaseNext;
  NodeClass *DefaultCase;

  ObjectClass *Variable;
  int64_t Value;
};

NodeClass *newCast(NodeClass *Expression, TypeClass *Type);
ObjectClass *parse(TokenClass *Token);

enum TypeKind {
  TYPE_VOID,
  TYPE_INTEGER,
  TYPE_POINTER,
  TYPE_FUNCTION,
};

struct TypeClass {
  TypeKind Kind;
  int Size;
  int AlignBytes;
  TypeClass *BaseType;
  TokenClass *Name;

  TypeClass *ReturnType;
  TypeClass *Parameters;
  TypeClass *Next;
};

extern TypeClass *TypeVoid;
extern TypeClass *TypeInteger;

bool isInteger(TypeClass *Type);
TypeClass *copyType(TypeClass *Type);
TypeClass *pointerTo(TypeClass *BaseType);
void addType(NodeClass *Node);
TypeClass *functionType(TypeClass *ReturnType);

void optimize(ObjectClass *Program);
void codeGenerate(ObjectClass *Program, FILE *File);
int alignTo(int NBytes, int AlignBytes);
