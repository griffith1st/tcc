#include "common.h"
#include <cstdio>

static char *CurrentInput;

bool is(TokenClass *Token, const char *String) {
  return memcmp(Token->Location, String, Token->Length) == 0 && String[Token->Length] == '\0';
}

TokenClass *ignore(TokenClass *Token, const char *String) {
  if (!is(Token, String)) {
    printf("expect '%s'", String);
  }
  return Token->Next;
}

bool remove(TokenClass **RestTokens, TokenClass *Token, const char *String) {
  if (is(Token, String)) {
    *RestTokens = Token->Next;
    return true;
  }
  *RestTokens = Token;
  return false;
}

static TokenClass *newToken(TokenKindClass Kind, char *Start, char *End) {
  TokenClass *Token = new TokenClass{};
  Token->Kind = Kind;
  Token->Location = Start;
  Token->Length = End - Start;
  return Token;
}

static bool startsWith(const char *String, const char *SubString) {
  return strncmp(String, SubString, strlen(SubString)) == 0;
}

static bool isIdentifier1(char Character) {
  return ('a' <= Character && Character <= 'z') || ('A' <= Character && Character <= 'Z') || Character == '_';
}

static bool isIdentifier2(char Character) {
  return isIdentifier1(Character) || ('0' <= Character && Character <= '9');
}

static int readPunction(char *Pointer) {
  static const char *Keywords[] = {
      "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--", "%=", "&&", "||",
  };

  for (unsigned Index = 0; Index < sizeof(Keywords) / sizeof(*Keywords); ++Index) {
    if (startsWith(Pointer, Keywords[Index])) {
      return strlen(Keywords[Index]);
    }
  }

  return ispunct(*Pointer) ? 1 : 0;
}

static bool isKeyword(TokenClass *Token) {
  static const char *Keywords[] = {
      "return", "if", "else", "for", "while", "int", "void", "break", "continue", "switch", "case", "default",
  };

  for (unsigned Index = 0; Index < sizeof(Keywords) / sizeof(*Keywords); ++Index) {
    if (is(Token, Keywords[Index])) {
      return true;
    }
  }

  return false;
}

static TokenClass *readIntLiteral(char *Start) {
  char *Pointer = Start;

  int BaseType = 10;
  if (!strncasecmp(Pointer, "0x", 2) && isxdigit(Pointer[2])) {
    Pointer += 2;
    BaseType = 16;
  } else if (!strncasecmp(Pointer, "0b", 2) && (Pointer[2] == '0' || Pointer[2] == '1')) {
    Pointer += 2;
    BaseType = 2;
  } else if (*Pointer == '0') {
    BaseType = 8;
  }

  long Value = strtoul(Pointer, &Pointer, BaseType);
  TokenClass *Token = newToken(TOKEN_NUMBER, Start, Pointer);
  Token->Value = Value;
  return Token;
}

static void convertKeywords(TokenClass *Token) {
  for (TokenClass *SubToken = Token; SubToken->Kind != TOKEN_END_OF_FILE; SubToken = SubToken->Next) {
    if (isKeyword(SubToken)) {
      SubToken->Kind = TOKEN_KEYWORD;
    }
  }
}

static void addLineNumbers(TokenClass *Token) {
  char *CharPointer = CurrentInput;
  int Number = 1;

  do {
    if (CharPointer == Token->Location) {
      Token = Token->Next;
    }
    if (*CharPointer == '\n') {
      Number++;
    }
  } while (*CharPointer++);
}

TokenClass *tokenize(char *Pointer) {
  CurrentInput = Pointer;
  TokenClass Head = {};
  TokenClass *This = &Head;

  while (*Pointer) {
    if (startsWith(Pointer, "//")) {
      Pointer += 2;
      while (*Pointer != '\n') {
        Pointer++;
      }
      continue;
    }

    if (startsWith(Pointer, "/*")) {
      Pointer = strstr(Pointer + 2, "*/") + 2;
      continue;
    }

    if (isspace(*Pointer)) {
      ++Pointer;
      continue;
    }

    if (isdigit(*Pointer)) {
      This->Next = readIntLiteral(Pointer);
      This = This->Next;
      Pointer += This->Length;
      continue;
    }

    if (isIdentifier1(*Pointer)) {
      char *Start = Pointer;
      do {
        ++Pointer;
      } while (isIdentifier2(*Pointer));
      This->Next = newToken(TOKEN_IDENTIFIER, Start, Pointer);
      This = This->Next;
      continue;
    }

    int PunctionLength = readPunction(Pointer);
    if (PunctionLength) {
      This->Next = newToken(TOKEN_PUNCTION, Pointer, Pointer + PunctionLength);
      This = This->Next;
      Pointer += PunctionLength;
      continue;
    }
  }

  This->Next = newToken(TOKEN_END_OF_FILE, Pointer, Pointer);
  addLineNumbers(Head.Next);
  convertKeywords(Head.Next);
  return Head.Next;
}

static char *readFile(char *FilePath) {
  FILE *File;

  if (strcmp(FilePath, "-") == 0) {
    File = stdin;
  } else {
    File = fopen(FilePath, "r");
  }

  char *BufferArea;
  size_t BufferLenth;
  FILE *OutputFile = open_memstream(&BufferArea, &BufferLenth);

  while (true) {
    char BufferArea2[4096];
    int Numbers = fread(BufferArea2, 1, sizeof(BufferArea2), File);
    if (Numbers == 0) {
      break;
    }
    fwrite(BufferArea2, 1, Numbers, OutputFile);
  }

  if (File != stdin) {
    fclose(File);
  }

  fflush(OutputFile);
  if (BufferLenth == 0 || BufferArea[BufferLenth - 1] != '\n') {
    fputc('\n', OutputFile);
  }
  fputc('\0', OutputFile);
  fclose(OutputFile);
  return BufferArea;
}

TokenClass *tokenizeFilePath(char *FilePath) {
  return tokenize(readFile(FilePath));
}
