#include "common.h"

static char *OptionOutputFile;
static char StdandardInputOutput[] = "-";
static char *InputFilePath;

static void parseArguments(int ArgumentCount, char **ArgumentVector) {
  if (ArgumentCount == 1 || (ArgumentCount == 2 && std::string(ArgumentVector[1]) == "-opt")) {
    OptionOutputFile = StdandardInputOutput;
    InputFilePath = StdandardInputOutput;
    return;
  }

  for (int Index = 1; Index < ArgumentCount; Index++) {
    if (std::string(ArgumentVector[Index]) == "-o") {
      if (!ArgumentVector[++Index]) {
        exit(1);
      }
      OptionOutputFile = ArgumentVector[Index];
      continue;
    }

    if (!strncmp(ArgumentVector[Index], "-o", 2)) {
      OptionOutputFile = ArgumentVector[Index] + 2;
      continue;
    }

    InputFilePath = ArgumentVector[Index];
  }
}

static FILE *openFile(char *FilePath) {
  if (!FilePath || strcmp(FilePath, "-") == 0) {
    return stdout;
  }

  FILE *OutputFile = fopen(FilePath, "w");
  return OutputFile;
}

int main(int ArgumentCount, char **ArgumentVector) {
  parseArguments(ArgumentCount, ArgumentVector);

  TokenClass *Tokens = tokenizeFilePath(InputFilePath);

  ObjectClass *Program = parse(Tokens);

  optimize(Program);

  FILE *OpenedFile = openFile(OptionOutputFile);
  codeGenerate(Program, OpenedFile);
  return 0;
}
