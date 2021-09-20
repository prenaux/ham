#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _MAX_PATH
#  include <limits.h>
#  define _MAX_PATH (PATH_MAX-1)
#endif

#define BIN2H_VER "v2"
#define BUF_SIZE 1024

static const int NUM_COLS = 16;

int main(int argc, char** argv) {
  const char* varName = "_BIN2H";
  FILE* fpIn = NULL;
  FILE* fpOut = NULL;
  char* inputPath;
  char outputPath[_MAX_PATH];

  if (argc < 3) {
    printf("BIN2H (" BIN2H_VER "): input output (variable_name)\n");
    printf("  If variable_name ends with _RSTR a C++ multiline string is output.\n");
    printf("\n");
    printf("error: Invalid number of parameters !\n");
    goto _error;
  }

  // get the input path
  inputPath = argv[1];
  fpIn = fopen(inputPath, "rb");
  if (!fpIn) {
    printf("BIN2H: Can't open input file '%s' !\n", inputPath);
    goto _error;
  }

  // get the output path
  if (argc > 2) {
    strcpy(outputPath, argv[2]);
  }
  else {
    strcpy(outputPath, inputPath);
    strcat(outputPath, ".h");
  }
  fpOut = fopen(outputPath, "wb");
  if (!fpOut) {
    printf("BIN2H: Can't open output file '%s' !\n", inputPath);
    goto _error;
  }

  // get the variable name
  if (argc > 3) {
    varName = argv[3];
  }

  int isRawString = 0;
  if (strstr(varName,"_RSTR") != NULL) {
    isRawString = 1;
  }

  if (isRawString) {
    char buf[BUF_SIZE];
    int size = 0;
    fprintf(fpOut, "const char* %s = R\"\"\"(\n", varName);
    while (1) {
      int n = fread(buf,1,BUF_SIZE,fpIn);
      if (n > 0) {
        fwrite(buf,1,n,fpOut);
        size += n;
      }
      if (n != BUF_SIZE) {
        break;
      }
    }
    fprintf(fpOut, "\n)\"\"\";\n");
    fprintf(fpOut, "int %s_LEN = %d;\n", varName, size);
  }
  else {
    unsigned char buf[BUF_SIZE];
    int size = 0;
    fprintf(fpOut, "unsigned char %s_DATA[] = {", varName);
    while (1) {
      int n = fread(buf,1,BUF_SIZE,fpIn);
      for (int i = 0; i < n; ++i) {
        unsigned char byte = buf[i];
        if ((size % NUM_COLS) == 0) {
          fprintf(fpOut, "\n ");
        }
        if (size == 0) {
          fprintf(fpOut, "0x%02x", byte);
        }
        else {
          fprintf(fpOut, ",0x%02x", byte);
        }
        ++size;
      }
      if (n != BUF_SIZE)
        break;
    }
    fprintf(fpOut, "\n};\n");
    fprintf(fpOut, "int %s_DATA_SIZE = %d;\n", varName, size);
  }

	return 0;

_error:
  if (fpIn) fclose(fpIn);
  if (fpOut) fclose(fpOut);
  return -1;
}
