#include <stdio.h>
#include <stdlib.h>

static const int NUM_COLS = 16;

int main(int argc, char** argv) {
  FILE* fpIn = NULL;
  FILE* fpOut = NULL;
  char* inputPath;
  char outputPath[_MAX_PATH];

  if (argc < 2) {
    printf("BIN2H: no input file specified !\n");
    goto _error;
  }

  inputPath = argv[1];

  strcpy(outputPath, inputPath);
  strcat(outputPath, ".h");

  fpIn = fopen(inputPath, "rb");
  if (!fpIn) {
    printf("BIN2H: Can't open input file '%s' !\n", inputPath);
    goto _error;
  }

  fpOut = fopen(outputPath, "wb");
  if (!fpOut) {
    printf("BIN2H: Can't open output file '%s' !\n", inputPath);
    goto _error;
  }

  {
    int size = 0;
    fprintf(fpOut, "unsigned char _BIN2H_DATA[] = {");
    while (1) {
      int byte = fgetc(fpIn);
      if (feof(fpIn))
        break;
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
    fprintf(fpOut, "\n};\n");
    fprintf(fpOut, "int _BIN2H_DATA_SIZE = %d;\n", size);
  }

	return 0;

_error:
  if (fpIn) fclose(fpIn);
  if (fpOut) fclose(fpOut);
  return -1;
}
