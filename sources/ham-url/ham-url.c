#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined _MSC_VER
#define stricmp strcasecmp
#endif

static const char* _ErrorTitleMsg = "ham-url (" __DATE__ "): enc/encq/dec input\n";
static const int _MaxInputSize = 1024 * 1024;

static char _HexDigitUpr[] = "0123456789ABCDEF";
static char _HexDigitLwr[] = "0123456789abcdef";

/* Portable character check (remember EBCDIC). Do not use isalnum() because
   its behavior is altered by the current locale.
   See http://tools.ietf.org/html/rfc3986#section-2.3
*/
static int _IsUnreserved(unsigned char inC)
{
  switch (inC) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case '-': case '.': case '_': case '~':
      return 1;
    default:
      break;
  }
  return 0;
}

// Converts a hex character to its integer value
static unsigned char _FromHex(unsigned char ch) {
  return isdigit(ch) ? ch - '0' : tolower(ch) - 'a' + 10;
}
// Converts an integer value to its hex character
static unsigned char _ToHex(unsigned char code) {
  return _HexDigitLwr[code & 15];
}

// Returns a url-encoded version of str, buf should be able to hold: strlen(str) * 3 + 1 bytes.
char* StringEncodeUrl(char* o, const char *str, int strLen) {
  const char* pstr = str;
  char* pbuf = o;
  while (*pstr) {
    if (_IsUnreserved(*pstr)) {
      *pbuf++ = *pstr;
    }
    else {
      *pbuf++ = '%';
      *pbuf++ = _ToHex(*pstr >> 4);
      *pbuf++ = _ToHex(*pstr & 15);
    }
    pstr++;
  }
  *pbuf = '\0';
  return o;
}

char* StringDecodeUrl(char* o, const char *str, int strLen) {
  const char* pstr = str;
  char* pbuf = o;
  if (strLen <= 0) {
    strLen = strlen(str);
  }
  while (*pstr) {
    if (*pstr == '%' && isxdigit(pstr[1]) && isxdigit(pstr[2])) {
      *pbuf++ = _FromHex(pstr[1]) << 4 | _FromHex(pstr[2]);
      pstr += 2;
    }
    else {
      *pbuf++ = *pstr;
    }
    pstr++;
  }
  *pbuf = '\0';
  return o;
}

int main(int argc, char** argv) {
  char* outBuffer = 0;
  int inputLen = 0;
  char* input = malloc(_MaxInputSize);

  if (argc < 3) {
    fputs(_ErrorTitleMsg, stdout);
    fputs("error: Invalid number of parameters !\n", stdout);
    goto _error;
  }

  // concatenate the input, put a space character between each parameters
  *input = 0;
  {
    int i;
    char* inputEnd = input;
    for (i = 2; i < argc; ++i) {
      int argLen = strlen(argv[i]);
      strcpy(inputEnd,argv[i]);
      inputEnd += argLen;
      if ((i+1) != argc) {
        *inputEnd = ' ';
        ++inputEnd;
      }
    }
    inputLen = inputEnd - input;
  }
  if (inputLen <= 0) {
    fputs(_ErrorTitleMsg, stdout);
    printf("error: Invalid input size '%d' !\n", inputLen);
    goto _error;
  }

  if (stricmp(argv[1],"dec") == 0) {
    const int allocSize = inputLen + 1;
    outBuffer = malloc(allocSize);
    memset(outBuffer, 0, allocSize);
    StringDecodeUrl(outBuffer, input, -1);
    fputs(outBuffer, stdout);
  }
  else if (stricmp(argv[1],"decq") == 0) {
    const int allocSize = inputLen + 1;
    outBuffer = malloc(allocSize);
    memset(outBuffer, 0, allocSize);
    StringDecodeUrl(outBuffer, input, -1);
    fputs("\"", stdout);
    fputs(outBuffer, stdout);
    fputs("\"", stdout);
  }
  else if (stricmp(argv[1],"enc") == 0) {
    const int allocSize = inputLen * 3 + 1;
    outBuffer = malloc(allocSize);
    memset(outBuffer, 0, allocSize);
    StringEncodeUrl(outBuffer, input, -1);
    fputs(outBuffer, stdout);
  }
  else {
    fputs(_ErrorTitleMsg, stdout);
    fputs("error: Unknown mode, should be enc, encq or dec !\n", stdout);
    goto _error;
  }

_error:
  fflush(stdout);
  if (input) {
    free(input);
  }
  if (outBuffer) {
    free(outBuffer);
  }
  return -1;
}
