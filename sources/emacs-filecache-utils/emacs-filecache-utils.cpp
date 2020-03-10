#include <stdio.h>
#include <assert.h>
#include <string>
#include <vector>
#include <unistd.h>
#include "parg.h"

static const char* _kProgName = "emacs-filecache-utils";
static const char* _kProgVersion = "v1.0.0";
static const char* _kProgOptions = "hc:fo:";

void printVersion() {
  printf("%s %s\n", _kProgName, _kProgVersion);
}

void printUsage() {
  printf("usage: %s (options) command\n", _kProgName);
  printf("\n");
  printf("options:\n");
  printf("  -c path        Path of the filecache\n");
  printf("                 (default: ~/.emacs-filecache)\n");
  printf("  -o path        Output result path\n");
  printf("                 (default: stdout)\n");
  printf("  -f             Output with simple formatting\n");
  printf("                 (Only used when outputing to stdout)\n");
  printf("\n");
  printf("commands:\n");
  printf("  list           List the files in the cache\n");
  printf("  exists         List the files in the cache that are still on the disk\n");
  printf("\n");
  printf("examples:\n");
  printf("  # Remove files that are not on the disk from the cache\n");
  printf("  emacs-filecache-utils -o ~/.emacs-filecache-cleaned exists\n");
  printf("  # List all the files in the cache to stdout\n");
  printf("  emacs-filecache-utils list\n");
}

struct AutoFileHandle {
  FILE* _fp;
  AutoFileHandle(FILE* aFP) : _fp(NULL) {
    _fp = aFP;
  }
  AutoFileHandle(const char* aPath, const char* aMode) : _fp(NULL) {
    Open(aPath,aMode);
  }
  ~AutoFileHandle() {
    Close();
  }
  bool IsOK() const {
    return _fp != NULL;
  }
  bool Open(const char* aPath, const char* aMode) {
    Close();
    _fp = fopen(aPath, aMode);
    return _fp != NULL;
  }
  void Close() {
    if (_fp) {
      fclose(_fp);
      _fp = NULL;
    }
  }
  FILE* operator * () const {
    assert(IsOK());
    return _fp;
  }
};

bool fileExists(const char* name, const char* dir) {
  std::string path;
  path = dir;
  path += name;
  return (access(path.c_str(), F_OK) != -1);
}

/*

  Emacs filecache format:
  (("filename" "dir1" ... "dirN") ...)

 */
struct sFilecacheEntry {
  std::string _filename;
  std::vector<std::string> _dirs;
  std::vector<bool> _exists;

  bool hasExistingFile() const {
    for (long i = 0; i < _exists.size(); ++i) {
      if (_exists[i])
        return true;
    }
    return false;
  }

  long countDirs() const {
    return _dirs.size();
  }

  long countExists() const {
    long c = 0;
    for (long i = 0; i < _exists.size(); ++i) {
      if (_exists[i])
        ++c;
    }
    return c;
  }
};

struct sFilecache {
  std::vector<sFilecacheEntry> _entries;

  enum eWriteFlags {
    eWriteFlags_SimpleFormatting = 1 << 0,
    eWriteFlags_ExistsOnly = 1 << 1,
  };

  sFilecache() {
  }

  // Returns the end of the string
  const char* ParseString(const char* p) {
    if (*p != '"') {
      printf("E/Expected '\"' to start the string, got '%c'.\n", *p);
      return NULL;
    }
    ++p;
    while (*p && *p != '"') {
      ++p;
    }
    if (*p != '"') {
      printf("E/Expected '\"' to end the string, got '%c'.\n", *p);
      return NULL;
    }
    ++p;
    return p;
  }

  const char* ParseEntry(const char* p) {
    if (*p != '(') {
      printf("E/Expected '(' to start the entry, got '%c'.\n", *p);
      return NULL;
    }
    ++p;

    const char* fnStart = p;
    const char* fnEnd = ParseString(p);
    if (!fnEnd) {
      printf("E/Can't read entry %ld filename.\n", _entries.size());
      return NULL;
    }

    p = fnEnd;
    if (!*p || *p != ' ') {
      printf("E/Expected space to separate entry fields.\n");
      return NULL;
    }
    ++p;

    sFilecacheEntry entry;
    entry._filename = std::string(fnStart+1, (fnEnd-fnStart)-2);

    while (1) {
      const char* dirStart = p;
      const char* dirEnd = ParseString(p);
      if (!dirEnd) {
        printf("E/Can't read entry %ld directory.\n", _entries.size());
        return NULL;
      }
      p = dirEnd;
      if (!*p || (*p != ')' && *p != ' ')) {
        printf("E/Expected ')' or ' ' to end or continue entry, got '%c'.\n", *p);
        return NULL;
      }
      entry._dirs.push_back(std::string(dirStart+1, (dirEnd-dirStart)-2));
      if (*p == ')') {
        ++p;
        break;
      }
      else {
        ++p;
      }
    }

    // printf("D/Parsed entry: %s\n", entry._filename.c_str());
    entry._exists.resize(entry._dirs.size());
    for (int ii = 0; ii < entry._dirs.size(); ++ii) {
      entry._exists[ii] = fileExists(entry._filename.c_str(), entry._dirs[ii].c_str());
      // printf("D/- %s: %s\n",
             // entry._dirs[ii].c_str(),
             // entry._exists[ii] ? "exists" : "not found");
    }
    _entries.push_back(entry);
    return p;
  }

  bool Parse(FILE* aFP) {
    fseek(aFP, 0, SEEK_END);
    long fsize = ftell(aFP);
    fseek(aFP, 0, SEEK_SET);
    std::vector<char> fileContent;
    fileContent.resize(fsize+1);
    fread(reinterpret_cast<void*>(&fileContent[0]), fsize, 1, aFP);

    const char* p = &fileContent[0];
    if (*p != '(') {
      printf("E/Expected '(' to start the file, got '%c'.\n", *p);
      return false;
    }
    ++p;
    while (p && *p) {
      p = ParseEntry(p);
      if (!p) {
        printf("E/Couldn't read entry %ld\n", _entries.size());
        return false;
      }
      if (*p == ')')
        break;
      if (!*p || *p != ' ') {
        printf("E/Expected space to separate entry.\n");
        return false;
      }
      ++p;
    }
    return true;
  }

  void WriteString(FILE* aFP, const char* aString) {
    fputc('"', aFP);
    fputs(aString, aFP);
    fputc('"', aFP);
  }

  void WriteEntry(FILE* aFP, const sFilecacheEntry& aEntry, const int aWriteFlags) {
    fputc('(', aFP);
    WriteString(aFP, aEntry._filename.c_str());
    fputc(' ', aFP);
    bool first = true;
    for (int i = 0; i < aEntry._dirs.size(); ++i) {
      if (aWriteFlags&eWriteFlags_ExistsOnly) {
        if (!aEntry._exists[i])
          continue;
      }
      if (!first) {
        if (aWriteFlags&eWriteFlags_SimpleFormatting) {
          fputc('\n', aFP);
          fputc(' ', aFP);
        }
        fputc(' ', aFP);
      }
      WriteString(aFP, aEntry._dirs[i].c_str());
      first = false;
    }
    fputc(')', aFP);
  }

  void WriteAll(FILE* aFP, const int aWriteFlags) {
    fputc('(', aFP);
    bool first = true;
    for (int i = 0; i < _entries.size(); ++i) {
      const sFilecacheEntry& e = _entries[i];
      if (aWriteFlags&eWriteFlags_ExistsOnly) {
        if (!e.hasExistingFile())
          continue;
      }
      if (!first) {
        if (aWriteFlags&eWriteFlags_SimpleFormatting) {
          fputc('\n', aFP);
        }
        fputc(' ', aFP);
      }
      WriteEntry(aFP, e, aWriteFlags);
      first = false;
    }
    fputc(')', aFP);
  }

  long countEntries() const {
    long c = 0;
    for (long i = 0; i < _entries.size(); ++i) {
      c += _entries[i].countDirs();
    }
    return c;
  }

  long countExists() const {
    long c = 0;
    for (long i = 0; i < _entries.size(); ++i) {
      c += _entries[i].countExists();
    }
    return c;
  }
};

int main(int argc, char** argv) {
  std::string command;
  std::string filecachePath;
  std::string outputPath;
  std::vector<std::string> nonoptions;
  int writeFlags = (sFilecache::eWriteFlags)0;

  {
    struct parg_state ps;
    int c;
    parg_init(&ps);
    while ((c = parg_getopt(&ps, argc, argv, _kProgOptions)) != -1) {
      switch (c) {
      case 1: {
        nonoptions.push_back(ps.optarg);
        break;
      }
      case 'h': {
        printUsage();
        return EXIT_SUCCESS;
      }
      case 'v': {
        printVersion();
        return EXIT_SUCCESS;
      }
      case 'c': {
        filecachePath = ps.optarg;
        break;
      }
      case 'o': {
        outputPath = ps.optarg;
        break;
      }
      case 'f': {
        writeFlags |= sFilecache::eWriteFlags_SimpleFormatting;
        break;
      }
      case '?': {
        if (ps.optopt == 'c') {
          printf("E/option -c requires an argument\n");
        }
        else {
          printf("E/unknown option -%c\n", ps.optopt);
        }
        printf("\n");
        printUsage();
        return EXIT_FAILURE;
      }
      default: {
        printf("error: unhandled option -%c\n", c);
        return EXIT_FAILURE;
      }
      }
    }
    // gather nonoptions
    for (c = ps.optind; c < argc; ++c) {
      nonoptions.push_back(argv[c]);
    }

    if (nonoptions.size() != 1) {
      printf("E/command not specified\n\n");
      printUsage();
      return EXIT_FAILURE;
    }
    command = nonoptions[0];
  }

  if (filecachePath.empty()) {
    filecachePath = getenv("HOME");
    filecachePath += "/";
    filecachePath += ".emacs-filecache";
  }

  printf("I/Reading filecache: %s\n", filecachePath.c_str());
  sFilecache filecache;
  {
    AutoFileHandle fpFilecache(filecachePath.c_str(),"rb");
    if (!fpFilecache.IsOK()) {
      printf("E/Can't open emacs-filecache '%s'\n.", filecachePath.c_str());
      return EXIT_FAILURE;
    }
    if (!filecache.Parse(*fpFilecache)) {
      printf("E/Can't parse emacs-filecache '%s'\n.", filecachePath.c_str());
      return EXIT_FAILURE;
    }
  }

  if (strcmp(command.c_str(), "list") == 0) {
    printf("I/Number of entries: %ld\n", filecache.countEntries());
  }
  else if (strcmp(command.c_str(), "exists") == 0) {
    printf("I/Number of entries: %ld\n", filecache.countEntries());
    printf("I/Number of existing entries: %ld\n", filecache.countExists());
    writeFlags |= sFilecache::eWriteFlags_ExistsOnly;
  }
  else {
    printf("E/Unknown command '%s'\n.", command.c_str());
    return EXIT_FAILURE;
  }

  {
    printf("I/Output to: %s\n", outputPath.empty() ? "stdout" : outputPath.c_str());
    AutoFileHandle fpOut((FILE*)NULL);
    FILE* output = stdout;
    if (!outputPath.empty()) {
      if (!fpOut.Open(outputPath.c_str(),"wb")) {
        printf("E/Can't open output file '%s'\n.", outputPath.c_str());
        return EXIT_FAILURE;
      }
      output = *fpOut;
      writeFlags &= ~sFilecache::eWriteFlags_SimpleFormatting;
    }
    filecache.WriteAll(output, writeFlags);
  }

  fflush(stdout);
	return EXIT_SUCCESS;
}
