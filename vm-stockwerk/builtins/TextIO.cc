//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstring>

#include "builtins/Authoring.hh"

static const BlockLabel STREAM_LABEL = Store::MakeLabel(0);   //--**

class Stream : private Block {
private:
  static const int SIZE     = 2;
  static const int FILE_POS = 1;
  static const int NAME_POS = 2;
public:
  using Block::ToWord;

  FILE *GetFile() {
    word w = GetArg(FILE_POS);
    return static_cast<FILE *>(Store::WordToUnmanagedPointer(w));
  }
  const char *GetName() {
    return String::FromWord(GetArg(NAME_POS))->GetValue();
  }

  static Stream *New(FILE *fs, String *name) {
    Block *b = Store::AllocBlock(STREAM_LABEL, SIZE);
    //--** fs is not reclaimed upon garbage collection:
    //--** register for finalization
    b->InitArg(FILE_POS, Store::UnmanagedPointerToWord(fs));
    b->InitArg(NAME_POS, name->ToWord());
    return static_cast<Stream *>(b);
  }
  static Stream *FromWord(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b == INVALID_POINTER || b->GetLabel() == STREAM_LABEL);
    return static_cast<Stream *>(b);
  }
};

#define DECLARE_STREAM(stream, x) DECLARE_BLOCKTYPE(Stream, stream, x)

class EnlargableString: private Block {
private:
  static const int LEN_POS = 1;
public:
  using Block::ToWord;

  static EnlargableString *New(int len) {
    String *s = String::New(len);
    return reinterpret_cast<EnlargableString *>(s);
  }

  char *GetValue() {
    return reinterpret_cast<char *>(GetBase() + 1);
  }
  int GetLength() {
    return (GetSize() - 1) * sizeof(word); // not String::GetLength!
  }
  void SetLength(int len) {
    InitArg(LEN_POS, Store::IntToWord(len));
  }

  EnlargableString *Enlarge() {
    EnlargableString *s = static_cast<EnlargableString *>(Block::Enlarge());
    s->SetLength(s->GetLength());
    return s;
  }
};

DEFINE1(TextIO_openIn) {
  DECLARE_STRING(as, x0);
  FILE *fs = std::fopen(as->GetValue(), "r");
  if (fs == NULL) {
    //--** raise OS exception
  }
  RETURN(Stream::New(fs, as)->ToWord());
} END

DEFINE1(TextIO_inputAll) {
  static int BUF_SIZE = 4096;
  static int MIN_READ = 512;

  DECLARE_STREAM(fs, x0);
  FILE *f             = fs->GetFile();
  EnlargableString *s = EnlargableString::New(BUF_SIZE);
  int rsize           = 0;
  while (!feof(f)) { //--** std::
    char *buf = s->GetValue() + rsize;
    int size  = s->GetLength() - rsize;
    if (size < MIN_READ) {
      s = s->Enlarge();
    } else {
      std::fgets(buf, size, f);
      //--** error checking
      rsize += std::strlen(buf);
    }
  }
  s->SetLength(MAX(0, (rsize - 1)));
  RETURN(s->ToWord());
} END

DEFINE1(TextIO_inputLine) {
  static int BUF_SIZE = 4096;
  static int MIN_READ = 512;

  DECLARE_STREAM(fs, x0);
  FILE *f             = fs->GetFile();
  EnlargableString *s = EnlargableString::New(BUF_SIZE);
  int rsize           = 0;
  while (!feof(f)) { //--** std::
    char *buf = (s->GetValue() + rsize);
    int size  = (s->GetLength() - rsize);
    if (size < MIN_READ) {
      s = s->Enlarge();
    } else {
      std::fgets(buf, size, f);
      //--** error checking
      rsize += std::strlen(buf);
      if (buf[rsize - 1] == '\n') {
	break;
      }
    }
  }
  s->SetLength(MAX(0, (rsize - 1)));
  RETURN(s->ToWord());
} END

DEFINE1(TextIO_closeIn) {
  DECLARE_STREAM(fs, x0);
  std::fclose(fs->GetFile());
  RETURN_UNIT;
} END

DEFINE1(TextIO_openOut) {
  DECLARE_STRING(as, x0);
  FILE *fs = std::fopen(as->GetValue(), "w");
  if (fs == NULL) {
    //--** raise OS exception
  }
  RETURN(Stream::New(fs, as)->ToWord());
} END

DEFINE2(TextIO_output) {
  DECLARE_STREAM(fs, x0);
  DECLARE_STRING(s, x1);
  std::fwrite(s->GetValue(), s->GetLength(), 1, fs->GetFile());
  //--** error checking!
  RETURN_UNIT;
} END

DEFINE2(TextIO_output1) {
  DECLARE_STREAM(fs, x0);
  DECLARE_INT(c, x1);
  std::fputc(c, fs->GetFile());
  //--** error checking!
  RETURN_UNIT;
} END

DEFINE1(flushOut) {
  DECLARE_STREAM(fs, x0);
  std::fflush(fs->GetFile());
  //--** error checking!
  RETURN_UNIT;
} END

DEFINE1(TextIO_closeOut) {
  DECLARE_STREAM(fs, x0);
  std::fclose(fs->GetFile());
  RETURN_UNIT;
} END

DEFINE1(TextIO_print) {
  DECLARE_STRING(s, x0);
  std::fwrite(s->GetValue(), s->GetLength(), 1, stdout);
  //--** error checking!
  RETURN_UNIT;
} END
