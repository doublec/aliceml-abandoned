//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstring>

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "ListOp.hh"
#include "TextIO.hh"

class FilePtr : private Block {
private:
  static const int SIZE     = 1;
  static const int FILE_POS = 1;
public:
  using Block::ToWord;

  FILE *GetFile() { return (FILE *) GetArg(FILE_POS); }

  static FilePtr *New(FILE *file) {
    Block *b = Store::AllocChunk(SIZE);

    b->InitArg(FILE_POS, (word) file);
    return (FilePtr *) b;
  }
};

class Stream : private Block {
private:
  static const int SIZE     = 2;
  static const int FILE_POS = 1;
  static const int NAME_POS = 2;
public:
  using Block::ToWord;

  FILE *GetFile() { return ((FilePtr *) Store::WordToBlock(GetArg(FILE_POS)))->GetFile(); }
  char *GetName() { return ((String *) Store::WordToBlock(GetArg(NAME_POS)))->GetValue(); }

  static Stream *New(FILE *fs, String *name) {
    static BlockLabel label = Store::MakeLabel(0);
    Block *b                = Store::AllocBlock(label, SIZE);
    FilePtr *file           = FilePtr::New(fs);

    b->InitArg(FILE_POS, file->ToWord());
    b->InitArg(NAME_POS, name->ToWord());
    return (Stream *) b;
  }
};

class EnlargableString : private Block {
private:
  static const int LEN_POS = 1;
public:
  using Block::ToWord;

  char *GetValue()        { return (char *) (ar + 2); }
  int GetLength()         { return ((GetSize() - 1) * sizeof(word)); } // not String::GetLength!
  void SetLength(int len) { InitArg(LEN_POS, Store::IntToWord(len)); }

  EnlargableString *Enlarge() {
    EnlargableString *s = (EnlargableString *) Block::Enlarge();

    s->SetLength(s->GetLength());
    return s;
  }

  static EnlargableString *New(int len) {
    String *s = String::New(len);

    return (EnlargableString *) s;
  }
};

namespace Builtins {
  namespace TextIO {
    word openIn(word a) {
      String *as = (String *) Store::WordToBlock(CommonOp::Sync(a));
      FILE *fs;

      if ((fs = std::fopen(as->GetValue(), "r")) == NULL) {
	// to be determined
      }
      Assert(fs != NULL);
      return Stream::New(fs, as)->ToWord();
    }
    word inputAll(word a) {
      static int BUF_SIZE = 4096;
      static int MIN_READ = 512;
      Stream *fs          = (Stream *) Store::WordToBlock(CommonOp::Sync(a));
      FILE *f             = fs->GetFile();
      EnlargableString *s = EnlargableString::New(BUF_SIZE);
      int rsize           = 0;

      while (!std::feof(f)) {
	char *buf = (s->GetValue() + rsize);
	int size  = (s->GetLength() - rsize);

	if (size < MIN_READ) {
	  s = s->Enlarge();
	}
	else {
	  std::fgets(buf, size, f);
	  rsize += std::strlen(buf);
	}
      }
      s->SetLength(MAX(0, (rsize - 1)));
      return s->ToWord();
    }
    word inputLine(word a) {
      static int BUF_SIZE = 4096;
      static int MIN_READ = 512;
      Stream *fs          = (Stream *) Store::WordToBlock(CommonOp::Sync(a));
      FILE *f             = fs->GetFile();
      EnlargableString *s = EnlargableString::New(BUF_SIZE);
      int rsize           = 0;

      while (!std::feof(f)) {
	char *buf = (s->GetValue() + rsize);
	int size  = (s->GetLength() - rsize);

	if (size < MIN_READ) {
	  s = s->Enlarge();
	}
	else {
	  std::fgets(buf, size, f);
	  rsize += std::strlen(buf);
	  if (buf[rsize - 1] == '\n') {
	    break;
	  }
	}
      }
      s->SetLength(MAX(0, (rsize - 1)));
      return s->ToWord();
    }
    word closeIn(word a) {
      std::fclose(((Stream *) Store::WordToBlock(CommonOp::Sync(a)))->GetFile());
      return Store::IntToWord(0); // to be determined
    }
    word openOut(word a) {
      String *as = (String *) Store::WordToBlock(CommonOp::Sync(a));
      FILE *fs;

      if ((fs = std::fopen(as->GetValue(), "w")) == NULL) {
	// to be determined
      }
      Assert(fs != NULL);
      return Stream::New(fs, as)->ToWord();
    }
    word output(word a, word b) {
      FILE *f   = ((Stream *) Store::WordToBlock(CommonOp::Sync(a)))->GetFile();
      String *s = (String *) Store::WordToBlock(CommonOp::Sync(a));

      fwrite(s->GetValue(), s->GetLength(), 1, f);
      return Store::IntToWord(0); // to be determined
    }
    word output1(word a, word b) {
      FILE *f = ((Stream *) Store::WordToBlock(CommonOp::Sync(a)))->GetFile();
      
      fputc(Store::WordToInt(CommonOp::Sync(b)), f);
      return Store::IntToWord(0); // to be determined
    }
    word flushOut(word a) {
      std::fflush(((Stream *) Store::WordToBlock(CommonOp::Sync(a)))->GetFile());
      return Store::IntToWord(0); // to be determined
    }
    word closeOut(word a) {
      std::fclose(((Stream *) Store::WordToBlock(CommonOp::Sync(a)))->GetFile());
      return Store::IntToWord(0); // to be determined
    }
    word print(word a) {
      String *s = (String *) Store::WordToBlock(CommonOp::Sync(a));
      std::fwrite(s->GetValue(), s->GetLength(), 1, stdout);
      return Store::IntToWord(0); // to be determined
    }
  }
}
