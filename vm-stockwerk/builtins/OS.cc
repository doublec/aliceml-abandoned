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

#include <cstdlib>

#include "builtins/Authoring.hh"

static char *ToCString(String *string) {
  int length = string->GetLength();
  char *s = static_cast<char *>(malloc(length + 1));
  memcpy(s, string->GetValue(), length);
  s[length] = '\0';
  return s;
};

DEFINE1(OS_Process_system) {
  DECLARE_STRING(string, x0);
  char *command = ToCString(string);
  int result = std::system(command);
  free(command);
  RETURN_INT(result);
} END

DEFINE1(OS_Process_exit) {
  DECLARE_INT(i, x0);
  std::exit(i);
} END

DEFINE1(getEnv) {
  DECLARE_STRING(string, x0);
  char *name = ToCString(string);
  char *value = std::getenv(name);
  free(name);
  RETURN(String::New(value)->ToWord());
} END
