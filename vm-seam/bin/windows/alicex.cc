//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "Seam.hh"
#include <windows.h>

// Platform SDK: 32767 maximum size excluding zero
#define MAX_ENV_VALUE_SIZE 32768
static char buffer[MAX_ENV_VALUE_SIZE];

static char *GetEnv(char *variable, char *defaults) {
  if (!GetEnvironmentVariable(variable, buffer, MAX_ENV_VALUE_SIZE))
    return defaults;
  else
    return strdup(buffer);
}

static void SetEnv(char *variable, char *value) {
  if (!SetEnvironmentVariable(variable, strdup(value))) {
    Error("Unable to set environment variable");
  }
}

static void InitAliceEnvironment() {
  char *aliceLoad       = GetEnv("ALICE_LOAD", NULL);
  char *aliceLoadPrefix = GetEnv("ALICE_LOAD_PREFIX", NULL);
  char *aliceLoadSuffix = GetEnv("ALICE_LOAD_SUFFIX", NULL);
  strcpy(buffer, "");
  if (aliceLoadPrefix != NULL) {
    strcat(buffer, aliceLoadPrefix);
    strcat(buffer, ";");
  }
  if (aliceLoad != NULL) {
    strcat(buffer, aliceLoad);
    strcat(buffer, ";");
  }
  strcat(buffer, "pattern=?{x}=?{x}.alc;pattern=?{x}=?{x}.dll");
  if (aliceLoadSuffix != 0) {
    strcat(buffer, ";");
    strcat(buffer, aliceLoadSuffix);
  }
  SetEnv("ALICE_LOAD", strdup(buffer));
}

static char *usage() {
  fprintf(stderr, "Usage: alicerun <name> <args> ...\n");
  exit(1);
  return 0;
}

int main(int argc, char *argv[]) {
  InitSeam();
  InitAliceEnvironment();
  int brokerArgc = argc + 1;
  char *brokerArgv[brokerArgc];
  brokerArgv[0] = "alice";
  brokerArgv[1] = ROOT;
  for (int i = 1; i < argc; i++)
    brokerArgv[i + 1] = argv[i];
  String *languageId = String::New(brokerArgv[0]);
  Broker::Start(languageId, brokerArgc, brokerArgv);
  return Scheduler::Run();
}
