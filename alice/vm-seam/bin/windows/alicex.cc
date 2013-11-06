//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Andreas Rossberg, 2004-2005
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

void trace(const char *s) {
  static char *active = GetEnv("ALICE_TRACE_STARTUP", NULL);
  if (active) {
    fprintf(stderr, "[%s]\n", s);
    fflush(stderr);
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
  trace("PATH");
  trace(GetEnv("PATH", NULL));
  trace("ALICE_HOME");
  trace(GetEnv("ALICE_HOME", NULL));
  trace("InitSeam");
  InitSeam();
  trace("InitAlice");
  InitAliceEnvironment();
#if PRELOAD_GTK
  // Workaround for the annoying problem we have with Windows causing
  // a deadlock on some machines when trying to load the Gtk DLL lazily.
  // No idea why this happens (probably xmlparse doing unrecommended stuff in
  // DllMain, likewise libwimp); no idea why this fixes it either (partially...),
  // but who wants to know anyway...
  trace("LoadGtk");
  static const char *dll[] = {
    "libwimp.dll",
    /* Not needed so far...
    "gdi32.dll",
    "intl.dll",
    "libglib-2.0-0.dll",
    "libgmodule-2.0-0.dll",
    "libgobject-2.0-0.dll",
    "libart_lgpl_2-2.dll",
    "libpangowin32-1.0-0.dll",
    "libpangoft2-1.0-0.dll",
    "libpango-1.0-0.dll",
    "libatk-1.0-0.dll",
    "libgdk_pixbuf-2.0-0.dll",
    "libgdk-win32-2.0-0.dll",
    "libgtk-win32-2.0-0.dll",
    "libgnomecanvas-2.dll",
    */
    NULL
    // Can we leave out some of these?
  };
  for (int i = 0; dll[i]; i++) {
    trace(dll[i]);
    if (!LoadLibrary(dll[i])) {
      fprintf(stderr, "alice: couldn't load %s\n", dll[i]);
      exit(1);
    }
  }
#endif
  trace("GetArgs");
  char *extraArgv[] = ARGS;
  int extraArgc = sizeof(extraArgv)/sizeof(char*);
  int brokerArgc = argc + extraArgc;
  char *brokerArgv[brokerArgc];
  brokerArgv[0] = "alice";
  for (int i = 0; i < extraArgc; i++)
    brokerArgv[i + 1] = extraArgv[i];
  for (int i = 1; i < argc; i++)
    brokerArgv[i + extraArgc] = argv[i];
  String *languageId = String::New(brokerArgv[0]);
  trace("StartBroker");
  Broker::Start(languageId, brokerArgc, brokerArgv);
  trace("RunScheduler");
  return Scheduler::Run();
}
