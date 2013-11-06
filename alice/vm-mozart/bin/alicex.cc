/*
 *  Author:
 *    Leif Kornstaedt <kornstae@ps.uni-sb.de>
 * 
 *  Copyright:
 *    Leif Kornstaedt, 1999
 * 
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 */

#include <windows.h>
#include <stdlib.h>
#include <string.h>

#include "startup.hh"

bool console = true;

typedef void (*MAIN)(int, char **);

int main(int argc, char **argv)
{
  initEnv();
  publishPid();

  char *ozemulator = ozGetenv("OZEMULATOR");
  if (ozemulator == NULL) {
    ozemulator = "emulator.dll";
  }

  int new_argc = argc + 4;
  char **new_argv = new char *[new_argc];
  new_argv[0] = strdup(ozemulator);
  new_argv[1] = "-u";
  new_argv[2] = "x-alice:/VMMain";
  new_argv[3] = "--";
  new_argv[4] = ROOT;
  for (int i = 1; i < argc; i++)
    new_argv[i + 4] = argv[i];

  HINSTANCE hEmulator = LoadLibrary(new_argv[0]);
  if (hEmulator == NULL) {
    panic(true,"Could not link %s.\n",new_argv[0]);
  }
  MAIN OZ_main = (MAIN) GetProcAddress(hEmulator, "OZ_main");
  if (OZ_main == NULL) {
    panic(true,"Could not find function OZ_main in %s.\n",new_argv[0]);
  }
  OZ_main(new_argc, new_argv);
  FreeLibrary(hEmulator);

  return 0;
}
