/*
 *  Authors:
 *    Ralf Scheidhauer <scheidhr@dfki.de>
 *    Leif Kornstaedt <kornstae@ps.uni-sb.de>
 * 
 *  Copyright:
 *    Ralf Scheidhauer, 1999
 *    Leif Kornstaedt, 1999
 * 
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 * 
 *  This file is part of Mozart, an implementation of Oz 3:
 *    http://www.mozart-oz.org
 * 
 *  See the file "LICENSE" or
 *    http://www.mozart-oz.org/LICENSE.html
 *  for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL
 *  WARRANTIES.
 */

#include <windows.h>
#include <stdio.h>
#include <string.h>

#include "startup.hh"

const char *ozplatform = "win32-i486";

static void ozSetenv(const char *var, const char *value)
{
  if (SetEnvironmentVariable(var,value) == FALSE) {
    panic(true,"Adding %s=%s to environment failed.\n",var,value);
  }
}

static int getParent(char *path, int n)
{
  for (int i = strlen(path) - 1; i >= 0; i--) {
    if (path[i] == '\\') {
      n--;
      if (n == 0)
	return i;
    }
  }
  return -1;
}

char *getAliceHome(bool toUnix)
{
  char *ret = ozGetenv("ALICE_HOME");
  if (ret == NULL) {
    char buffer[2048];
    GetModuleFileName(NULL,buffer,sizeof(buffer));
    int n = getParent(buffer,2);
    if (n == -1) {
      panic(false,"Cannot determine Alice installation directory.\n"
	    "Try setting the ALICE_HOME environment variable.");
    }
    buffer[n] = '\0';
    ret = buffer;
  }
  ret = strdup(ret);
  normalizePath(ret,toUnix);
  return ret;
}

static char *mozartKey = "SOFTWARE\\Mozart Consortium\\Mozart\\1.2.5";

char *getOzHome(bool toUnix)
{
  char *ozhome = getRegistry(mozartKey,"OZHOME");
  if (ozhome == NULL)
    panic(true, "Could not locate value `%s\\%s´ in registry\n",
	  mozartKey, "OZHOME");
  else
    ozhome = strdup(ozhome);
  normalizePath(ozhome,toUnix);
  return ozhome;
}

void initEnv(void)
{
  char buffer[2048];

  char *ozhome = getOzHome(true);

  ozSetenv("OZPLATFORM",ozplatform);
  ozSetenv("OZHOME",ozhome);

  char *aliceHome = getAliceHome(true);
  ozSetenv("ALICE_HOME",aliceHome);

  char *homedrive = ozGetenv("HOMEDRIVE");
  if (homedrive) homedrive = strdup(homedrive);

  char *homepath = ozGetenv("HOMEPATH");
  if (homedrive && homepath) {
    homepath = strdup(homepath);
  } else {
    homedrive = "";
    if (GetCurrentDirectory(sizeof(buffer),buffer) > sizeof(buffer) - 1)
      panic(true,"Could not determine current directory.");
    homepath = strdup(buffer);
  }
  normalizePath(homepath,true);

  char *ozpath = ozGetenv("OZPATH");
  if (ozpath == NULL) {
    ozpath = ".";
  }
  sprintf(buffer,"%s;%s/share",ozpath,ozhome);
  ozSetenv("OZPATH",buffer);

  char *path = ozGetenv("PATH");
  sprintf(buffer,"%s%s/.oz/platform/%s/lib;"
	  "%s/platform/%s/lib;"
	  "%s/bin;"
	  "%s/platform/%s"
	  "%s%s",
	  homedrive,homepath,ozplatform,
	  ozhome,ozplatform,
	  ozhome,
	  ozhome,ozplatform,
	  path? ";": "", path? path: "");
  ozSetenv("PATH",buffer);

  char *prefix = ozGetenv("ALICE_LOAD_PREFIX");
  prefix = prefix ? strdup(prefix) : NULL;

  char *suffix = ozGetenv("ALICE_LOAD_SUFFIX");
  suffix = suffix ? strdup(suffix) : NULL;

  sprintf(buffer,"%s%scache=%s%s/.alice/cache;"
	  "pattern=x-oz:?{x}=x-oz:?{x};"
	  "pattern=?{x}=?{x}.ozf;"
	  "pattern=?{x}=?{x}%s%s",
	  prefix ? prefix : "",
	  prefix ? ";" : "",
	  homedrive,homepath,
	  suffix ? ";" : "",
	  suffix ? suffix : "");
  ozSetenv("ALICE_LOAD",buffer);

  char *aliceLoad = strdup(buffer);
  char *ozLoad = ozGetenv("OZ_LOAD");
  sprintf(buffer,"%s;pattern=x-alice:/?{x}=%s/?{x}.ozf;",aliceLoad,aliceHome);
  if (ozLoad)
    sprintf(buffer+strlen(buffer),"%s",ozLoad);
  else
    sprintf(buffer+strlen(buffer),"cache=%s%s/.oz/cache;cache=%s/cache",
	    homedrive,homepath,ozhome);
  ozSetenv("OZ_LOAD",buffer);
}
