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

#ifndef __STORE__BASE_HH__
#define __STORE__BASE_HH__

#ifndef NULL
#define NULL 0
#endif

void AssertOutline(const char *file, int line, const char *message);
#define AssertBase(cond, message)					\
  if (!(cond)) { AssertOutline(__FILE__, __LINE__, message); exit(0); } else;

#ifdef DEBUG_STORE
#define AssertStore(Cond) AssertBase(Cond, #Cond)
#else
#define AssertStore(Cond)
#endif

//--** should be removed
#ifdef DEBUG_CHECK
#define Assert(Cond)							\
  if (!(Cond)) AssertOutline(__FILE__, __LINE__, #Cond); else;
#else
#define Assert(Cond)
#endif

void ErrorOutline(const char *file, int line, const char *message);
#define Error(message) ErrorOutline(__FILE__, __LINE__, message); exit(0);

#endif __STORE__BASE_HH__
