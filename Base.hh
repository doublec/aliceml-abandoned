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
#ifndef __BASE_HH__
#define __BASE_HH__

void AssertOutline(const char *file, int line, const char *message);
#define AssertBase(cond, message)					\
  if (!(cond)) { AssertOutline(__FILE__, __LINE__, message); exit(0); } else;

//--** should be removed
#ifdef DEBUG_CHECK
#define Assert(Cond)							\
  if (!(Cond)) AssertOutline(__FILE__, __LINE__, #Cond); else;
#else
#define Assert(Cond)
#endif

void ErrorOutline(const char *file, int line, const char *message);
#define Error(message) ErrorOutline(__FILE__, __LINE__, message); exit(0);

#endif __BASE_HH__
