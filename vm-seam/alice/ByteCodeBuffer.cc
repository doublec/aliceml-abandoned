//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/ByteCodeBuffer.hh"
#endif

#include "alice/ByteCodeBuffer.hh"

word *WriteBuffer::codeBuf = NULL;
u_int WriteBuffer::size = 0;
u_int WriteBuffer::top = 0;
