//
// Authors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__ZLIB_HH__
#define __GENERIC__ZLIB_HH__

#if HAVE_ZLIB
#include <zlib.h>
#define COMPRESSIONLEVEL "9"
#else
#include <cstdio>
#define COMPRESSIONLEVEL ""
#define gzFile std::FILE *
#define gzopen(path, mode)		std::fopen(path, mode)
#define gzgetc(file)			std::fgetc(file)
#define gzputc(file, c)			std::fputc(c, file)
#define gzread(file, p, size)		std::fread(p, size, 1, file)
#define gzwrite(file, p, size)		std::fwrite(p, size, 1, file)
#define gzseek(file, offset, whence)	std::fseek(file, offset, whence)
#define gzrewind(file)			std::rewind(file)
#define gzclose(file)			std::fclose(file)
#endif

#endif
