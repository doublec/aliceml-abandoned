//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#endif

#include "java/Authoring.hh"

DEFINE0(registerNatives) {
  RETURN_VOID;
} END

DEFINE0(currentTimeMillis) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  if (SystemTimeToFileTime(&st, &ft) == FALSE)
    Error("SystemTimeToFileTime failed"); //--**
  RETURN2(JavaLong::New(ft.dwHighDateTime, ft.dwLowDateTime)->ToWord(), null);
#else
  RETURN_INT(0); //--**
#endif
} END

DEFINE5(arraycopy) {
  word wSrc = x0;
  DECLARE_INT(srcPos, x1);
  word wDest = x2;
  DECLARE_INT(destPos, x3);
  DECLARE_INT(length, x4);
  Block *destBlock = Store::WordToBlock(wDest);
  if (destBlock == INVALID_POINTER) {
    if (Store::WordToInt(wDest) == INVALID_POINTER) REQUEST(wDest);
    THROW(NullPointerException, "dest");
  }
  Block *srcBlock = Store::WordToBlock(wSrc);
  if (srcBlock == INVALID_POINTER) {
    if (Store::WordToInt(wSrc) == INVALID_POINTER) REQUEST(wSrc);
    THROW(NullPointerException, "src");
  }
  BlockLabel destLabel = destBlock->GetLabel();
  BlockLabel srcLabel = srcBlock->GetLabel();
  if (destLabel == JavaLabel::BaseArray) {
    if (srcLabel == JavaLabel::BaseArray) {
      BaseArray *destArray = static_cast<BaseArray *>(destBlock);
      BaseArray *srcArray = static_cast<BaseArray *>(srcBlock);
      PrimitiveType::type destType = destArray->GetElementType();
      PrimitiveType::type srcType = srcArray->GetElementType();
      if (destType == srcType) {
	if (srcPos < 0)
	  THROW(IndexOutOfBoundsException, "srcPos is negative");
	if (destPos < 0)
	  THROW(IndexOutOfBoundsException, "destPos is negative");
	if (length < 0)
	  THROW(IndexOutOfBoundsException, "length is negative");
	if (static_cast<u_int>(srcPos + length) > srcArray->GetLength())
	  THROW(IndexOutOfBoundsException, "src");
	if (static_cast<u_int>(destPos + length) > destArray->GetLength())
	  THROW(IndexOutOfBoundsException, "dest");
	destArray->Copy(destPos, srcArray, srcPos, length);
      } else {
	THROW(ArrayStoreException,
	      "src and dest have different primitive component types");
      }
    } else if (srcLabel == JavaLabel::ObjectArray) {
      THROW(ArrayStoreException, "src and dest are incompatible array types");
    } else {
      THROW(ArrayStoreException, "src is not an array type");
    }
  } else if (destLabel == JavaLabel::ObjectArray) {
    if (srcLabel == JavaLabel::ObjectArray) {
      ObjectArray *destArray = static_cast<ObjectArray *>(destBlock);
      ObjectArray *srcArray = static_cast<ObjectArray *>(srcBlock);
      if (srcPos < 0)
	THROW(IndexOutOfBoundsException, "srcPos is negative");
      if (destPos < 0)
	THROW(IndexOutOfBoundsException, "destPos is negative");
      if (length < 0)
	THROW(IndexOutOfBoundsException, "length is negative");
      if (static_cast<u_int>(srcPos + length) > srcArray->GetLength())
	THROW(IndexOutOfBoundsException, "src");
      if (static_cast<u_int>(destPos + length) > destArray->GetLength())
	THROW(IndexOutOfBoundsException, "dest");
      //--** check assignment compatibility
      if (srcArray == destArray && srcPos > destPos) {
	for (u_int i = 0; i < static_cast<u_int>(length); i++)
	  destArray->Store(destPos + i, srcArray->Load(srcPos + i));
      } else
	for (u_int i = length; i--; )
	  destArray->Store(destPos + i, srcArray->Load(srcPos + i));
    } else if (srcLabel == JavaLabel::BaseArray) {
      THROW(ArrayStoreException, "src and dest are incompatible array types");
    } else {
      THROW(ArrayStoreException, "src is not an array type");
    }
  } else {
    THROW(ArrayStoreException, "dest is not an array type");
  }
  RETURN_VOID;
} END

void NativeMethodTable::java_lang_System(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "currentTimeMillis", "()J", currentTimeMillis, 0, false);
  Register(className, "arraycopy",
	   "(Ljava/lang/Object;ILjava/lang/Object;II)V", arraycopy, 5, false);
}
