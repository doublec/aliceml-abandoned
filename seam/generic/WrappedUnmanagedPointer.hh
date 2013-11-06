//
// Author:
//   Gareth Smith <garethdanielsmith@gmail.com>
//

#ifndef __GENERIC__WRAPPEDUNMANAGEDPOINTER_HH__
#define __GENERIC__WRAPPEDUNMANAGEDPOINTER_HH__

#if defined(INTERFACE)
#pragma interface "generic/WrappedUnmanagedPointer.hh"
#endif


#include "store/Store.hh"
#include "generic/ConcreteRepresentation.hh"
#include "generic/ConcreteRepresentationHandler.hh"


template <typename T>
class SeamDll WrappedUnmanagedPointer : private ConcreteRepresentation {
private:
  enum { PTR_POS, SIZE };
public:
  using ConcreteRepresentation::ToWord;
  using ConcreteRepresentation::GetHandler;
  using ConcreteRepresentation::ReplaceHandler;
  
  static WrappedUnmanagedPointer<T> *New(T *t, ConcreteRepresentationHandler *handler) {
    ConcreteRepresentation *cr = ConcreteRepresentation::New(handler, SIZE);
    cr->Init(PTR_POS, Store::UnmanagedPointerToWord(t));
    return static_cast<WrappedUnmanagedPointer<T>*>(cr);
  }
  
  static WrappedUnmanagedPointer<T> *New(T *t) {
    return WrappedUnmanagedPointer<T>::New(t, NullConcreteRepresentationHandler::GetInstance());
  }
  
  static WrappedUnmanagedPointer<T> *FromWord(word w) {
    return static_cast<WrappedUnmanagedPointer<T>*>(ConcreteRepresentation::FromWord(w));
  }
  
  static WrappedUnmanagedPointer<T> *FromWordDirect(word w) {
    return static_cast<WrappedUnmanagedPointer<T>*>(ConcreteRepresentation::FromWordDirect(w));
  }
  
  T *GetValue() {
    return static_cast<T*>(Store::WordToUnmanagedPointer(Get(PTR_POS)));
  }
  
  void SetValue(T *t) {
    Replace(PTR_POS, Store::UnmanagedPointerToWord(t));
  }
  
  bool IsNull() {
    return GetValue() == INVALID_POINTER;
  }
  
  void SetNull() {
    SetValue(INVALID_POINTER);
  }
};

#endif
