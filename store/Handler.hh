#ifndef __STORE__HANDLER_HH__
#define __STORE__HANDLER_HH__

class Handler {
public:
  virtual void PrepareForGC(Block *p) = 0;
};

#endif __STORE__HANDLER_HH__
