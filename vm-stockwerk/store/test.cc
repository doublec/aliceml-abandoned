#include <iostream.h>
#include <unistd.h>
#include "store.hh"

static word MakeNil() {
  word nil = Store::BPointerToWord(Store::AllocTuple(TAG0, Store::GenTSize(1)));

  Assert(nil != NULL);
  Store::SetArg(Store::WordToBPointer(nil), Store::GenTField(1), Store::IntToWord(0));
  return nil;
}

static int IsNil(word v) {
  Assert(v != NULL);
  return (Store::GetLabel(Store::WordToBPointer(v)) == TAG0);
}

static word Cons(word car, word cdr) {
  word cell = Store::BPointerToWord(Store::AllocTuple(TAG1, Store::GenTSize(2)));
  
  Store::SetArg(Store::WordToBPointer(cell), Store::GenTField(1), car);
  Store::SetArg(Store::WordToBPointer(cell), Store::GenTField(2), cdr);

  return cell;
}

static word Car(word cell) {
  return Store::GetArg(Store::WordToBPointer(cell), Store::GenTField(1));
}

static word Cdr(word cell) {
  return Store::GetArg(Store::WordToBPointer(cell), Store::GenTField(2));
}

static int Length(word list) {
  int l = 0;

  while (!IsNil(list)) {
    l++;
    list = Cdr(list);
  }
}

static word Append(word l1, word l2) {
  if (IsNil(l1)) {
    return l2;
  }
  else {
    return Cons(Car(l1), Append(Cdr(l1), l2));
  }
}

static void ShowList(word l) {
  cout << "[";
  while (!IsNil(l)) {
    word car = Car(l);
    word tmp = Cdr(l);

    if (Store::WordToBPointer(car) == INVALID_BPOINTER) {
      cout << Store::WordToInt(car);
    }
    else {
      ShowList(car);
    }

    if (!IsNil(tmp)) {
      cout << " ";
    }
    l = tmp;
  }
  cout << "]\n";
}

static word NthTail(word l, int n) {
  while ((n > 1) && (!IsNil(l))) {
    l = Cdr(l);
    n--;
  }
  
  return l;
}

static word MakeIntList(int n, int off) {
  word l = MakeNil();

  for (;n > 0; n--) {
    l = Cons(Store::IntToWord(n + off), l);
  }

  return l;
}

int main(int argc, char **argv) {
  word l1, l2, l3, l4;
  DynamicTuple *rs;

  Store::InitStore();
  rs = new DynamicTuple(100);

  l1 = MakeIntList(20000, 0);
  Store::MemStat();
  l2 = MakeIntList(10000, 20000);
  Store::MemStat();
  l3 = Append(l1, l2);
  Store::MemStat();

  l3 = NthTail(l3, 30000);
  Store::MemStat();
  ShowList(l3);

  rs->Add(l3);
  cout << "Performing GC...\n";
  Store::DoGC(rs, 0);
  cout << "Done\n";
  Store::MemStat();
  cout << "Re-ShowList\n";
  l3 = rs->GetArg(0);
  ShowList(l3);
  l4 = MakeIntList(1, 5);
  Store::ReplaceArg(Store::WordToBPointer(l3), Store::GenTField(1), l4);
  ShowList(l3);
  Store::MemStat();
  cout << "Performing GC again...\n";
  Store::DoGC(rs, 1);
  cout << "Done\n";
  ShowList(rs->GetArg(0));
  Store::MemStat();
  cout << "Now switching the 2nd generation flag...(gc'ing)\n";
  Store::DoGC(rs, 2);
  cout << "Done\n";
  ShowList(rs->GetArg(0));
  Store::MemStat();
  // Debug Hack
  exit(0);
}
