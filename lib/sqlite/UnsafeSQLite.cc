//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2003
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "Alice.hh"

#include <sqlite.h>

#define DECLARE_SQLITEDB(db, x)                       \
  sqlite *db;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    db = (sqlite *) Store::WordToUnmanagedPointer(cr->Get(0)); }

#define DECLARE_SQLITEVM(vm, x)                       \
  sqlite_vm *vm;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    vm = (sqlite_vm *) Store::WordToUnmanagedPointer(cr->Get(0)); }


class SQLiteHandler : public ConcreteRepresentationHandler {
  Transform *SQLiteHandler::GetAbstractRepresentation(ConcreteRepresentation *)
  {
    return INVALID_POINTER;
  }
};

static SQLiteHandler *sqliteHandler;

class SQLiteFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

static SQLiteFinalizationSet *sqliteFinalizationSet;

void SQLiteFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  word ptr = cr->Get(0);
}

static word SQLErrorConstructor;

static word MakeSQLError(String *err) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(SQLErrorConstructor), 1);
  conVal->Init(0, err->ToWord());
  return conVal->ToWord();
}

DEFINE1(sql_open) {
  DECLARE_STRING(filename, x0);
  char *errmsg;

  sqlite *db = sqlite_open(filename->ExportC(), 0, &errmsg);
  if (db==NULL) {
    word exn = MakeSQLError(String::New(errmsg));
    free(errmsg);
    RAISE(exn);
  }

  ConcreteRepresentation *cr = ConcreteRepresentation::New(sqliteHandler, 2);
  cr->Init(0, Store::UnmanagedPointerToWord(db));
  cr->Init(1, Store::IntToWord(0));
  sqliteFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(sql_close) {
  DECLARE_SQLITEDB(db, x0);
  sqlite_close(db);
  RETURN_UNIT;
} END

DEFINE2(sql_compile) {
  DECLARE_SQLITEDB(db, x0);
  DECLARE_STRING(sqlstatement, x1);

  sqlite_vm *ppVm;
  const char *pzTail;
  char *errmsg;
  int res = sqlite_compile(db,
			   sqlstatement->ExportC(),
			   &pzTail,
			   &ppVm,
			   &errmsg);
  if (res!=SQLITE_OK) {
    word exn = MakeSQLError(String::New(errmsg));
    free(errmsg);
    RAISE(exn);    
  }

  ConcreteRepresentation *cr = ConcreteRepresentation::New(sqliteHandler, 2);
  cr->Init(0, Store::UnmanagedPointerToWord(ppVm));
  cr->Init(1, Store::IntToWord(1));
  sqliteFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(sql_step) {
  DECLARE_SQLITEVM(vm, x0);
  int cols;
  const char **pazValue;
  const char **pazColName;
  int retcode = sqlite_step(vm,
			    &cols,
			    &pazValue,
			    &pazColName);

  switch(retcode) {
  case SQLITE_BUSY:
    RETURN_INT(0); // datatype res = BUSY | ...
    break;
  case SQLITE_ROW:
    {
      Vector *values = Vector::New(cols);
      Vector *columnNames = Vector::New(cols);
      for (u_int i=0; i<cols; i++) {
	if (pazValue[i]==0) {
	  values->Init(i, Store::IntToWord(0));
	} else {
	  TagVal *tv = TagVal::New(1,1);
	  tv->Init(0, String::New(pazValue[i])->ToWord());
	  values->Init(i, tv->ToWord());
	}
	columnNames->Init(i, String::New(pazColName[i])->ToWord());
      }
      TagVal *ret = TagVal::New(2, 2); // datatype res = ... | ROW of ...
      ret->Init(0, values->ToWord());
      ret->Init(1, columnNames->ToWord());
      RETURN(ret->ToWord());
    }
    break;
  case SQLITE_DONE:
    RETURN_INT(1); // datatype res = ... | DONE | ...
    break;
  case SQLITE_ERROR:
  case SQLITE_MISUSE:
    RAISE(MakeSQLError(String::New("sqlite step error")));
    break;
  }


} END

DEFINE1(sql_finalize) {
  DECLARE_SQLITEVM(vm, x0);
  sqlite_finalize(vm, 0);
  RETURN_UNIT;
} END

DEFINE3(sql_error) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(SQLErrorConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

word InitComponent() {
  sqliteFinalizationSet = new SQLiteFinalizationSet();
  sqliteHandler = new SQLiteHandler();

  SQLErrorConstructor =
    UniqueConstructor::New("SQLError", "UnsafeSQLite.SQLError")->ToWord();
  RootSet::Add(SQLErrorConstructor);

  Record *record = Record::New(7);

  INIT_STRUCTURE(record, "UnsafeSQLite", "opendb",
		 sql_open, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "closedb",
		 sql_close, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "compile",
		 sql_compile, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "step",
		 sql_step, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "finalize",
		 sql_finalize, 1);

  record->Init("'SQLError", SQLErrorConstructor);
  INIT_STRUCTURE(record, "UnsafeSQLite", "SQLError",
		 sql_error, 1);
  
  RETURN_STRUCTURE("UnsafeSQLite$", record);
}
