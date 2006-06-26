//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2003
// 
//  See the file "LICENSE" for information on usage and
//  redistribution of this file, and for a
//     DISCLAIMER OF ALL WARRANTIES.
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "Alice.hh"

#include <sqlite3.h>

#define DECLARE_SQLITEDB(db, x)                       \
  sqlite3 *db;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x); \
    db = (sqlite3 *) Store::WordToUnmanagedPointer(cr->Get(0)); }

#define DECLARE_SQLITESTMT(stmt, x)                       \
  sqlite3_stmt *stmt;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x); \
    stmt = (sqlite3_stmt *) Store::WordToUnmanagedPointer(cr->Get(0)); }


class SQLiteHandler : public ConcreteRepresentationHandler {
  Transform *GetAbstractRepresentation(ConcreteRepresentation *) {
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
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(value);
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
  sqlite3 *db;
  int ret = sqlite3_open(filename->ExportC(), &db);
  if (ret!=SQLITE_OK) {
    const char *errmsg = sqlite3_errmsg(db);
    word exn = MakeSQLError(String::New(errmsg));
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
  int ret = sqlite3_close(db);
  if (ret!=SQLITE_OK) {
    const char *errmsg = sqlite3_errmsg(db);
    fprintf(stderr, "not ok: %s\n", errmsg);
    word exn = MakeSQLError(String::New(errmsg));
    RAISE(exn);
  }
  RETURN_UNIT;
} END

DEFINE2(sql_prepare) {
  DECLARE_SQLITEDB(db, x0);
  DECLARE_STRING(sqlstatement, x1);

  sqlite3_stmt *stmt;
  const char *pzTail;
  int res = sqlite3_prepare(db,
                            sqlstatement->ExportC(),
                            sqlstatement->GetSize(),
                            &stmt,
                            &pzTail);
  if (res!=SQLITE_OK) {
    const char *errmsg = sqlite3_errmsg(db);
    word exn = MakeSQLError(String::New(errmsg));
    RAISE(exn);    
  }

  ConcreteRepresentation *cr = ConcreteRepresentation::New(sqliteHandler, 2);
  cr->Init(0, Store::UnmanagedPointerToWord(stmt));
  cr->Init(1, Store::IntToWord(1));
  sqliteFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(sql_step) {
  DECLARE_SQLITESTMT(stmt, x0);
  int ret = sqlite3_step(stmt);

  switch(ret) {
  case SQLITE_BUSY:
    RETURN_INT(0); // datatype res = BUSY | ...
    break;
  case SQLITE_ROW:
    {
      RETURN_INT(2);
      break;
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
  DECLARE_SQLITESTMT(stmt, x0);
  int res = sqlite3_finalize(stmt);
  if (res!=SQLITE_OK) {
    word exn = MakeSQLError(String::New("sqlite finalizing error"));
    RAISE(exn);    
  }
  RETURN_UNIT;
} END

DEFINE1(sql_reset) {
  DECLARE_SQLITESTMT(stmt, x0);
  int res = sqlite3_reset(stmt);
  if (res!=SQLITE_OK) {
    word exn = MakeSQLError(String::New("sqlite reset error"));
    RAISE(exn);    
  }
  RETURN_UNIT;
} END

DEFINE3(sql_error) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(SQLErrorConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

/* Functions for accessing columns in a result row */

DEFINE2(sql_c_blob) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  const char* blob = (const char*) sqlite3_column_blob(stmt, col);
  RETURN(String::New(blob)->ToWord());
} END

DEFINE2(sql_c_int) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  int i = sqlite3_column_int(stmt, col);
  RETURN_INT(i);
} END

DEFINE2(sql_c_real) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  double i = sqlite3_column_double(stmt, col);
  RETURN(Real::New(i)->ToWord());
} END

DEFINE2(sql_c_text) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  const char* text = (char *)sqlite3_column_text(stmt, col);
  RETURN(String::New(text)->ToWord());
} END

DEFINE2(sql_c_type) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  switch(sqlite3_column_type(stmt, col)) {
  case SQLITE_INTEGER : RETURN_INT(1);
  case SQLITE_FLOAT : RETURN_INT(3);
  case SQLITE_TEXT : RETURN_INT(4);
  case SQLITE_BLOB : RETURN_INT(0);
  case SQLITE_NULL : RETURN_INT(2);
  default:
    word exn = MakeSQLError(String::New("unknown type"));
    RAISE(exn);
  }
} END

DEFINE2(sql_c_name) {
  DECLARE_SQLITESTMT(stmt, x0);
  DECLARE_INT(col, x1);

  const char* text = sqlite3_column_name(stmt, col);
  RETURN(String::New(text)->ToWord());
} END

DEFINE1(sql_c_count) {
  DECLARE_SQLITESTMT(stmt, x0);

  int cols = sqlite3_column_count(stmt);
  RETURN_INT(cols);
} END

word InitComponent() {
  sqliteFinalizationSet = new SQLiteFinalizationSet();
  sqliteHandler = new SQLiteHandler();

  SQLErrorConstructor =
    UniqueConstructor::New("SQLError", "UnsafeSQLite.SQLError")->ToWord();
  RootSet::Add(SQLErrorConstructor);

  Record *record = Record::New(15);

  INIT_STRUCTURE(record, "UnsafeSQLite", "opendb",
		 sql_open, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "closedb",
		 sql_close, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "prepare",
		 sql_prepare, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "step",
		 sql_step, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "finalize",
		 sql_finalize, 1);
  INIT_STRUCTURE(record, "UnsafeSQLite", "reset",
		 sql_reset, 1);

  INIT_STRUCTURE(record, "UnsafeSQLite", "c_blob",
		 sql_c_blob, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_int",
		 sql_c_int, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_real",
		 sql_c_real, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_text",
		 sql_c_text, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_type",
		 sql_c_type, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_name",
		 sql_c_name, 2);
  INIT_STRUCTURE(record, "UnsafeSQLite", "c_count",
		 sql_c_count, 1);

  record->Init("'SQLError", SQLErrorConstructor);
  INIT_STRUCTURE(record, "UnsafeSQLite", "SQLError",
		 sql_error, 1);
  
  RETURN_STRUCTURE("UnsafeSQLite$", record);
}
