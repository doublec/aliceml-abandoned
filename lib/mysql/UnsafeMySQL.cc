//
// Author:
//   Simon Pinkel <pinkel@ps.uni-sb.de>
// 
// Copyright:
//   Simon Pinkel, 2004
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "Alice.hh"

#include <mysql/mysql.h>
#include <stdio.h>

#define DECLARE_MYSQL(mysql, x)                       \
  MYSQL *mysql;                                                  \
  int is_closed = 0;                                             \
  ConcreteRepresentation *mcr;                                   \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { mcr = ConcreteRepresentation::FromWordDirect(x); \
    mysql = (MYSQL *) Store::WordToUnmanagedPointer(mcr->Get(0)); \
    is_closed = Store::WordToInt(mcr->Get(1)); }

#define DECLARE_MYSQL_RES(result, x)                       \
  MYSQL_RES *result;                                                  \
  int is_free = 0;                                                    \
  ConcreteRepresentation *rcr;                                        \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { rcr = ConcreteRepresentation::FromWordDirect(x); \
    result = (MYSQL_RES *) Store::WordToUnmanagedPointer(rcr->Get(0)); \
    is_free = Store::WordToInt(rcr->Get(1)); }


class MySQLHandler : public ConcreteRepresentationHandler {
  Transform *MySQLHandler::GetAbstractRepresentation(ConcreteRepresentation *)
  {
    return INVALID_POINTER;
  }
};

static MySQLHandler *mysqlHandler;

class MySQLResultHandler : public ConcreteRepresentationHandler {
  Transform
  *MySQLResultHandler::GetAbstractRepresentation(ConcreteRepresentation *)
  {
    return INVALID_POINTER;
  }
};

static MySQLResultHandler *mysqlResultHandler;


class MySQLFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

// Garbage Collection
void MySQLFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  word ptr = cr->Get(0);
  // ...
}

static MySQLFinalizationSet *mysqlFinalizationSet;

static word MySQLErrorConstructor;

static word MakeMySQLError(String *err) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(MySQLErrorConstructor), 1);

  conVal->Init(0, err->ToWord());
  return conVal->ToWord();
}

DEFINE1(my_sql_error) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(MySQLErrorConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

DEFINE4(_mysql_connect) {
  DECLARE_STRING(host,x0);
  DECLARE_STRING(user,x1);
  DECLARE_STRING(password,x2);
  DECLARE_STRING(db,x3);

  MYSQL *mysql = (MYSQL*)malloc(sizeof(MYSQL));

  mysql = mysql_init(mysql);
  if (!mysql_real_connect(mysql,host->ExportC(),
			  user->ExportC(),password->ExportC(),
			  db->ExportC(),0,NULL,0)) {
    RAISE(MakeMySQLError(String::New(mysql_error(mysql))));
  }

  // to make MYSQL-resources unpickable:
  ConcreteRepresentation *cr = ConcreteRepresentation::New(mysqlHandler, 2);
  cr->Init(0, Store::UnmanagedPointerToWord(mysql));
  cr->Init(1, Store::IntToWord(0));
  // garbage collection
  mysqlFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE3(_mysql_query) {
  DECLARE_MYSQL(mysql,x0);
  DECLARE_STRING(query,x1);
  DECLARE_BOOL(store_result,x2);

  MYSQL_RES* result;

  if (is_closed) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_query: Connection lost or closed earlier")));
  }

  if (mysql_real_query(mysql,query->ExportC(),query->GetSize())) {
    RAISE(MakeMySQLError(String::New(mysql_error(mysql))));
  }

  // fprintf(stderr,"Query issued...\n"); // DEBUG

  if (store_result) result = mysql_store_result(mysql);
  else result = mysql_use_result(mysql);

  // fprintf(stderr,"Building Concrete Representation...\n"); // DEBUG

  if (result == NULL) {
    RETURN(Store::IntToWord(0)); // NONE
  }

  // to make MYSQL-resources unpickable
  ConcreteRepresentation *cr = 
    ConcreteRepresentation::New(mysqlResultHandler, 2);
  cr->Init(0, Store::UnmanagedPointerToWord(result));
  cr->Init(1, Store::IntToWord(0));
  // garbage collection

  // fprintf(stderr,"Collecting Garbage...\n"); // DEBUG

  mysqlFinalizationSet->Register(cr->ToWord());

  // fprintf(stderr,"Building TagVal...\n"); // DEBUG

  TagVal *tv = TagVal::New(1,1);
  tv->Init(0,cr->ToWord());

  // fprintf(stderr,"Returning Resource\n"); // DEBUG

  RETURN(tv->ToWord()); // SOME result
} END

DEFINE2(_mysql_fetch_row) {
  DECLARE_MYSQL(mysql,x0);
  DECLARE_MYSQL_RES(result,x1);

  if (is_closed) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_fetch_row: Connection lost or closed earlier")));
  }
  if (is_free) {
      RAISE(MakeMySQLError(String::New("Internal Error, mysql_fetch_row: Result was freed before")));
  }

  MYSQL_ROW row = mysql_fetch_row(result);

  if (row == NULL) {
    if (mysql_errno(mysql)) {
      RAISE(MakeMySQLError(String::New(mysql_error(mysql))));
    } else {
      RETURN_INT(0); // datatype res = DONE | ...
    }
  } else {
    unsigned int num_fields = mysql_num_fields(result);
    Vector *values = Vector::New(num_fields);
    for (unsigned int i=0; i<num_fields; i++) {
      if (row[i] == NULL) {
	values->Init(i,Store::IntToWord(0)); // NONE
      } else {
	TagVal *tv = TagVal::New(1,1);
	
	// fprintf(stderr,"fetched value: %s\n",row[i]); // DEBUG

	tv->Init(0,String::New(row[i])->ToWord());
	values->Init(i,tv->ToWord()); // SOME v
      }
    }
    TagVal *ret = TagVal::New(1, 1); // datatype res = ... | ROW of ...
    ret->Init(0,values->ToWord());
    RETURN(ret->ToWord());
  }
} END

DEFINE1(_mysql_fetch_fields) {
  DECLARE_MYSQL_RES(result,x0);

  if (is_free) {
      RAISE(MakeMySQLError(String::New("Internal Error, mysql_fetch_fields: Result was freed before")));
  }

  // fprintf(stderr,"Result is not free, proceeding...\n"); // DEBUG

  // fprintf(stderr,"calling mysql_num_fields...\n"); // DEBUG
  
  unsigned int num_fields = mysql_num_fields(result);
  Vector *fields = Vector::New(num_fields);

  // fprintf(stderr,"fetching fields...\n"); // DEBUG

  MYSQL_FIELD* _fields = mysql_fetch_fields(result);

  for (unsigned int i=0; i<num_fields; i++) {
    fields->Init(i,String::New(_fields[i].name)->ToWord());
  }

  // fprintf(stderr,"returning fields...\n"); // DEBUG

  RETURN(fields->ToWord());
} END

DEFINE1(_mysql_free_result) {
  DECLARE_MYSQL_RES(result,x0);
  if (is_free) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_free_result: Result resource is already free")));
  } else {
    rcr->Replace(1,Store::IntToWord(1));
    mysql_free_result(result);
  }
  RETURN_UNIT;
} END

DEFINE1(_mysql_close) {
  DECLARE_MYSQL(mysql,x0);
  if (is_closed) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_close: Connection is already closed")));
  } else {
    mcr->Replace(1,Store::IntToWord(1));
    mysql_close(mysql);
    free(mysql);
  }
  RETURN_UNIT;
} END

DEFINE1(_mysql_character_set_name) {
  DECLARE_MYSQL(mysql,x0);

  if (is_closed) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_character_set_name: Connection lost or closed earlier")));
  }

  RETURN(String::New(mysql_character_set_name(mysql))->ToWord());
} END

DEFINE1(_mysql_ping) {
  DECLARE_MYSQL(mysql,x0);

  if (is_closed) {
    RAISE(MakeMySQLError(String::New("Internal Error, mysql_ping: Connection lost or closed earlier")));
  }

  if (mysql_ping(mysql)) {
    if  (mysql_errno(mysql)) {
     RAISE(MakeMySQLError(String::New(mysql_error(mysql))));
    } else {
     RAISE(MakeMySQLError(String::New("Internal Error, mysql_ping: Unknown")));
    }
  }
  RETURN_UNIT;
} END


word InitComponent() {
  mysqlFinalizationSet = new MySQLFinalizationSet();
  mysqlHandler = new MySQLHandler();
  mysqlResultHandler = new MySQLResultHandler();

  MySQLErrorConstructor =
    UniqueConstructor::New("MySQLError",
			   "UnsafeMySQL.MySQLError")->ToWord();
  RootSet::Add(MySQLErrorConstructor);

  Record *record = Record::New(10);

  record->Init("'MySQLError", MySQLErrorConstructor);
  INIT_STRUCTURE(record,"UnsafeMySQL","MySQLError",
		 my_sql_error, 1);

  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_connect",_mysql_connect,4);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_query",_mysql_query,3);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_fetch_row",_mysql_fetch_row,2);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_fetch_fields",
		 _mysql_fetch_fields,1);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_free_result",
		 _mysql_free_result,1);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_close",_mysql_close,1);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_character_set_name",
		 _mysql_character_set_name,1);
  INIT_STRUCTURE(record,"UnsafeMySQL","mysql_ping",_mysql_ping,1);

  RETURN_STRUCTURE("UnsafeMySQL$",record);
}
