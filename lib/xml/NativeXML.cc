//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2003
// 
// See the file "LICENSE" for information on usage and
// redistribution of this file, and for a
//    DISCLAIMER OF ALL WARRANTIES.
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "Alice.hh"
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

class XMLRepHandler : public ConcreteRepresentationHandler {
  Transform *GetAbstractRepresentation(ConcreteRepresentation *) {
    return INVALID_POINTER;
  }
};

static XMLRepHandler *xmlRepHandler;

class XMLFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

static XMLFinalizationSet *xmlFinalizationSet;

static int xmlTypeMapping[22];

void XMLFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  word docptr = cr->Get(0);
  xmlDocPtr doc = (xmlDocPtr)Store::WordToUnmanagedPointer(docptr);
  xmlFreeDoc(doc);
}

#define DECLARE_UNMANAGED_POINTER(pointer, x)                       \
  void *pointer = NULL;                                             \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  else { pointer = Store::WordToUnmanagedPointer(x); }     

xmlDocPtr extract_docptr(word c) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(c);
  return (xmlDocPtr)Store::WordToUnmanagedPointer(cr->Get(0));
}
  
static word XMLErrorConstructor;

static word MakeXMLError(String *err) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(XMLErrorConstructor), 1);
  conVal->Init(0, err->ToWord());
  return conVal->ToWord();
}

#define CHECK_NULL(node)						\
  if ( (node) == NULL ) {						\
    word exn ## __LINE__ = MakeXMLError(String::New("node is NULL"));	\
    RAISE(exn ## __LINE__);						\
  }

DEFINE1(xml_parse) {
  DECLARE_STRING(filename, x0);
  xmlDocPtr doc;
  xmlNodePtr cur;
  doc = xmlParseFile(filename->ExportC());

  if (doc==NULL) {
    xmlErrorPtr err = xmlGetLastError();
    String *s;
    if (err==NULL)
      s = String::New("unknown error");
    else
      s = String::New(err->message);
    xmlResetLastError();
    word exn = MakeXMLError(s);
    RAISE(exn);
  }

  cur = xmlDocGetRootElement((xmlDocPtr) doc);

  ConcreteRepresentation *cr = ConcreteRepresentation::New(xmlRepHandler,1);
  cr->Init(0, Store::UnmanagedPointerToWord(doc));
  xmlFinalizationSet->Register(cr->ToWord());
  Tuple *t = Tuple::New(2);
  t->Init(0, cr->ToWord());
  t->Init(1, Store::UnmanagedPointerToWord(cur));

  RETURN(t->ToWord());
} END

DEFINE1(xml_parseString) {
  DECLARE_STRING(str, x0);
  xmlDocPtr doc;
  xmlNodePtr cur;
  char* strv = STATIC_CAST(char*, str->GetValue());
  doc = xmlParseMemory(strv, str->GetSize());

  if (doc==NULL) {
    xmlErrorPtr err = xmlGetLastError();
    String *s;
    if (err==NULL)
      s = String::New("unknown error");
    else
      s = String::New(err->message);
    xmlResetLastError();
    word exn = MakeXMLError(s);
    RAISE(exn);
  }

  cur = xmlDocGetRootElement((xmlDocPtr) doc);

  ConcreteRepresentation *cr = ConcreteRepresentation::New(xmlRepHandler,1);
  cr->Init(0, Store::UnmanagedPointerToWord(doc));
  xmlFinalizationSet->Register(cr->ToWord());
  Tuple *t = Tuple::New(2);
  t->Init(0, cr->ToWord());
  t->Init(1, Store::UnmanagedPointerToWord(cur));
  RETURN(t->ToWord());
} END

DEFINE1(xml_isNull) {
  DECLARE_TUPLE(t, x0);
  xmlDocPtr doc = extract_docptr(t->Sel(0));
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  RETURN_BOOL(node==NULL || doc==NULL);
} END

DEFINE1(xml_children) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);
  Tuple *c = Tuple::New(2);
  c->Init(0, t->Sel(0));
  c->Init(1, Store::UnmanagedPointerToWord(node->children));
  RETURN(c->ToWord());
} END

DEFINE1(xml_parent) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);
  Tuple *c = Tuple::New(2);
  c->Init(0, t->Sel(0));

  if (node == xmlDocGetRootElement(extract_docptr(t->Sel(0)))) {
    c->Init(1, Store::UnmanagedPointerToWord(NULL));
  } else {
    c->Init(1, Store::UnmanagedPointerToWord(node->parent));
  }

  RETURN(c->ToWord());
} END

DEFINE1(xml_next) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);
  Tuple *c = Tuple::New(2);
  c->Init(0, t->Sel(0));
  c->Init(1, Store::UnmanagedPointerToWord(node->next));
  RETURN(c->ToWord());
} END

DEFINE1(xml_prev) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);
  Tuple *c = Tuple::New(2);
  c->Init(0, t->Sel(0));
  c->Init(1, Store::UnmanagedPointerToWord(node->prev));
  RETURN(c->ToWord());
} END

DEFINE1(xml_properties) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);
  Tuple *c = Tuple::New(2);
  c->Init(0, t->Sel(0));
  c->Init(1, Store::UnmanagedPointerToWord(node->properties));
  RETURN(c->ToWord());
} END

DEFINE1(xml_name) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);

  if (node->name) {
    int len = strlen((const char*)node->name);
    int inlen = len;
    
    Chunk *buffer = Store::AllocChunk(len);
    UTF8Toisolat1 ((unsigned char*) buffer->GetBase(), &len,
		   node->name, &inlen);
    
    String *retName = String::New((char*) buffer->GetBase(),len);
    RETURN(retName->ToWord());
  } else {
    RETURN(String::New("")->ToWord());
  }
} END

DEFINE2(xml_nodeListGetString) {
  DECLARE_TUPLE(t, x0);
  xmlDocPtr doc = extract_docptr(t->Sel(0));
  xmlNodePtr node = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(node);

  DECLARE_INT(inLine, x1);

  xmlChar *key;
  key = xmlNodeListGetString(doc,
			     node,
			     inLine);

  if (key==NULL) {
    RETURN(Store::IntToWord(0)); // NONE
  }

  int len = strlen((const char*) key);
  int inlen = len;
  Chunk *buffer = Store::AllocChunk(len);
  UTF8Toisolat1 ((unsigned char*) buffer->GetBase(), &len,
		 key, &inlen);
  xmlFree(key);
  String *retName = String::New((char*) buffer->GetBase(),len);
  TagVal *tv = TagVal::New(1,1);
  tv->Init(0, retName->ToWord());
  RETURN(tv->ToWord()); // SOME retName
} END

DEFINE1(xml_getType) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr cur = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(cur);
  RETURN_INT(xmlTypeMapping[cur->type]);
} END

DEFINE2(xml_getProp) {
  DECLARE_TUPLE(t, x0);
  xmlNodePtr cur = (xmlNodePtr)Store::WordToUnmanagedPointer(t->Sel(1));
  CHECK_NULL(cur);
  DECLARE_STRING(name, x1);

  int len2 = name->GetSize();
  int outlen = len2*2;
  Chunk *buffer2 = Store::AllocChunk(outlen);
  
  isolat1ToUTF8((unsigned char*) buffer2->GetBase(), &outlen,
		(unsigned char*) name->ExportC(), &len2);
  buffer2->GetBase()[outlen] = 0;

  xmlChar *prop;

  prop = xmlGetProp((xmlNodePtr) cur,
		    (xmlChar *) buffer2->GetBase());

  if (prop==NULL) {
    RETURN(Store::IntToWord(0)); // NONE
  }

  int len = strlen((const char*) prop);
  int inlen = len;

  Chunk *buffer = Store::AllocChunk(len);
  UTF8Toisolat1 ((unsigned char*) buffer->GetBase(), &len,
		 prop, &inlen);
  xmlFree(prop);
  String *retName = String::New((char*) buffer->GetBase(),len);
  TagVal *tv = TagVal::New(1,1);
  tv->Init(0, retName->ToWord());
  RETURN(tv->ToWord()); // SOME retName

} END

DEFINE3(xml_error) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(XMLErrorConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

static void IgnoreXMLError(void *ctx, const char* msg,...) {
  return;
}

word InitComponent() {

  xmlSetGenericErrorFunc(NULL, IgnoreXMLError);

  Record *record = Record::New(14);
  xmlFinalizationSet = new XMLFinalizationSet();
  xmlRepHandler = new XMLRepHandler();

  XMLErrorConstructor =
    UniqueConstructor::New("XMLError", "NativeXML.XMLError")->ToWord();
  RootSet::Add(XMLErrorConstructor);

  xmlTypeMapping[1] = 10;
  xmlTypeMapping[2] = 1;
  xmlTypeMapping[3] = 18;
  xmlTypeMapping[4] = 2;
  xmlTypeMapping[5] = 13;
  xmlTypeMapping[6] = 12;
  xmlTypeMapping[7] = 17;
  xmlTypeMapping[8] = 3;
  xmlTypeMapping[9] = 6;
  xmlTypeMapping[10] = 7;
  xmlTypeMapping[11] = 5;
  xmlTypeMapping[12] = 16;
  xmlTypeMapping[13] = 14;
  xmlTypeMapping[14] = 8;
  xmlTypeMapping[15] = 9;
  xmlTypeMapping[16] = 0;
  xmlTypeMapping[17] = 11;
  xmlTypeMapping[18] = 15;
  xmlTypeMapping[19] = 20;
  xmlTypeMapping[20] = 19;
  xmlTypeMapping[21] = 4;

  INIT_STRUCTURE(record, "NativeXML", "parse",
		 xml_parse, 1);
  INIT_STRUCTURE(record, "NativeXML", "parseString",
		 xml_parseString, 1);
  INIT_STRUCTURE(record, "NativeXML", "isNull",
		 xml_isNull, 1);
  INIT_STRUCTURE(record, "NativeXML", "children",
		 xml_children, 1);
  INIT_STRUCTURE(record, "NativeXML", "parent",
		 xml_parent, 1);
  INIT_STRUCTURE(record, "NativeXML", "next",
		 xml_next, 1);
  INIT_STRUCTURE(record, "NativeXML", "prev",
		 xml_prev, 1);
  INIT_STRUCTURE(record, "NativeXML", "properties",
		 xml_properties, 1);
  INIT_STRUCTURE(record, "NativeXML", "name",
		 xml_name, 1);
  INIT_STRUCTURE(record, "NativeXML", "getType",
		 xml_getType, 1);
  INIT_STRUCTURE(record, "NativeXML", "getProp",
		 xml_getProp, 2);
  INIT_STRUCTURE(record, "NativeXML", "nodeListGetString",
		 xml_nodeListGetString, 2);
  record->Init("'XMLError", XMLErrorConstructor);
  INIT_STRUCTURE(record, "NativeXML", "XMLError",
		 xml_error, 1);
  RETURN_STRUCTURE("NativeXML$", record);
}
