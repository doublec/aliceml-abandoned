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
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

#define DECLARE_UNMANAGED_POINTER(pointer, x)                       \
  void *pointer = NULL;                                             \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  else { pointer = Store::WordToUnmanagedPointer(x); }     

DEFINE1(xml_parseFile) {
  DECLARE_STRING(filename, x0);
  xmlDocPtr doc;
  doc = xmlParseFile(filename->ExportC());
  RETURN(Store::UnmanagedPointerToWord(doc));
} END

DEFINE1(xml_freeDoc) {
  DECLARE_UNMANAGED_POINTER(doc, x0);
  xmlFreeDoc((xmlDocPtr) doc);
  RETURN_UNIT;
} END

DEFINE1(xml_isNull) {
  DECLARE_UNMANAGED_POINTER(ptr, x0);
  RETURN_BOOL(ptr==NULL);
} END

DEFINE1(xml_docGetRootElement) {
  DECLARE_UNMANAGED_POINTER(doc, x0);
  xmlNodePtr cur = xmlDocGetRootElement((xmlDocPtr) doc);
  RETURN(Store::UnmanagedPointerToWord(cur));
} END

DEFINE1(xml_children) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  RETURN(Store::UnmanagedPointerToWord(((xmlNodePtr) cur)->children));
} END

DEFINE1(xml_parent) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  RETURN(Store::UnmanagedPointerToWord(((xmlNodePtr) cur)->parent));
} END

DEFINE1(xml_next) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  RETURN(Store::UnmanagedPointerToWord(((xmlNodePtr) cur)->next));
} END

DEFINE1(xml_prev) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  RETURN(Store::UnmanagedPointerToWord(((xmlNodePtr) cur)->prev));
} END

DEFINE1(xml_properties) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  RETURN(Store::UnmanagedPointerToWord(((xmlNodePtr) cur)->properties));
} END

DEFINE1(xml_name) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  xmlNodePtr node = (xmlNodePtr) cur;
  
  int len = strlen((const char*)node->name);
  int inlen = len;

  Chunk *buffer = Store::AllocChunk(len);
  UTF8Toisolat1 ((unsigned char*) buffer->GetBase(), &len,
		 node->name, &inlen);

  String *retName = String::New((char*) buffer->GetBase(),len);
  RETURN(retName->ToWord());
} END

DEFINE3(xml_nodeListGetString) {
  DECLARE_UNMANAGED_POINTER(doc, x0);
  DECLARE_UNMANAGED_POINTER(cur, x1);
  DECLARE_INT(inLine, x2);

  xmlChar *key;
  key = xmlNodeListGetString((xmlDocPtr) doc,
			     (xmlNodePtr) cur,
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

DEFINE2(xml_getProp) {
  DECLARE_UNMANAGED_POINTER(cur, x0);
  DECLARE_STRING(name, x1);

  int len2 = name->GetSize();
  int outlen = len2*2;
  Chunk *buffer2 = Store::AllocChunk(outlen);
  
  isolat1ToUTF8((unsigned char*) buffer2->GetBase(), &outlen,
		(unsigned char*) name->ExportC(), &len2);
  buffer2->GetBase()[outlen] = 0;
  //  String *utf8name = String::New((char*) buffer2->GetBase(),outlen);
  //  fprintf(stderr, "buffer: %s\n", utf8name->ExportC());

  xmlChar *prop;

  prop = xmlGetProp((xmlNodePtr) cur,
		    //		    (xmlChar *) utf8name->ExportC());
		    (xmlChar *) buffer2->GetBase());
  //		    (unsigned char*) name->ExportC());

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

word InitComponent() {
  Record *record = Record::New(12);
  INIT_STRUCTURE(record, "NativeAliceXML", "parseFile",
		 xml_parseFile, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "freeDoc",
		 xml_freeDoc, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "isNull",
		 xml_isNull, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "docGetRootElement",
		 xml_docGetRootElement, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "children",
		 xml_children, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "parent",
		 xml_parent, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "next",
		 xml_next, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "prev",
		 xml_prev, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "properties",
		 xml_properties, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "name",
		 xml_name, 1);
  INIT_STRUCTURE(record, "NativeAliceXML", "getProp",
		 xml_getProp, 2);
  INIT_STRUCTURE(record, "NativeAliceXML", "nodeListGetString",
		 xml_nodeListGetString, 3);
  RETURN_STRUCTURE("NativeAliceXML$", record);
}
