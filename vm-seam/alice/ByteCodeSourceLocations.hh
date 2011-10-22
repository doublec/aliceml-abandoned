//
// Author:
//   Gareth Smith <garethdanielsmith@gmail.com>
//


#ifndef __ALICE_BYTE_CODE_SOURCE_LOCATIONS_HH__
#define __ALICE_BYTE_CODE_SOURCE_LOCATIONS_HH__

#include "alice/Base.hh"
#include "alice/Data.hh"
#include "alice/AbstractCodeInterpreter.hh"

#include <iomanip>


/**
 * This is used by the byte code jitter to construct source location
 * data, which are values of the Alice type:
 * 
 * datatype source_location =
 *   | Coord of int * coord * source_location
 *   | Begin_Inline_Function of function_coord * source_location
 *   | End_Inline_Function of source_location
 *   | End
 */
class AliceDll ByteCodeSourceLocations {
private:
  word start, current;
  
  
  void Append(word w) {
    TagVal *src = TagVal::FromWord(current);
    
    if (src == INVALID_POINTER) { // current == Empty()
      current = start = w;
    }
    else {
      src->Init(ContinuationPos(src), w);
      current = w;
    }
  }
  
  
  static u_int ContinuationPos(TagVal *src) {
    switch(src->GetTag()) {
      case Coord: return 2;
      case Begin_Inline_Function: return 1;
      case End_Inline_Function: return 0;
      default:
        Error("invalid source_location tag");
    }
  }
  
  
  static TagVal *NextCoord(TagVal *src) {
    while ((src = TagVal::FromWord(src->Sel(ContinuationPos(src)))) != INVALID_POINTER) {
      if (src->GetTag() == Coord) {
        return src;
      }
    }
    return INVALID_POINTER;
  }
  
  
  static void PrintFrameInternal(Stack *funs, std::ostream& out) {
    u_int i = 0;
    while (!funs->IsEmpty()) {
      word posCoord = funs->Pop();
      word funCoord = funs->Pop();
      
      if (i > 0) {
        out << "- ";
      }
      AbstractCodeInterpreter::DumpAliceFrame(funCoord, false, posCoord, !funs->IsEmpty(), out);
      i++;
    }
  }


  static void PrintN(std::ostream& out, u_int n, const char* str) {
    for (u_int i=0; i<n; i++) {
      out << str;
    }
  }


public:
  
  enum {
    Begin_Inline_Function,
    Coord, 
    End,
    End_Inline_Function
  };
  
  
  /**
   * Print the entirity of sourceLocations to the specified
   * ostream in a human readable format.
   * 
   * funCoord is the function_coord of the AbstractCode Function that
   * the bytecode was generated from.
   */
  static void Print(word funCoord, word sourceLocations, std::ostream& out = std::cerr) {

    Tuple *fc = Tuple::FromWord(funCoord);
    String *file = String::FromWord(fc->Sel(0));
    String *name = String::FromWord(fc->Sel(1));
    s_int line   = Store::WordToInt(fc->Sel(2));
    s_int col    = Store::WordToInt(fc->Sel(3));
    
    out << "Byte Code Source Locations: " << name << " - "
      << file << " " << line << "." << col << std::endl;
    
    u_int offset = 0;
    TagVal *src = TagVal::FromWord(sourceLocations);
    while (src != INVALID_POINTER) {
      switch (src->GetTag()) {
        
        case Coord: {
          s_int pc = Store::WordToInt(src->Sel(0));
          Tuple *coord = Tuple::FromWord(src->Sel(1));
          s_int line   = Store::WordToInt(coord->Sel(0));
          s_int col    = Store::WordToInt(coord->Sel(1));
          
	  // show PC offset in bytes to match bytecode disassembler
          out << "0x" << std::hex << std::setfill('0') << std::setw(4)
	    << (pc * sizeof(word)) << std::dec << ": ";
	  PrintN(out, offset, "----");
          out << (offset == 0 ? "" : " ") << line << "." << col << std::endl;
          
          src = TagVal::FromWord(src->Sel(2));
          break;
        }
        
        case Begin_Inline_Function: {
          Tuple *fc = Tuple::FromWord(src->Sel(0));
          String *file = String::FromWord(fc->Sel(0));
          String *name = String::FromWord(fc->Sel(1));
          s_int line   = Store::WordToInt(fc->Sel(2));
          s_int col    = Store::WordToInt(fc->Sel(3));
          
          PrintN(out, 8, " ");
	  PrintN(out, offset, "----");
	  out << (offset == 0 ? "" : " ") << "start inlined function: "
	    << name << " - " << file << " " <<  line << "." << col << std::endl;
          offset++;
          src = TagVal::FromWord(src->Sel(1));
          break;
        }
        
        case End_Inline_Function: {
          offset--;
	  
	  PrintN(out, 8, " ");
	  PrintN(out, offset, "----");
	  out << (offset == 0 ? "" : " ") << "end" << std::endl;
          src = TagVal::FromWord(src->Sel(0));
          break;
        }
        
        default: {
          Error("invalid source_location tag");
        }
      }
    }
  }
  
  
  /**
   * Prints a section of stack trace that describes the specified function
   * executing code at the specified pc.
   */
  static void PrintFrame(word funCoord, word sourceLocations, u_int pc, std::ostream& out = std::cerr) {
    
    // stack of pairs of (function_coord, coord-within-fun OR Store::IntToWord(0))
    Stack *funs = Stack::New(8);
    word coord = Store::IntToWord(0);
    
    TagVal *src = TagVal::FromWord(sourceLocations);
    while (src != INVALID_POINTER) {
      switch (src->GetTag()) {
        
        case Coord: {
	  coord = src->Sel(1);
	  u_int curPc = Store::WordToInt(src->Sel(0));
	  TagVal *next = NextCoord(src);
	  
	  if (next == INVALID_POINTER) {
	    src = INVALID_POINTER;
	  }
	  else {
	    u_int nextPc = Store::WordToInt(next->Sel(0));
	    if (pc >= curPc && pc <= nextPc) {
	      src = INVALID_POINTER;
	    }
	    else {
	      src = TagVal::FromWord(src->Sel(2));
	    }
	  }
          break;
        }
        
        case Begin_Inline_Function: {
          funs->Push(funCoord);
          funs->Push(coord);
          funCoord = src->Sel(0);
	  coord = Store::IntToWord(0);
          src = TagVal::FromWord(src->Sel(1));
          break;
        }
        
        case End_Inline_Function: {
          coord = funs->Pop();
          funCoord = funs->Pop();
          src = TagVal::FromWord(src->Sel(0));
          break;
        }
        
        default: {
          Error("invalid source_location tag");
        }
      }
    }
    
    funs->Push(funCoord);
    funs->Push(coord);
    PrintFrameInternal(funs, out);
  }

  
  /**
   * @return The empty source_location
   */
  static word Empty() {
    return Store::IntToWord(End);
  }
  
  
  ByteCodeSourceLocations() {
    Reset();
  }
  
  
  /**
   * Reset this to a newly-created state.
   */
  void Reset() {
    current = start = Empty();
  }
  
  
  /**
   * Called to record that a coord was encountered during jitting.
   * pc must be >= any pc called since creation/reset.
   */
  void RecordCoord(u_int pc, word coord) {
    // collapse adjacent Coords with the same pc
    TagVal *cur = TagVal::FromWord(current);
    if (cur != INVALID_POINTER && cur->GetTag() == Coord && Store::WordToInt(cur->Sel(0)) == pc) {
      cur->Init(1, coord);
    }
    else {
      TagVal *newCoord = TagVal::New(Coord, 3);
      newCoord->Init(0, Store::IntToWord(pc));
      newCoord->Init(1, coord);
      Append(newCoord->ToWord());
    }
  }
  
  
  /**
   * Called to record that a function is being inlined into the byte code.
   */
  void BeginInlineFunction(TagVal *abstractCode) {
    TagVal *tag = TagVal::New(Begin_Inline_Function, 2);
    tag->Init(0, abstractCode->Sel(0));
    Append(tag->ToWord());
  }
  
  
  /**
   * Called to record that the function currently being inlined (there must
   * be one) has now been finished.
   */
  void EndInlineFunction() {
    TagVal *tag = TagVal::New(End_Inline_Function, 1);
    Append(tag->ToWord());
  }
  
  
  /**
   * @return an Alice value of source_location type, and implicitly Reset() this.
   */
  word Export() {
    Append(Empty());
    word result = start;
    Reset();
    return result;
  }
};

#endif
