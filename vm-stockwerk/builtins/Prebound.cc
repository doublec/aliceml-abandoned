//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include "store/store.hh"
#include "datalayer/alicedata.hh"
#include "Prebound.hh"

namespace Builtins {
  namespace Prebound {
    namespace Array {
      FUNPTR array    = (FUNPTR) ::Builtins::Array::array;
      FUNPTR fromList = (FUNPTR) ::Builtins::Array::fromList;
      FUNPTR length   = (FUNPTR) ::Builtins::Array::length;
      FUNPTR sub      = (FUNPTR) ::Builtins::Array::sub;
      FUNPTR update   = (FUNPTR) ::Builtins::Array::update;
    }
    namespace Char {
      FUNPTR opless      = (FUNPTR) ::Builtins::Char::opless;
      FUNPTR opgreater   = (FUNPTR) ::Builtins::Char::opgreater;
      FUNPTR oplessEq    = (FUNPTR) ::Builtins::Char::oplessEq;
      FUNPTR opgreaterEq = (FUNPTR) ::Builtins::Char::opgreaterEq;
      FUNPTR ord         = (FUNPTR) ::Builtins::Char::ord;
      FUNPTR chr         = (FUNPTR) ::Builtins::Char::chr;
      FUNPTR isAlpha     = (FUNPTR) ::Builtins::Char::isAlpha;
      FUNPTR isAlphaNum  = (FUNPTR) ::Builtins::Char::isAlphaNum;
      FUNPTR isCntrl     = (FUNPTR) ::Builtins::Char::isCntrl;
      FUNPTR isDigit     = (FUNPTR) ::Builtins::Char::isDigit;
      FUNPTR isGraph     = (FUNPTR) ::Builtins::Char:isGraph;
      FUNPTR isHexDigit  = (FUNPTR) ::Builtins::Char::isHexDigit;
      FUNPTR isLower     = (FUNPTR) ::Builtins::Char::isLower;
      FUNPTR isPrint     = (FUNPTR) ::Builtins::Char::isPrint;
      FUNPTR isPunct     = (FUNPTR) ::Builtins::Char::isPunct;
      FUNPTR isSpace     = (FUNPTR) ::Builtins::Char::isSpace;
      FUNPTR isUpper     = (FUNPTR) ::Builtins::Char::isUpper;
      FUNPTR toLower     = (FUNPTR) ::Builtins::Char::toLower;
      FUNPTR toUpper     = (FUNPTR) ::Builtins::Char::toUpper;
    }
    namespace CommandLine {
      FUNPTR name      = (FUNPTR) ::Builtins::CommandLine::name;
      FUNPTR arguments = (FUNPTR) ::Builtins::CommandLine::arguments;
    }
    namespace Future {
      FUNPTR alarmQuote = (FUNPTR) ::Builtins::Future::alarmQuote;
      FUNPTR await      = (FUNPTR) ::Builtins::Future::await;
      FUNPTR awaitOne   = (FUNPTR) ::Builtins::Future::awaitOne;
      FUNPTR byneed     = (FUNPTR) ::Builtins::Future::byneed;
      FUNPTR concur     = (FUNPTR) ::Builtins::Future::concur;
      FUNPTR isFailed   = (FUNPTR) ::Builtins::Future::isFailed;
      FUNPTR isFuture   = (FUNPTR) ::Builtins::Future::isFuture;
    }
    namespace General {
      FUNPTR assign   = (FUNPTR) ::Builtins::General::assign;
      FUNPTR exchange = (FUNPTR) ::Builtins::General::exchange;
      FUNPTR exnName  = (FUNPTR) ::Builtins::General::exnName;
    }
    namespace Hole {
      FUNPTR fail     = (FUNPTR) ::Builtins::Hole::fail;
      FUNPTR fill     = (FUNPTR) ::Builtins::Hole::fill;
      FUNPTR future   = (FUNPTR) ::Builtins::Hole::future;
      FUNPTR hole     = (FUNPTR) ::Builtins::Hole::hole;
      FUNPTR isFailed = (FUNPTR) ::Builtins::Hole::isFailed;
    }
    namespace Int {
      FUNPTR opnegate    = (FUNPTR) ::Builtins::Int::opnegate;
      FUNPTR opadd       = (FUNPTR) ::Builtins::Int::opadd;
      FUNPTR opsub       = (FUNPTR) ::Builtins::Int::opsub;
      FUNPTR opmul       = (FUNPTR) ::Builtins::Int::opmul;
      FUNPTR opless      = (FUNPTR) ::Builtins::Int::opless;
      FUNPTR opgreater   = (FUNPTR) ::Builtins::Int::opgreater;
      FUNPTR oplessEq    = (FUNPTR) ::Builtins::Int::oplessEq;
      FUNPTR opgreaterEq = (FUNPTR) ::Builtins::Int::opgreaterEq;
      FUNPTR abs         = (FUNPTR) ::Builtins::Int::abs;
      FUNPTR compare     = (FUNPTR) ::Builtins::Int::compare;
      FUNPTR div         = (FUNPTR) ::Builtins::Int::div;
      FUNPTR mod         = (FUNPTR) ::Builtins::Int::mod;
      FUNPTR quot        = (FUNPTR) ::Builtins::Int::quot;
      FUNPTR rem         = (FUNPTR) ::Builtins::Int::rem;
      FUNPTR toString    = (FUNPTR) ::Builtins::Int::toString;
    }
    namespace Math {
      FUNPTR acos  = (FUNPTR) ::Builtins::Math::acos;
      FUNPTR acosh = (FUNPTR) ::Builtins::Math::acosh;
      FUNPTR asin  = (FUNPTR) ::Builtins::Math::asin;
      FUNPTR asinh = (FUNPTR) ::Builtins::Math::asinh;
      FUNPTR atan  = (FUNPTR) ::Builtins::Math::atan;
      FUNPTR atanh = (FUNPTR) ::Builtins::Math::atanh;
      FUNPTR atan2 = (FUNPTR) ::Builtins::Math::atan2;
      FUNPTR cos   = (FUNPTR) ::Builtins::Math::cos;
      FUNPTR cosh  = (FUNPTR) ::Builtins::Math::cosh;
      FUNPTR exp   = (FUNPTR) ::Builtins::Math::exp;
      FUNPTR ln    = (FUNPTR) ::Builtins::Math::ln;
      FUNPTR pow   = (FUNPTR) ::Builtins::Math::pow;
      FUNPTR sin   = (FUNPTR) ::Builtins::Math::sin;
      FUNPTR sinh  = (FUNPTR) ::Builtins::Math::sinh;
      FUNPTR sqrt  = (FUNPTR) ::Builtins::Math::sqrt;
      FUNPTR tan   = (FUNPTR) ::Builtins::Math::tan;
      FUNPTR tanh  = (FUNPTR) ::Builtins::Math::tanh;
    }
    namespace OS {
      FUNPTR system = (FUNPTR) ::Builtins::OS::system;
      FUNPTR exit   = (FUNPTR) ::Builtins::OS::exit;
      FUNPTR getEnv = (FUNPTR) ::Builtins::OS::getEnv;
    }
    namespace Real {
      FUNPTR opnegate    = (FUNPTR) ::Builtins::Real::opnegate;
      FUNPTR opadd       = (FUNPTR) ::Builtins::Real::opadd;
      FUNPTR opsub       = (FUNPTR) ::Builtins::Real::opsub;
      FUNPTR opmul       = (FUNPTR) ::Builtins::Real::opmul;
      FUNPTR opdiv       = (FUNPTR) ::Builtins::Real::opdiv;
      FUNPTR opless      = (FUNPTR) ::Builtins::Real::opless;
      FUNPTR opgreater   = (FUNPTR) ::Builtins::Real::opgreater;
      FUNPTR oplessEq    = (FUNPTR) ::Builtins::Real::oplessEq;
      FUNPTR opgreaterEq = (FUNPTR) ::Builtins::Real::opgreaterEq;
      FUNPTR ceil        = (FUNPTR) ::Builtins::Real::ceil;
      FUNPTR compare     = (FUNPTR) ::Builtins::Real::compare;
      FUNPTR floor       = (FUNPTR) ::Builtins::Real::floor;
      FUNPTR fromInt     = (FUNPTR) ::Builtins::Real::fromInt;
      FUNPTR realCeil    = (FUNPTR) ::Builtins::Real::realCeil;
      FUNPTR realFloor   = (FUNPTR) ::Builtins::Real::realFloor;
      FUNPTR realRound   = (FUNPTR) ::Builtins::Real::realRound;
      FUNPTR realTrunc   = (FUNPTR) ::Builtins::Real::realTrunc;
      FUNPTR rem         = (FUNPTR) ::Builtins::Real::rem;
      FUNPTR round       = (FUNPTR) ::Builtins::Real::round;
      FUNPTR toString    = (FUNPTR) ::Builtins::Real::toString;
      FUNPTR trunc       = (FUNPTR) ::Builtins::Real::trunc;
    }
    namespace String {
      FUNPTR append      = (FUNPTR) ::Builtins::String::append;
      FUNPTR opless      = (FUNPTR) ::Builtins::String::opless;
      FUNPTR opgreater   = (FUNPTR) ::Builtins::String::opgreater;
      FUNPTR oplessEq    = (FUNPTR) ::Builtins::String::oplessEq;
      FUNPTR opgreaterEq = (FUNPTR) ::Builtins::String::opgreaterEq;
      FUNPTR compare     = (FUNPTR) ::Builtins::String::compare;
      FUNPTR explode     = (FUNPTR) ::Builtins::String::explode;
      FUNPTR implode     = (FUNPTR) ::Builtins::String::implode;
      FUNPTR size        = (FUNPTR) ::Builtins::String::size;
      FUNPTR sub         = (FUNPTR) ::Builtins::String::sub;
      FUNPTR substring   = (FUNPTR) ::Builtins::String::substring;
      FUNPTR str         = (FUNPTR) ::Builtins::String::str;
    }
    namespace TextIO {
      FUNPTR openIn    = (FUNPTR) ::Builtins::TextIO::openIn;
      FUNPTR inputAll  = (FUNPTR) ::Builtins::TextIO::inputAll;
      FUNPTR inputLine = (FUNPTR) ::Builtins::TextIO::inputLine;
      FUNPTR closeIn   = (FUNPTR) ::Builtins::TextIO::closeIn;
      FUNPTR openOut   = (FUNPTR) ::Builtins::TextIO::openOut;
      FUNPTR output    = (FUNPTR) ::Builtins::TextIO::output;
      FUNPTR output1   = (FUNPTR) ::Builtins::TextIO::output1;
      FUNPTR flushOut  = (FUNPTR) ::Builtins::TextIO::flushOut;
      FUNPTR closeOut  = (FUNPTR) ::Builtins::TextIO::closeOut;
      FUNPTR print     = (FUNPTR) ::Builtins::TextIO::print;
    }
    namespace Thread {
      FUNPTR terminate   = (FUNPTR) ::Builtins::Thread::terminate;
      FUNPTR current     = (FUNPTR) ::Builtins::Thread::current;
      FUNPTR isSuspended = (FUNPTR) ::Builtins::Thread::isSuspended;
      FUNPTR raiseIn     = (FUNPTR) ::Builtins::Thread::raiseIn;
      FUNPTR resume      = (FUNPTR) ::Builtins::Thread::resume;
      FUNPTR state       = (FUNPTR) ::Builtins::Thread::state;
      FUNPTR suspend     = (FUNPTR) ::Builtins::Thread::suspend;
      FUNPTR yield       = (FUNPTR) ::Builtins::Thread::yield;
    }
    namespace Unsafe {
      namespace Array {
	FUNPTR sub    = (FUNPTR) ::Builtins::Unsafe::Array::sub;
	FUNPTR update = (FUNPTR) ::Builtins::Unsafe::Array::update;
      }
      namespace String {
	FUNPTR sub = (FUNPTR) ::Builtins::Unsafe::String::sub;
      }
      namespace Vector {
	FUNPTR sub = (FUNPTR) ::Builtins::Unsafe::Vector::sub;
      }
      FUNPTR cast     = (FUNPTR) ::Builtins::Unsafe::cast;
      FUNPTR getTag   = (FUNPTR) ::Builtins::Unsafe::getTag;
      FUNPTR getValue = (FUNPTR) ::Builtins::Unsafe::getValue;
    }
    namespace Vector {
      FUNPTR fromList = (FUNPTR) ::Builtins::Vector::fromList;
      FUNPTR length   = (FUNPTR) ::Builtins::Vector::length;
      FUNPTR sub      = (FUNPTR) ::Builtins::Vector::sub;
    }
    namespace Word {
      FUNPTR fromIntQuote = (FUNPTR) ::Builtins::Word::fromIntQuote;
      FUNPTR fromInt      = (FUNPTR) ::Builtins::Word::fromInt;
      FUNPTR toInt        = (FUNPTR) ::Builtins::Word::toInt;
      FUNPTR toIntX       = (FUNPTR) ::Builtins::Word::toIntX;
      FUNPTR opadd        = (FUNPTR) ::Builtins::Word::opadd;
      FUNPTR opsub        = (FUNPTR) ::Builtins::Word::opsub;
      FUNPTR opmul        = (FUNPTR) ::Builtins::Word::opmul;
      FUNPTR opdiv        = (FUNPTR) ::Builtins::Word::opdiv;
      FUNPTR mod          = (FUNPTR) ::Builtins::Word::mod;
      FUNPTR orb          = (FUNPTR) ::Builtins::Word::orb;
      FUNPTR xorb         = (FUNPTR) ::Builtins::Word::xorb;
      FUNPTR andb         = (FUNPTR) ::Builtins::Word::andb;
      FUNPTR notb         = (FUNPTR) ::Builtins::Word::notb;
      FUNPTR shl          = (FUNPTR) ::Builtins::Word::shl;
      FUNPTR shr          = (FUNPTR) ::Builtins::Word::shr;
      FUNPTR arithshr     = (FUNPTR) ::Builtins::Word::arithshr;
      FUNPTR toString     = (FUNPTR) ::Builtins::Word::toString;
    }
    FUNPTR opeq    = (FUNPTR) ::Builtins::opeq;
    FUNPTR opnoteq = (FUNPTR) ::Builtins::opnoteq;
  }
}
