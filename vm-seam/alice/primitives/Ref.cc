//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE2(Ref_assign) {
  DECLARE_CELL(cell, x0);
  cell->Assign(x1);
  RETURN_UNIT;
} END

DEFINE2(Ref_exchange) {
  DECLARE_CELL(cell, x0);
  RETURN(cell->Exchange(x1));
} END

void PrimitiveTable::RegisterRef() {
  Register("Ref.:=", Ref_assign, 2);
  Register("Ref.exchange", Ref_exchange, 2);
}
