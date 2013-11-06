% Authors:
%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%
% Copyright:
%   Andreas Rossberg, 2001
%
% Last Change:
%   $Date$ by $Author$
%   $Revision$

functor
import
    Select at x-ozlib://duchier/cp/Select.ozf
export
    'Select$' : SelectModule
define
    SelectModule = 'Select'(
	'the':		fun {$ S I} {Select.the S I} unit end
	'fd':		fun {$ Ms I M} {Select.fd Ms I M} unit end
	'fs':		fun {$ Ss I S} {Select.fs Ss I S} unit end
	'union':	fun {$ Ss I S} {Select.union Ss I S} unit end
	'intersect':	fun {$ Ss I S} {Select.inter Ss I S} unit end
	'seqUnion':	fun {$ Ss I S} {Select.seqUnion Ss I S} unit end
	'indexedUnion':	fun {$ Ss I S} {Select.indexedUnion Ss I S} unit end
    )
end
