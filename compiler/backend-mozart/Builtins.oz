%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1997-1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

%%
%% This defines a function `GetBuiltinInfo' that returns information
%% about the builtin with a given name A.  This information is either:
%%
%%    noInformation
%%       if A does not denote a known builtin.
%%
%%    builtin(types: [...] det: [...] imods: [bool] ...)
%%       if A denotes a known builtin with argument types and determinancy
%%       as given.  The following features may or may not be contained in
%%       the record, as appropriate:
%%
%%          imods: [bool]
%%             for each input argument for which this list has a `true',
%%             no assumptions may be made about the contents of the
%%             corresponding register after the builtin application.
%%          test: B
%%             if this feature is present and B is true, then this
%%             builtin may be used as argument to the testBI instruction.
%%          negated: A
%%             if this feature is present then A is the name of a builtin
%%             that returns the negated result from this builtin.
%%          doesNotReturn: B
%%             if this feature is present and B is true, then the
%%             instructions following the call to A are never executed
%%             unless branched to from elsewhere.
%%

functor
export
   getInfo: GetBuiltinInfo
prepare
   BuiltinTable = builtinTable(
'Array.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Array.high':
	builtin(
		types: ['array' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Array.get':
	builtin(
		types: ['array' 'int' 'value']
		det: [det det any]
		imods: [false false]
	)
'Array.put':
	builtin(
		types: ['array' 'int' 'value']
		det: [det det any]
		imods: [false false false]
	)
'Array.new':
	builtin(
		types: ['int' 'int' 'value' 'array']
		det: [det det any any(det)]
		imods: [false false false]
	)
'Array.low':
	builtin(
		types: ['array' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Atom.toString':
	builtin(
		types: ['atom' 'string']
		det: [det any(det)]
		imods: [false]
	)
'Atom.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'BitArray.disjoint':
	builtin(
		types: ['bitArray' 'bitArray' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'BitArray.clear':
	builtin(
		types: ['bitArray' 'int']
		det: [det det]
		imods: [false false]
	)
'BitArray.new':
	builtin(
		types: ['int' 'int' 'bitArray']
		det: [det det any(det)]
		imods: [false false]
	)
'BitArray.disj':
	builtin(
		types: ['bitArray' 'bitArray']
		det: [det det]
		imods: [false false]
	)
'BitArray.conj':
	builtin(
		types: ['bitArray' 'bitArray']
		det: [det det]
		imods: [false false]
	)
'BitArray.card':
	builtin(
		types: ['bitArray' 'int']
		det: [det any(det)]
		imods: [false]
	)
'BitArray.set':
	builtin(
		types: ['bitArray' 'int']
		det: [det det]
		imods: [false false]
	)
'BitArray.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'BitArray.toList':
	builtin(
		types: ['bitArray' list('int')]
		det: [det any(det)]
		imods: [false]
	)
'BitArray.nimpl':
	builtin(
		types: ['bitArray' 'bitArray']
		det: [det det]
		imods: [false false]
	)
'BitArray.high':
	builtin(
		types: ['bitArray' 'int']
		det: [det any(det)]
		imods: [false]
	)
'BitArray.test':
	builtin(
		types: ['bitArray' 'int' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'BitArray.clone':
	builtin(
		types: ['bitArray' 'bitArray']
		det: [det any(det)]
		imods: [false]
	)
'BitArray.low':
	builtin(
		types: ['bitArray' 'int']
		det: [det any(det)]
		imods: [false]
	)
'BitArray.complementToList':
	builtin(
		types: ['bitArray' list('int')]
		det: [det any(det)]
		imods: [false]
	)
'Bool.not':
	builtin(
		types: ['bool' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Bool.or':
	builtin(
		types: ['bool' 'bool' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'Bool.and':
	builtin(
		types: ['bool' 'bool' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'Bool.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Cell.new':
	builtin(
		types: ['value' 'cell']
		det: [any any(det)]
		imods: [false]
	)
'Cell.access':
	builtin(
		types: ['cell' 'value']
		det: [det any]
		imods: [false]
	)
'Cell.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Cell.exchangeFun':
	builtin(
		types: ['cell' 'value' 'value']
		det: [det any any]
		imods: [false false]
	)
'Cell.assign':
	builtin(
		types: ['cell' 'value']
		det: [det any]
		imods: [false false]
	)
'Char.toAtom':
	builtin(
		types: ['char' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'Char.isLower':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isXDigit':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isPunct':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isSpace':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isUpper':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isAlNum':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isPrint':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.toLower':
	builtin(
		types: ['char' 'char']
		det: [det any(det)]
		imods: [false]
	)
'Char.isDigit':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isGraph':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.isCntrl':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Char.toUpper':
	builtin(
		types: ['char' 'char']
		det: [det any(det)]
		imods: [false]
	)
'Char.type':
	builtin(
		types: ['char' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'Char.isAlpha':
	builtin(
		types: ['char' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Chunk.new':
	builtin(
		types: ['record' 'chunk']
		det: [det any(det)]
		imods: [false]
	)
'Chunk.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Class.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Class.isSited':
	builtin(
		types: ['class' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Class.new':
	builtin(
		types: ['record' 'bool' 'bool' 'class']
		det: [any det det any(det)]
		imods: [false false false]
	)
'Class.isLocking':
	builtin(
		types: ['class' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Dictionary.keys':
	builtin(
		types: ['dictionary' list('feature')]
		det: [det any(det)]
		imods: [false]
	)
'Dictionary.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Dictionary.remove':
	builtin(
		types: ['dictionary' 'feature']
		det: [det det]
		imods: [false false]
	)
'Dictionary.get':
	builtin(
		types: ['dictionary' 'feature' 'value']
		det: [det det any]
		imods: [false false]
	)
'Dictionary.clone':
	builtin(
		types: ['dictionary' 'dictionary']
		det: [det any(det)]
		imods: [false]
	)
'Dictionary.items':
	builtin(
		types: ['dictionary' list('value')]
		det: [det any(det)]
		imods: [false]
	)
'Dictionary.toRecord':
	builtin(
		types: ['literal' 'dictionary' 'record']
		det: [det det any(det)]
		imods: [false false]
	)
'Dictionary.condGet':
	builtin(
		types: ['dictionary' 'feature' 'value' 'value']
		det: [det det any any]
		imods: [false false false]
	)
'Dictionary.waitOr':
	builtin(
		types: ['dictionary' 'feature']
		det: [det any(det)]
		imods: [false]
	)
'Dictionary.removeAll':
	builtin(
		types: ['dictionary']
		det: [det]
		imods: [false]
	)
'Dictionary.member':
	builtin(
		types: ['dictionary' 'feature' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'Dictionary.put':
	builtin(
		types: ['dictionary' 'feature' 'value']
		det: [det det any]
		imods: [false false false]
	)
'Dictionary.entries':
	builtin(
		types: ['dictionary' list(pair('feature' 'value'))]
		det: [det any(det)]
		imods: [false]
	)
'Dictionary.markSafe':
	builtin(
		types: ['dictionary']
		det: [det]
		imods: [false]
	)
'Dictionary.new':
	builtin(
		types: ['dictionary']
		det: [any(det)]
		imods: nil
	)
'Dictionary.isEmpty':
	builtin(
		types: ['dictionary' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Exception.fail':
	builtin(
		types: nil
		det: nil
		imods: nil
		doesNotReturn: true
	)
'Exception.raise':
	builtin(
		types: ['value']
		det: [any]
		imods: [false]
		doesNotReturn: true
	)
'Exception.raiseError':
	builtin(
		types: ['value']
		det: [any]
		imods: [false]
		doesNotReturn: true
	)
'Float.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Float.fPow':
	builtin(
		types: ['float' 'float' 'float']
		det: [det det any(det)]
		imods: [false false]
	)
'Float.atan':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.ceil':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.\'/\'':
	builtin(
		types: ['float' 'float' 'float']
		det: [det det any(det)]
		imods: [false false]
	)
'Float.exp':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.cos':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.atan2':
	builtin(
		types: ['float' 'float' 'float']
		det: [det det any(det)]
		imods: [false false]
	)
'Float.toInt':
	builtin(
		types: ['float' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Float.acos':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.round':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.toString':
	builtin(
		types: ['float' 'string']
		det: [det any(det)]
		imods: [false]
	)
'Float.sqrt':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.sin':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.log':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.floor':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.asin':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Float.tan':
	builtin(
		types: ['float' 'float']
		det: [det any(det)]
		imods: [false]
	)
'ForeignPointer.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'ForeignPointer.toInt':
	builtin(
		types: ['foreignPointer' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Int.mod':
	builtin(
		types: ['int' 'int' 'int']
		det: [det det any(det)]
		imods: [false false]
	)
'Int.toFloat':
	builtin(
		types: ['int' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Int.\'+1\'':
	builtin(
		types: ['int' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Int.toString':
	builtin(
		types: ['int' 'string']
		det: [det any(det)]
		imods: [false]
	)
'Int.div':
	builtin(
		types: ['int' 'int' 'int']
		det: [det det any(det)]
		imods: [false false]
	)
'Int.\'-1\'':
	builtin(
		types: ['int' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Int.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'List.toRecord':
	builtin(
		types: ['literal' list(pair('feature' 'value')) 'record']
		det: [det det any(det)]
		imods: [false false]
	)
'Literal.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Lock.new':
	builtin(
		types: ['lock']
		det: [any(det)]
		imods: nil
	)
'Lock.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Name.newUnique':
	builtin(
		types: ['atom' 'name']
		det: [det any(det)]
		imods: [false]
	)
'Name.new':
	builtin(
		types: ['name']
		det: [any(det)]
		imods: nil
	)
'Name.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Number.\'+\'':
	builtin(
		types: ['number' 'number' 'number']
		det: [det det any(det)]
		imods: [false false]
	)
'Number.\'-\'':
	builtin(
		types: ['number' 'number' 'number']
		det: [det det any(det)]
		imods: [false false]
	)
'Number.\'~\'':
	builtin(
		types: ['number' 'number']
		det: [det any(det)]
		imods: [false]
	)
'Number.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Number.abs':
	builtin(
		types: ['number' 'number']
		det: [det any(det)]
		imods: [false]
	)
'Number.\'*\'':
	builtin(
		types: ['number' 'number' 'number']
		det: [det det any(det)]
		imods: [false false]
	)
'Object.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Object.\'<-\'':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false false]
	)
'Object.newObject':
	builtin(
		types: ['class' 'object']
		det: [det any(det)]
		imods: [false]
	)
'Object.\',\'':
	builtin(
		types: ['class' 'record']
		det: [det det]
		imods: [false false]
	)
'Object.\'@\'':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false]
	)
'Object.ooExch':
	builtin(
		types: ['value' 'value' 'value']
		det: [any any any]
		imods: [false false]
	)
'Object.getClass':
	builtin(
		types: ['object' 'class']
		det: [det any(det)]
		imods: [false]
	)
'Object.ooGetLock':
	builtin(
		types: ['lock']
		det: [any]
		imods: [false]
	)
'Object.new':
	builtin(
		types: ['class' 'record' 'value']
		det: [det det any]
		imods: [false false false]
	)
'Object.send':
	builtin(
		types: ['record' 'class' 'object']
		det: [det det det]
		imods: [false false false]
	)
'Object.copyRecord':
	builtin(
		types: ['record' 'record']
		det: [det any(det)]
		imods: [false]
	)
'Port.new':
	builtin(
		types: ['value' 'port']
		det: [any any(det)]
		imods: [false]
	)
'Port.send':
	builtin(
		types: ['port' 'value']
		det: [det any]
		imods: [false false]
	)
'Port.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Procedure.arity':
	builtin(
		types: ['procedure' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Procedure.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Procedure.apply':
	builtin(
		types: ['procedureOrObject' list('value')]
		det: [det det]
		imods: [false false]
	)
'Record.adjoin':
	builtin(
		types: ['record' 'record' 'record']
		det: [det det any(det)]
		imods: [false false]
	)
'Record.waitOr':
	builtin(
		types: ['record' 'value']
		det: [det any]
		imods: [false]
	)
'Record.toDictionary':
	builtin(
		types: ['record' 'dictionary']
		det: [det any(det)]
		imods: [false]
	)
'Record.arity':
	builtin(
		types: ['record' list('feature')]
		det: [det any(det)]
		imods: [false]
	)
'Record.adjoinAt':
	builtin(
		types: ['record' 'feature' 'value' 'record']
		det: [det det any any(det)]
		imods: [false false false]
	)
'Record.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Record.testFeature':
	builtin(
		types: ['value' 'feature' 'bool' 'value']
		det: [any det any(det) any]
		imods: [false false]
		test: true
	)
'Record.make':
	builtin(
		types: ['literal' list('feature') 'record']
		det: [det det any(det)]
		imods: [false false]
	)
'Record.aritySublist':
	builtin(
		types: ['record' 'record' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'Record.testLabel':
	builtin(
		types: ['value' 'literal' 'bool']
		det: [any det any(det)]
		imods: [false false]
		test: true
	)
'Record.test':
	builtin(
		types: ['value' 'literal' list('feature') 'bool']
		det: [any det det any(det)]
		imods: [false false false]
		test: true
	)
'Record.width':
	builtin(
		types: ['record' 'int']
		det: [det any(det)]
		imods: [false]
	)
'Record.label':
	builtin(
		types: ['recordC' 'literal']
		det: [detOrKinded any(det)]
		imods: [false]
	)
'Record.clone':
	builtin(
		types: ['record' 'record']
		det: [det any(det)]
		imods: [false]
	)
'Record.adjoinList':
	builtin(
		types: ['record' list(pair('feature' 'value')) 'record']
		det: [det det any(det)]
		imods: [false false]
	)
'String.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'String.toAtom':
	builtin(
		types: ['string' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'String.toInt':
	builtin(
		types: ['string' 'int']
		det: [det any(det)]
		imods: [false]
	)
'String.toFloat':
	builtin(
		types: ['string' 'float']
		det: [det any(det)]
		imods: [false]
	)
'Thread.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Thread.injectException':
	builtin(
		types: ['thread' 'value']
		det: [det det]
		imods: [false false]
	)
'Thread.preempt':
	builtin(
		types: ['thread']
		det: [det]
		imods: [false]
	)
'Thread.setPriority':
	builtin(
		types: ['thread' 'atom']
		det: [det det]
		imods: [false false]
	)
'Thread.resume':
	builtin(
		types: ['thread']
		det: [det]
		imods: [false]
	)
'Thread.state':
	builtin(
		types: ['thread' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'Thread.getPriority':
	builtin(
		types: ['thread' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'Thread.suspend':
	builtin(
		types: ['thread']
		det: [det]
		imods: [false]
	)
'Thread.create':
	builtin(
		types: ['procedure/0']
		det: [det]
		imods: [false]
	)
'Thread.isSuspended':
	builtin(
		types: ['thread' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Thread.this':
	builtin(
		types: ['thread']
		det: [any(det)]
		imods: nil
	)
'Time.alarm':
	builtin(
		types: ['int' 'unit']
		det: [det any]
		imods: [false false]
	)
'Time.time':
	builtin(
		types: ['int']
		det: [any(det)]
		imods: nil
	)
'Tuple.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Tuple.make':
	builtin(
		types: ['literal' 'int' 'tuple']
		det: [det det any(det)]
		imods: [false false]
	)
'Unit.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'Value.wait':
	builtin(
		types: ['value']
		det: [det]
		imods: [false]
	)
'Value.waitOr':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false false]
	)
'Value.isFuture':
	builtin(
		types: ['value' 'bool']
		det: [any any(det)]
		imods: [false]
	)
'Value.byNeed':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false]
	)
'Value.isDet':
	builtin(
		types: ['value' 'bool']
		det: [any any(det)]
		imods: [false]
	)
'Value.future':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false]
	)
'Value.\'\\=\'':
	builtin(
		types: ['value' 'value' 'bool']
		det: [detOrKinded detOrKinded any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'==\''
	)
'Value.waitQuiet':
	builtin(
		types: ['value']
		det: [det]
		imods: [false]
	)
'Value.isFree':
	builtin(
		types: ['value' 'bool']
		det: [any any(det)]
		imods: [false]
	)
'Value.byNeedDot':
	builtin(
		types: ['value' 'feature' 'value']
		det: [any det any]
		imods: [false false]
	)
'Value.\'=<\'':
	builtin(
		types: ['comparable' 'comparable' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'>\''
	)
'Value.\'==\'':
	builtin(
		types: ['value' 'value' 'bool']
		det: [detOrKinded detOrKinded any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'\\=\''
	)
'Value.\'>=\'':
	builtin(
		types: ['comparable' 'comparable' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'<\''
	)
'Value.toVirtualString':
	builtin(
		types: ['value' 'int' 'int' 'string']
		det: [any det det any(det)]
		imods: [false false false]
	)
'Value.\'<\'':
	builtin(
		types: ['comparable' 'comparable' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'>=\''
	)
'Value.type':
	builtin(
		types: ['value' 'atom']
		det: [det any(det)]
		imods: [false]
	)
'Value.\'=\'':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false false]
	)
'Value.\'>\'':
	builtin(
		types: ['comparable' 'comparable' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
		negated: 'Value.\'=<\''
	)
'Value.status':
	builtin(
		types: ['value' 'tuple']
		det: [any any(det)]
		imods: [false]
	)
'Value.condSelect':
	builtin(
		types: ['recordCOrChunk' 'feature' 'value' 'value']
		det: [detOrKinded det any any]
		imods: [false false false]
	)
'Value.min':
	builtin(
		types: ['comparable' 'comparable' 'comparable']
		det: [det det any(det)]
		imods: [false false]
	)
'Value.max':
	builtin(
		types: ['comparable' 'comparable' 'comparable']
		det: [det det any(det)]
		imods: [false false]
	)
'Value.nameVariable':
	builtin(
		types: ['value' 'atom']
		det: [any det]
		imods: [false false]
	)
'Value.\'!!\'':
	builtin(
		types: ['value' 'value']
		det: [any any]
		imods: [false]
	)
'Value.hasFeature':
	builtin(
		types: ['recordCOrChunk' 'feature' 'bool']
		det: [detOrKinded det any(det)]
		imods: [false false]
		test: true
	)
'Value.isKinded':
	builtin(
		types: ['value' 'bool']
		det: [any any(det)]
		imods: [false]
	)
'Value.\'.\'':
	builtin(
		types: ['recordCOrChunk' 'feature' 'value']
		det: [detOrKinded det any]
		imods: [false false]
	)
'VirtualString.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [true]
		test: true
	)
'VirtualString.toByteString':
	builtin(
		types: ['virtualString' 'int' 'virtualString' 'byteString']
		det: [det det det any(det)]
		imods: [true true false]
	)
'VirtualString.length':
	builtin(
		types: ['virtualString' 'int' 'int']
		det: [any det any(det)]
		imods: [true true]
	)
'BitString.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'BitString.make':
	builtin(
		types: ['int' list('int') 'bitString']
		det: [det det any(det)]
		imods: [false false]
	)
'BitString.toList':
	builtin(
		types: ['bitString' list('int')]
		det: [det any(det)]
		imods: [false]
	)
'BitString.get':
	builtin(
		types: ['bitString' 'int' 'bool']
		det: [det det any(det)]
		imods: [false false]
		test: true
	)
'BitString.width':
	builtin(
		types: ['bitString' 'int']
		det: [det any(det)]
		imods: [false]
	)
'BitString.put':
	builtin(
		types: ['bitString' 'int' 'bool' 'bitString']
		det: [det det det any(det)]
		imods: [false false false]
	)
'BitString.disj':
	builtin(
		types: ['bitString' 'bitString' 'bitString']
		det: [det det any(det)]
		imods: [false false]
	)
'BitString.conj':
	builtin(
		types: ['bitString' 'bitString' 'bitString']
		det: [det det any(det)]
		imods: [false false]
	)
'BitString.nega':
	builtin(
		types: ['bitString' 'bitString']
		det: [det any(det)]
		imods: [false]
	)
'ByteString.append':
	builtin(
		types: ['byteString' 'byteString' 'byteString']
		det: [det det any(det)]
		imods: [false false]
	)
'ByteString.toString':
	builtin(
		types: ['byteString' 'string']
		det: [det any(det)]
		imods: [false]
	)
'ByteString.is':
	builtin(
		types: ['value' 'bool']
		det: [det any(det)]
		imods: [false]
		test: true
	)
'ByteString.make':
	builtin(
		types: ['string' 'byteString']
		det: [det any(det)]
		imods: [false]
	)
'ByteString.cmp':
	builtin(
		types: ['byteString' 'byteString' 'int']
		det: [det det any(det)]
		imods: [false false]
	)
'ByteString.toStringWithTail':
	builtin(
		types: ['byteString' 'value' 'string']
		det: [det any any]
		imods: [false false]
	)
'ByteString.width':
	builtin(
		types: ['byteString' 'int']
		det: [det any(det)]
		imods: [false]
	)
'ByteString.get':
	builtin(
		types: ['byteString' 'int' 'int']
		det: [det det any(det)]
		imods: [false false]
	)
'ByteString.slice':
	builtin(
		types: ['byteString' 'int' 'int' 'byteString']
		det: [det det det any(det)]
		imods: [false false false]
	)
'ByteString.strchr':
	builtin(
		types: ['byteString' 'int' 'int' 'value']
		det: [det det det any(det)]
		imods: [false false false]
	)
'InterFault.interDistHandlerDeInstall':
	builtin(
		types: ['value' 'value' 'bool']
		det: [any any any(det)]
		imods: [false false]
		test: true
	)
'InterFault.interDistHandlerInstall':
	builtin(
		types: ['value' 'value' 'bool']
		det: [any any any(det)]
		imods: [false false]
		test: true
	)
		     )

   proc {E Name T}
      {Exception.raiseError compiler(badBuiltinTableEntry Name T)}
   end

   %%
   %% Do some consistency checks on the builtin table
   %%

   {Record.forAllInd BuiltinTable
    proc {$ Name Entry}
       if {HasFeature Entry types} andthen {IsList Entry.types} then skip
       else {E Name types}
       end
       if {HasFeature Entry det} andthen {IsList Entry.det} then skip
       else {E Name det}
       end
       if {Length Entry.types} == {Length Entry.det} then skip
       else {E Name typesdet}
       end
       if {Not {HasFeature Entry imods}} then skip
       elseif {IsList Entry.imods} then skip
       elseif {Length Entry.imods} =< Entry.iarity then skip
       else {E Name imods}
       end
       if {Not {HasFeature Entry test}} then skip
       elseif {IsBool Entry.test} then skip
       else {E Name test}
       end
       if {HasFeature Entry negated} then NBI = Entry.negated in
	  if {IsAtom NBI} then
	     if {HasFeature Entry test} then
		if {HasFeature BuiltinTable NBI} then
		   if {HasFeature BuiltinTable.NBI test} then skip
		   else {E Name negatedNotTest2}
		   end
		else {E Name undefinedNegatedBuiltin}
		end
	     else {E Name negatedNotTest}
	     end
	  else {E Name negated}
	  end
       else skip
       end
       if {Not {HasFeature Entry doesNotReturn}} then skip
       elseif {IsBool Entry.doesNotReturn} then skip
       else {E Name doesNotReturn}
       end
    end}

   fun {GetBuiltinInfo Name}
      {CondSelect BuiltinTable Name noInformation}
   end
end
