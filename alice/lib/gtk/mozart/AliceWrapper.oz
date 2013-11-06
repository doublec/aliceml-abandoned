%%%
%%% Author:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor $
import
   Pickle(load)
   Word at 'x-oz://boot/Word.ozf'
   GtkBinding
export
   'createFuncs'  : CreateFuncs
   'createFields' : CreateFields
define
   local
      Counter = {Cell.new 0}
      Names   = {Cell.new nil}

      fun {SeekName Items N}
	 case Items
	 of (Key#Item)|Ir then if Item == N then Key else {SeekName Ir N} end
	 [] nil           then unit
	 end
      end
   in
      fun {NToS N}
	 case {SeekName {Cell.access Names} N}
	 of unit then
	    Count = {Cell.access Counter}
	 in
	    {Cell.assign Counter (Count + 1)}
	    {Cell.assign Names ((Count#N)|{Cell.access Names})}
	    {VirtualString.toString "Name"#Count}
	 [] Val then {VirtualString.toString "Name"#Val}
	 end
      end
   end
   
   fun {ToS V}
      NewVal = if {IsFree V}
	       then nil
	       elseif {IsName V}
	       then {NToS V}
	       elsecase V
	       of 'struct_member(decl, expr)'(Name _) then Name
	       [] '*'('*')                            then "**"
	       elseif {VirtualString.is V}
	       then V
	       else "removed_blurb_data"
	       end
   in
      {ByteString.make {VirtualString.toString NewVal}}
   end
   
   fun {MakeCType Type}
      case Type
      of type(Name Ptrs) then 'TYPE'({ToS Name} {ToS Ptrs})
      end
   end

   local
      fun {Search V Is}
	 case Is
	 of item(Name Val)|Ir then
	    if {ToS V} == {ToS Name}
	    then Val else {Search V Ir} end
	 [] nil               then ({Pow 2 32} - 1)
	 end
      end
   in
      fun {ComputeVal V Items}
	 if {IsInt V}
	 then V
	 elseif {IsAtom V}
	 then {ComputeVal {Search V Items} Items}
	 elsecase V
	 of 'inclor(expr, expr)'(...) then
	    Vs = {Record.toList V}
	 in
	    {Word.toInt
	     {FoldL Vs
	      fun {$ E X}
		 {Word.orb E {Word.make 32 {ComputeVal X Items}}}
	      end {Word.make 32 0}}}
	 [] 'un_op cast_exp'('-' A) then
	    if A == '1' then ({Pow 2 32} - 1) else 1 end
	 [] 'un_op cast_exp'('~' A) then
	    %% Wrong but never needed.
	    {ComputeVal A Items}
	 end
      end
   end

   fun {MakeValue Value Items}
      Val = {ComputeVal Value Items}
   in
      if {IsInt Value} then 'SOME'(Val) else 'NONE' end
   end
   
   fun {MakeArgs Args}
      {Map Args fun {$ A}
		   case A
		   of arg(Type Name) then
		      'ARG'({MakeCType Type} {ToS Name})
		   end
		end}
   end

   fun {IsValidAlias Alias}
      if {IsAtom Alias}
      then true
      elsecase Alias
      of 'struct_member(decl, expr)'(...) then true
      else false
      end
   end

   fun {FilterItems Items}
      {Filter Items
       fun {$ Item}
	  case Item
	  of item(text(_) _ Alias) then {IsValidAlias Alias}
	  else false
	  end
       end}
   end
   
   fun {MakeStructItems Items}
      {Map {FilterItems Items}
       fun {$ I}
	  case I
	  of item(text(Name) Ptrs Alias) then
	     '#'(field:{ToS Name} ptrs:{ToS Ptrs} alias:{ToS Alias})
	  end
       end}
   end

   fun {MakeEnumItems Items}
      {Map Items
       fun {$ I}
	  case I
	  of item(Name Value) then
	     '#'(field:{ToS Name} value: {MakeValue Value Items})
	  end
       end}
   end
   
   local
      fun {ConvertFunction Key Value}
	 case Value
	 of function(Name RetType Args) then
	    FuncData = '#'(name:{ToS Name}
			   ret:{MakeCType RetType}
			   args: {MakeArgs Args})
	 in
	    'FUNCTION'({ToS Name} FuncData)
	 [] _ then unit
	 end
      end
      fun {ConvertStruct Key Value}
	 case Value
	 of struct(Items) then
	    'STRUCTURE'({ToS Key} {MakeStructItems Items}) 
	 [] union(Items) then
	    'UNION'({ToS Key} {MakeStructItems Items}) 
	 [] _ then unit
	 end
      end
      fun {ConvertEnum Key Value}
	 case Value
	 of enum(Items) then
	    'ENUM'({ToS Key} {MakeEnumItems Items})
	 [] _ then unit
	 end
      end
      fun {ConvertAlias Key Value}
	 case Value
	 of alias(A B) then 'ALIAS'({ToS Key} {ToS A} {ToS B})
	 [] _          then unit
	 end
      end
      %% Preliminary
      fun {ConvertType Key Value}
	 case Value
	 of type(A B) then 'ALIAS'({ToS Key} {ToS A} {ToS B})
	 [] _         then unit
	 end
      end
   in
      fun {Convert Item}
	 case Item
	 of Key#Value then
	    case Value
	    of function(...) then {ConvertFunction Key Value}
	    [] struct(...)   then {ConvertStruct Key Value}
	    [] enum(...)     then {ConvertEnum Key Value}
	    [] union(...)    then {ConvertStruct Key Value}
	    [] alias(...)    then {ConvertAlias Key Value}
	    [] type(...)     then {ConvertType Key Value}
	    [] _             then unit
	    end
	 end
      end
   end

   fun {ConvertClass Class}
      case Class
      of 'Gdk'(Name)       then '#'(space:{ToS "GDK"} name:{ToS Name})
      [] 'Gtk'(Name)       then '#'(space:{ToS "GTK"} name:{ToS Name})
      [] 'GtkCanvas'(Name) then '#'(space:{ToS "GTKCANVAS"} name:{ToS Name})
      end
   end
   
   AliceTypes

   proc {CreateFuncs AllTypes}
      OzClasses = {Map
		   {Pickle.load "x-oz://system/gtk/ClassNames.ozp"}
		   fun {$ 'class'(A B)}
		      'CLASS'({ConvertClass A} {ConvertClass B})
		   end}
      DictTypes =
      {FoldL {Dictionary.entries AllTypes}
       fun {$ E X}
	  case {Convert X}
	  of unit then E
	  [] V    then V|E
	  end
       end nil}
   in
      AliceTypes = 'CLASSES'(OzClasses)|DictTypes 
      {GtkBinding.'GtkBinding$'.createFuncs AliceTypes _}
   end
   proc {CreateFields _}
      {GtkBinding.'GtkBinding$'.createFields AliceTypes _}
   end
end
