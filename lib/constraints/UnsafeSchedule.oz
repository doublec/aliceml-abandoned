%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Schedule
export
   'UnsafeSchedule$' : UnsafeSchedule
define
   %% Global Helper Functions
   fun {ToAtom S}
      {String.toAtom {ByteString.toString S}}
   end
   fun {ConvertT T}
      {List.toRecord tasksR
       {Record.foldR T fun {$ '#'(S Rs) E}
			  ({ToAtom S}#{Map Rs fun {$ X} {ToAtom X} end})|E
		       end nil}}
   end
   fun {ConvertS L S}
      {List.toRecord L
       {Record.foldR S fun {$ '#'(S X) E}
			  ({ToAtom S}#X)|E
		       end nil}}
   end
   
   %% Cumulative Scheduling
   local
      fun {CallCumulative F TasksR StartR DurR UseR CapR}
	 {F
	  {ConvertT TasksR}
	  {ConvertS startR StartR}
	  {ConvertS durR DurR}
	  {ConvertS useR UseR}
	  {ConvertS capR CapR}}
	 unit
      end
   in
      fun {CumulativeFun TasksR StartR DurR UseR CapR}
	 {CallCumulative Schedule.cumulative TasksR StartR DurR UseR CapR}
      end
      fun {CumulativeEFFun TasksR StartR DurR UseR CapR}
	 {CallCumulative Schedule.cumulativeEF TasksR StartR DurR UseR CapR}
      end
      fun {CumulativeTIFun TasksR StartR DurR UseR CapR}
	 {CallCumulative Schedule.cumulativeTI TasksR StartR DurR UseR CapR}
      end
      fun {CumulativeUpFun TasksR StartR DurR UseR CapR}
	 {CallCumulative Schedule.cumulativeUp TasksR StartR DurR UseR CapR}
      end
   end

   %% Schedule Distribution
   fun {DisjointFun D1 I1 D2 I2}
      {Schedule.disjoint D1 I1 D2 I2}
      unit
   end
   fun {FirstsDistFun TasksR StartR DurR}
      {Schedule.firstsDist {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {LastsDistFun TasksR StartR DurR}
      {Schedule.lastsDist {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {FirstsLastsDistFun TasksR StartR DurR}
      {Schedule.firstsLastsDist {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {TaskIntervalsDistPFun TasksR StartR DurR}
      {Schedule.taskIntervalsDistP {ConvertT TasksR}
       {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {TaskIntervalsDistOFun TasksR StartR DurR}
      {Schedule.taskIntervalsDistO {ConvertT TasksR}
       {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   
   %% Serialized Scheduling
   fun {SerializedDisjFun TasksR StartR DurR}
      {Schedule.serializedDisj {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {SerializedFun TasksR StartR DurR}
      {Schedule.serialized {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end
   fun {TaskIntervalsFun TasksR StartR DurR}
      {Schedule.taskIntervals {ConvertT TasksR} {ConvertS startR StartR} {ConvertS durR DurR}}
   end

   %% Create Interface
   UnsafeSchedule = 'UnsafeSchedule'(cumulative: CumulativeFun
				     cumulativeEF: CumulativeEFFun
				     cumulativeTI: CumulativeTIFun
				     cumulativeUp: CumulativeUpFun
				     disjoint : DisjointFun
				     firstsDist : FirstsDistFun
				     lastsDist : LastsDistFun
				     firstsLastsDist : FirstsLastsDistFun
				     taskIntervalsDistP : TaskIntervalsDistPFun
				     taskIntervalsDistO : TaskIntervalsDistOFun
				     serializedDisj : SerializedDisjFun
				     serialized : SerializedFun
				     taskIntervals : TaskIntervalsFun)
end
