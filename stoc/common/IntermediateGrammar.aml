structure IntermediateInfo =
  struct
    type lab_info	= { region: Source.region }
    type id_info	= { region: Source.region }
    type longid_info	= { region: Source.region }
    type exp_info	= { region: Source.region, typ: Type.t }
    type pat_info	= { region: Source.region, typ: Type.t }
    type 'a field_info	= { region: Source.region }
    type match_info	= { region: Source.region }
    type dec_info	= { region: Source.region }
  end

structure IntermediateGrammar = MakeIntermediateGrammar(open IntermediateInfo
							type sign = unit)
