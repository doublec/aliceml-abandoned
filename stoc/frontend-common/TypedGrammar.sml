structure TypedInfo =
  struct
    type fix_info	= { region: Source.region, fix: Fixity.t }
    type lab_info	= { region: Source.region }
    type id_info	= { region: Source.region }
    type longid_info	= { region: Source.region }
    type exp_info	= { region: Source.region, typ: Type.t }
    type pat_info	= { region: Source.region, typ: Type.t }
    type 'a row_info	= { region: Source.region }
    type 'a field_info	= { region: Source.region }
    type match_info	= { region: Source.region }
    type typ_info	= { region: Source.region, typ: Type.t }
    type con_info	= { region: Source.region, typ: Type.t }
    type mod_info	= { region: Source.region, inf: Inf.t }
    type inf_info	= { region: Source.region, inf: Inf.t }
    type dec_info	= { region: Source.region }
    type spec_info	= { region: Source.region }
    type imp_info	= { region: Source.region }
    type ann_info	= { region: Source.region, sign: Inf.sign }
    type comp_info	= { region: Source.region, sign: Inf.sign }

    fun labToIdInfo i	= i
    fun idToLabInfo i	= i

    fun nonInfo r	= { region = r }
    fun fixInfo(r,f)	= { region = r, fix = f }
    fun typInfo(r,t)	= { region = r, typ = t }
    fun infInfo(r,j)	= { region = r, inf = j }
    fun sigInfo(r,s)	= { region = r, sign = s }
  end

structure TypedGrammar = MakeAbstractGrammar(TypedInfo)
