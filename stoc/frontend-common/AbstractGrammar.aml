structure AbstractInfo =
  struct
    type lab_info	= Source.region
    type id_info	= Source.region
    type longid_info	= Source.region
    type exp_info	= Source.region
    type pat_info	= Source.region
    type 'a row_info	= Source.region
    type 'a field_info	= Source.region
    type match_info	= Source.region
    type typ_info	= Source.region
    type con_info	= Source.region
    type mod_info	= Source.region
    type inf_info	= Source.region
    type dec_info	= Source.region
    type spec_info	= Source.region
    type comp_info	= Source.region
    type imp_info	= Source.region

    fun labToIdInfo r	= r
    fun idToLabInfo r	= r
  end

structure AbstractGrammar = MakeAbstractGrammar(AbstractInfo)
