structure AbstractInfo =
  struct
    type fix_info	= Source.region
    type vallab_info	= Source.region
    type typlab_info	= Source.region
    type modlab_info	= Source.region
    type inflab_info	= Source.region
    type valid_info	= Source.region
    type typid_info	= Source.region
    type modid_info	= Source.region
    type infid_info	= Source.region
    type vallongid_info	= Source.region
    type typlongid_info	= Source.region
    type modlongid_info	= Source.region
    type inflongid_info	= Source.region
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
    type imp_info	= Source.region
    type ann_info	= Source.region
    type comp_info	= Source.region
  end

structure AbstractGrammar = MakeAbstractGrammar(AbstractInfo)
