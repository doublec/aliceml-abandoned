structure AbstractInfo =
  struct
    type fix_info	= Source.region
    type vallab_info	= Source.region
    type typlab_info	= Source.region
    type modlab_info	= Source.region
    type inflab_info	= Source.region
    type valid_info	= Source.region
    type typid_info	= Source.region
    type varid_info	= Source.region
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
    type mod_info	= Source.region
    type inf_info	= Source.region
    type dec_info	= Source.region
    type spec_info	= Source.region
    type imp_info	= Source.region
    type ann_info	= Source.region
    type comp_info	= Source.region
  end

structure AbstractGrammar = MakeAbstractGrammar(AbstractInfo)

structure PPAbstractGrammar = MakePPAbstractGrammar(
	structure AbstractGrammar = AbstractGrammar
	fun ppInfo _		= PrettyPrint.empty
	val ppFixInfo		= ppInfo
	val ppVallabInfo	= ppInfo
	val ppTyplabInfo	= ppInfo
	val ppModlabInfo	= ppInfo
	val ppInflabInfo	= ppInfo
	val ppValidInfo		= ppInfo
	val ppTypidInfo		= ppInfo
	val ppVaridInfo		= ppInfo
	val ppModidInfo		= ppInfo
	val ppInfidInfo		= ppInfo
	val ppVallongidInfo	= ppInfo
	val ppTyplongidInfo	= ppInfo
	val ppModlongidInfo	= ppInfo
	val ppInflongidInfo	= ppInfo
	val ppExpInfo		= ppInfo
	val ppPatInfo		= ppInfo
	val ppRowInfo		= fn _ => ppInfo
	val ppFieldInfo		= fn _ => ppInfo
	val ppMatchInfo		= ppInfo
	val ppTypInfo		= ppInfo
	val ppModInfo		= ppInfo
	val ppInfInfo		= ppInfo
	val ppDecInfo		= ppInfo
	val ppSpecInfo		= ppInfo
	val ppImpInfo		= ppInfo
	val ppAnnInfo		= ppInfo
	val ppCompInfo		= ppInfo)
