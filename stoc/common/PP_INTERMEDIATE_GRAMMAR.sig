signature PP_INTERMEDIATE_GRAMMAR =
sig
    type doc = PrettyPrint.doc
    type comp

    val ppComp : comp -> doc
end
