signature PP_ABSTRACT_GRAMMAR =
sig
    type doc = PrettyPrint.doc
    type comp

    val ppComp : comp -> doc
end
