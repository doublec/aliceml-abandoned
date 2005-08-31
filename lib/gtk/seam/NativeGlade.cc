#include <glade/glade.h>
#include "MyNativeAuthoring.hh"
#include "NativeUtils.hh"

#define DECLARE_OPT_CSTRING(s, x) \
    const char *s = 0;  \
    if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }\
    if (Store::WordToInt (x) == Types::NONE) { \
        s = 0; \
    } else { \
        TagVal *tv = TagVal::FromWord (x); \
        Assert(tv->GetTag () == Types::SOME); \
        DECLARE_STRING(str##__tmp, tv->Sel(0)); \
        s = str##__tmp->ExportC();      \
    }
        
        
        
DEFINE3(LibGlade_load) {
    DECLARE_OPT_CSTRING(domain, x0);
    DECLARE_CSTRING(filename, x1);
    DECLARE_OPT_CSTRING(root, x2);

    GladeXML *glade = glade_xml_new_with_domain (filename, root, domain);
    if (glade == 0) {
        RAISE_CORE_ERROR("Glade: load failure");
    }

    RETURN(OBJECT_TO_WORD(glade, TYPE_G_OBJECT, GLADE_TYPE_XML));
} END


DEFINE2(LibGlade_getWidget) {
    DECLARE_OBJECT_OF_TYPE(g, x0, GLADE_TYPE_XML);
    DECLARE_CSTRING(name, x1);
    
    GladeXML *glade = (GladeXML*)g;
    GtkWidget *ob = glade_xml_get_widget (glade, name);
    if (ob == 0) {
        RAISE_CORE_ERROR("Glade: getWidget failed");
    } 
    
    RETURN(OBJECT_TO_WORD(ob, TYPE_GTK_OBJECT));
} END



static void connect_fun (const gchar *handler, GObject *ob, const gchar *sig_name,
        const gchar *signal_data, GObject *con_ob, gboolean after, gpointer data) {
    word *res = (word*)data;
    res[0] = NativeCore_SignalConnect (ob, (char*)sig_name, after == TRUE);
    res[1] = OBJECT_TO_WORD(ob, TYPE_G_OBJECT);
}


DEFINE2(LibGlade_connect) {
    DECLARE_OBJECT_OF_TYPE(g, x0, GLADE_TYPE_XML);
    DECLARE_CSTRING(name, x1);
    word res[2];
    glade_xml_signal_connect_full ((GladeXML*)g, name, connect_fun, res);
    RETURN2(res[0], res[1]);
} END

word NativeGlade_CreateComponent () {
    Record *record = Record::New (3);
    
    INIT_STRUCTURE(record, "NativeLibs.NativeGlade", "load", LibGlade_load, 3);
    INIT_STRUCTURE(record, "NativeLibs.NativeGlade", "getWidget", LibGlade_getWidget, 2);
    INIT_STRUCTURE_N(record, "NativeLibs.NativeGlade", "connect", LibGlade_connect, 2, 2);
    return record->ToWord ();
}

