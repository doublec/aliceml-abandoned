#include "NativeUtils.hh"
word NativeCore_CreateComponent();
word NativeGLib_CreateComponent();
word NativePango_CreateComponent();
word NativeAtk_CreateComponent();
word NativeGdk_CreateComponent();
word NativeGtk_CreateComponent();
word NativeCanvas_CreateComponent();
word InitComponent () {
    Record *record = Record::New (2);
    record->Init ("NativeCore$", NativeCore_CreateComponent());
    record->Init ("NativeGLib$", NativeGLib_CreateComponent());
    record->Init ("NativePango$", NativePango_CreateComponent());
    record->Init ("NativeAtk$", NativeAtk_CreateComponent());
    record->Init ("NativeGdk$", NativeGdk_CreateComponent());
    record->Init ("NativeGtk$", NativeGtk_CreateComponent());
    record->Init ("NativeCanvas$", NativeCanvas_CreateComponent());
    RETURN_STRUCTURE("NativeLibs$", record);
}
