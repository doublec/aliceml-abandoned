
/* Basic */
void* c_new(int bytes);
void c_delete(void* ptr);
void** pointer(void* ptr);
void* unref(void** ptr);
void* cast(void* ptr);

/* Char */
int sizeof_char();
char* cast_char(void* ptr);
char* pointer_char(char c);
char unref_char(char* pchr);

/* Short */
int sizeof_short();
short* cast_short(void* ptr);
short* pointer_short(short c);
short unref_short(short* pchr);
 
/* Int */
int sizeof_int();
int* cast_int(void* ptr);
int* pointer_int(int c);
int unref_int(int* pchr);

/* Long */
int sizeof_long();
long* cast_long(void* ptr);
long* pointer_long(long c);
long unref_long(long* pchr);
 
/* Float */
int sizeof_float();
float* cast_float(void* ptr);
float* pointer_float(float c);
float unref_float(float* pchr);

/* Double */
int sizeof_double();
double* cast_double(void* ptr);
double* pointer_double(double c);
double unref_double(double* pchr);
