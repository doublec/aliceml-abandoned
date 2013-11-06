#include "mozart.h"
#include "math.h"

class Matrix : public OZ_Extension {
public:
  double a11,a12,a13,a14;
  double a21,a22,a23,a24;
  double a31,a32,a33,a34;
  Matrix(Matrix*m)
    :OZ_Extension(),
     a11(m->a11),a12(m->a12),a13(m->a13),a14(m->a14),
     a21(m->a21),a22(m->a22),a23(m->a23),a24(m->a24),
     a31(m->a31),a32(m->a32),a33(m->a33),a34(m->a34){}
  Matrix(){};
  Matrix(double x11,double x12,double x13,double x14,
	 double x21,double x22,double x23,double x24,
	 double x31,double x32,double x33,double x34)
    :OZ_Extension(),
     a11(x11),a12(x12),a13(x13),a14(x14),
     a21(x21),a22(x22),a23(x23),a24(x24),
     a31(x31),a32(x32),a33(x33),a34(x34){}
  //
  // Extension
  //
  static int id;
  virtual int getIdV() { return id; }
  virtual OZ_Term typeV() { return OZ_atom("matrix"); }
  virtual OZ_Extension* gCollectV(void);
  virtual OZ_Extension* sCloneV(void) { Assert(0); return NULL; }
  virtual void gCollectRecurseV(void){}
  virtual void sCloneRecurseV(void){}
  virtual OZ_Term printV(int depth = 10);
};

class Vector : public OZ_Extension {
public:
  double v1,v2,v3;
  Vector(Vector*v)
    :OZ_Extension(),v1(v->v1),v2(v->v2),v3(v->v3){}
  Vector(double x1,double x2,double x3)
    :OZ_Extension(),v1(x1),v2(x2),v3(x3){}
  Vector(){};
  //
  // Extension
  //
  static int id;
  virtual int getIdV() { return id; }
  virtual OZ_Term typeV() { return OZ_atom("vector"); }
  virtual OZ_Extension* gCollectV(void);
  virtual OZ_Extension* sCloneV(void) { Assert(0); return NULL; }
  virtual void gCollectRecurseV(void){}
  virtual void sCloneRecurseV(void){}
  virtual OZ_Term printV(int depth = 10);
};

int Matrix::id;
int Vector::id;

inline int oz_isMatrix(OZ_Term t)
{
  t = OZ_deref(t);
  return OZ_isExtension(t) &&
    OZ_getExtension(t)->getIdV()==Matrix::id;
}

inline int oz_isVector(OZ_Term t)
{
  t = OZ_deref(t);
  return OZ_isExtension(t) &&
    OZ_getExtension(t)->getIdV()==Vector::id;
}

inline Matrix* tagged2Matrix(OZ_Term t)
{
  Assert(oz_isMatrix(t));
  return (Matrix*) OZ_getExtension(OZ_deref(t));
}

inline Vector* tagged2Vector(OZ_Term t)
{
  Assert(oz_isVector(t));
  return (Vector*) OZ_getExtension(OZ_deref(t));
}

OZ_Extension* Matrix::gCollectV(void)
{
  return (new Matrix(this));
}

OZ_Extension* Vector::gCollectV(void)
{
  return (new Vector(this));
}

OZ_Term Matrix::printV(int depth = 10)
{
  return OZ_mkTupleC("#",25,
		     OZ_atom("#<<"),
		     OZ_float(a11),
		     OZ_atom(","),
		     OZ_float(a12),
		     OZ_atom(","),
		     OZ_float(a13),
		     OZ_atom(","),
		     OZ_float(a14),
		     OZ_atom("><"),
		     OZ_float(a21),
		     OZ_atom(","),
		     OZ_float(a22),
		     OZ_atom(","),
		     OZ_float(a23),
		     OZ_atom(","),
		     OZ_float(a24),
		     OZ_atom("><"),
		     OZ_float(a31),
		     OZ_atom(","),
		     OZ_float(a32),
		     OZ_atom(","),
		     OZ_float(a33),
		     OZ_atom(","),
		     OZ_float(a34),
		     OZ_atom(">>"));
}

OZ_Term Vector::printV(int depth = 10)
{
  return OZ_mkTupleC("#",7,
		     OZ_atom("#<"),
		     OZ_float(v1),
		     OZ_atom(","),
		     OZ_float(v2),
		     OZ_atom(","),
		     OZ_float(v3),
		     OZ_atom(">"));
}

#define MatrixException(PROC,MSG) \
OZ_raiseC("matrix",3,OZ_atom(PROC),OZ_inAsList(),OZ_atom(MSG))

#define VectorException(PROC,MSG) \
OZ_raiseC("vector",3,OZ_atom(PROC),OZ_inAsList(),OZ_atom(MSG))

#define MatrixError(PROC,MSG) \
OZ_raiseErrorC("matrix",3,OZ_atom(PROC),OZ_inAsList(),OZ_atom(MSG))

#define VectorError(PROC,MSG) \
OZ_raiseErrorC("vector",3,OZ_atom(PROC),OZ_inAsList(),OZ_atom(MSG))

#define OZ_declareMatrix(ARG,VAR) \
OZ_declareType(ARG,VAR,Matrix*,"matrix",oz_isMatrix,tagged2Matrix)

#define OZ_declareVector(ARG,VAR) \
OZ_declareType(ARG,VAR,Vector*,"vector",oz_isVector,tagged2Vector)

OZ_BI_define(matrix_is,1,1)
{
  OZ_declareDetTerm(0,x);
  OZ_RETURN_BOOL(oz_isMatrix(x));
}
OZ_BI_end

OZ_BI_define(vector_is,1,1)
{
  OZ_declareDetTerm(0,x);
  OZ_RETURN_BOOL(oz_isVector(x));
}
OZ_BI_end

OZ_BI_define(matrix_new,12,1)
{
  OZ_declareFloat( 0,x11);
  OZ_declareFloat( 1,x12);
  OZ_declareFloat( 2,x13);
  OZ_declareFloat( 3,x14);
  OZ_declareFloat( 4,x21);
  OZ_declareFloat( 5,x22);
  OZ_declareFloat( 6,x23);
  OZ_declareFloat( 7,x24);
  OZ_declareFloat( 8,x31);
  OZ_declareFloat( 9,x32);
  OZ_declareFloat(10,x33);
  OZ_declareFloat(11,x34)

  OZ_RETURN(OZ_extension(new Matrix(x11,x12,x13,x14,
				    x21,x22,x23,x24,
				    x31,x32,x33,x34)));
}
OZ_BI_end

OZ_BI_define(vector_new,3,1)
{
  OZ_declareFloat(0,x1);
  OZ_declareFloat(1,x2);
  OZ_declareFloat(2,x3);

  OZ_RETURN(OZ_extension(new Vector(x1,x2,x3)));
}
OZ_BI_end

#define SET(V) f=m->V; goto found

OZ_BI_define(matrix_get,3,1)
{
  OZ_declareMatrix(0,m);
  OZ_declareInt(1,i);
  OZ_declareInt(2,j);

  if (i<1 || i>3 || j<1 || j>4)
    return MatrixException("matrix_get","outOfBound");

  double f;

  switch (i) {
  case 1:
    switch (j) {
    case 1: SET(a11);
    case 2: SET(a12);
    case 3: SET(a13);
    case 4: SET(a14);
    default:goto failed;
    }
  case 2:
    switch (j) {
    case 1: SET(a21);
    case 2: SET(a22);
    case 3: SET(a23);
    case 4: SET(a24);
    default:goto failed;
    }
  case 3:
    switch (j) {
    case 1: SET(a31);
    case 2: SET(a32);
    case 3: SET(a33);
    case 4: SET(a34);
    default:goto failed;
    }
  default:goto failed;
  }
 failed:
  return FAILED;
 found:
  OZ_RETURN(OZ_float(f));
}
OZ_BI_end

#undef SET
#define SET(V) f=v->V; goto found

OZ_BI_define(vector_get,2,1)
{
  OZ_declareVector(0,v);
  OZ_declareInt(1,i);

  if (i<1 || i>3)
    return VectorError("vector_get","outOfBound");

  double f;
  
  switch (i) {
  case 1: SET(v1);
  case 2: SET(v2);
  case 3: SET(v3);
  default:goto failed;
  }
 failed:
  return FAILED;
 found:
  OZ_RETURN(OZ_float(f));
}
OZ_BI_end

#undef SET
#define SET(V) m->V=f; goto success

OZ_BI_define(matrix_put,4,0)
{
  OZ_declareMatrix(0,m);
  OZ_declareInt(1,i);
  OZ_declareInt(2,j);
  OZ_declareFloat(3,f);

  if (i<1 || i>3 || j<1 || j>4)
    return MatrixException("matrix_put","outOfBound");

  switch (i) {
  case 1:
    switch (j) {
    case 1: SET(a11);
    case 2: SET(a12);
    case 3: SET(a13);
    case 4: SET(a14);
    default:goto failed;
    }
  case 2:
    switch (j) {
    case 1: SET(a21);
    case 2: SET(a22);
    case 3: SET(a23);
    case 4: SET(a24);
    default:goto failed;
    }
  case 3:
    switch (j) {
    case 1: SET(a31);
    case 2: SET(a32);
    case 3: SET(a33);
    case 4: SET(a34);
    default:goto failed;
    }
  default:goto failed;
  }
 failed:
  return FAILED;
 success:
  return PROCEED;
}
OZ_BI_end

#undef SET
#define SET(V) v->V=f; goto success

OZ_BI_define(vector_put,3,0)
{
  OZ_declareVector(0,v);
  OZ_declareInt(1,i);
  OZ_declareFloat(2,f);

  if (i<1 || i>3)
    return VectorError("vector_put","outOfBound");

  switch (i) {
  case 1: SET(v1);
  case 2: SET(v2);
  case 3: SET(v3);
  default:goto failed;
  }
 failed:
  return FAILED;
 success:
  return PROCEED;
}
OZ_BI_end

#undef SET

OZ_BI_define(matrix_mul,2,1)
{
  OZ_declareMatrix(0,m1);
  OZ_declareMatrix(1,m2);

  Matrix * m = new Matrix
    (m1->a11*m2->a11 + m1->a12*m2->a21 + m1->a13*m2->a31,
     m1->a11*m2->a12 + m1->a12*m2->a22 + m1->a13*m2->a32,
     m1->a11*m2->a13 + m1->a12*m2->a23 + m1->a13*m2->a33,
     m1->a11*m2->a14 + m1->a12*m2->a24 + m1->a13*m2->a34 + m1->a14,
     m1->a21*m2->a11 + m1->a22*m2->a21 + m1->a23*m2->a31,
     m1->a21*m2->a12 + m1->a22*m2->a22 + m1->a23*m2->a32,
     m1->a21*m2->a13 + m1->a22*m2->a23 + m1->a23*m2->a33,
     m1->a21*m2->a14 + m1->a22*m2->a24 + m1->a23*m2->a34 + m1->a24,
     m1->a31*m2->a11 + m1->a32*m2->a21 + m1->a33*m2->a31,
     m1->a31*m2->a12 + m1->a32*m2->a22 + m1->a33*m2->a32,
     m1->a31*m2->a13 + m1->a32*m2->a23 + m1->a33*m2->a33,
     m1->a31*m2->a14 + m1->a32*m2->a24 + m1->a33*m2->a34 + m1->a34);

  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_rotz,1,1)
{
  OZ_declareFloat(0,a);
  double cosa = cos(a);
  double sina = sin(a);
  Matrix * m = new Matrix(cosa, -sina, 0.0, 0.0,
			  sina, cosa , 0.0, 0.0,
			  0.0 , 0.0  , 1.0, 0.0);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_roty,1,1)
{
  OZ_declareFloat(0,a);
  double sina = sin(a);
  double cosa = cos(a);
  Matrix * m = new Matrix(cosa , 0.0, sina, 0.0,
			  0.0  , 1.0,  0.0, 0.0,
			  -sina, 0.0, cosa, 0.0);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_rotx,1,1)
{
  OZ_declareFloat(0,a);
  double sina = sin(a);
  double cosa = cos(a);
  Matrix * m = new Matrix(1.0,  0.0,   0.0, 0.0,
			  0.0, cosa, -sina, 0.0,
			  0.0, sina,  cosa, 0.0);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_uscale,1,1)
{
  OZ_declareFloat(0,s);
  Matrix * m = new Matrix(  s, 0.0, 0.0, 0.0,
			  0.0,   s, 0.0, 0.0,
			    0.0, 0.0,   s, 0.0);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_scale,3,1)
{
  OZ_declareFloat(0,sx);
  OZ_declareFloat(1,sy);
  OZ_declareFloat(2,sz);
  Matrix * m = new Matrix( sx, 0.0, 0.0, 0.0,
			  0.0,  sy, 0.0, 0.0,
			   0.0, 0.0,  sz, 0.0);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

OZ_BI_define(matrix_translation,3,1)
{
  OZ_declareFloat(0,tx);
  OZ_declareFloat(1,ty);
  OZ_declareFloat(2,tz);
  Matrix * m = new Matrix(1.0, 0.0, 0.0, tx,
			  0.0, 1.0, 0.0, ty,
			  0.0, 0.0, 1.0, tz);
  OZ_RETURN(OZ_extension(m));
}
OZ_BI_end

inline double absVec(Vector* v)
{
  return sqrt(v->v1*v->v1 + v->v2*v->v2 + v->v3*v->v3);
}

OZ_BI_define(vector_abs,1,1)
{
  OZ_declareVector(0,v);
  OZ_RETURN(OZ_float(absVec(v)));
}
OZ_BI_end

OZ_BI_define(vector_normalize,1,1)
{
  OZ_declareVector(0,v);
  double k = 1.0 / absVec(v);
  Vector * r = new Vector(k*v->v1,k*v->v2,k*v->v3);
  OZ_RETURN(OZ_extension(r));
}
OZ_BI_end

OZ_BI_define(matrix_point_mul,2,1)
{
  OZ_declareMatrix(0,m);
  OZ_declareVector(1,v);
  Vector * r = new Vector
    (m->a11*v->v1 + m->a12*v->v2 + m->a13*v->v3 + m->a14,
     m->a21*v->v1 + m->a22*v->v2 + m->a23*v->v3 + m->a24,
     m->a31*v->v1 + m->a32*v->v2 + m->a33*v->v3 + m->a34);
  OZ_RETURN(OZ_extension(r));
}
OZ_BI_end

OZ_BI_define(matrix_vector_mul,2,1)
{
  OZ_declareMatrix(0,m);
  OZ_declareVector(1,v);
  Vector * r = new Vector
    (m->a11*v->v1 + m->a12*v->v2 + m->a13*v->v3,
     m->a21*v->v1 + m->a22*v->v2 + m->a23*v->v3,
     m->a31*v->v1 + m->a32*v->v2 + m->a33*v->v3);
  OZ_RETURN(OZ_extension(r));
}
OZ_BI_end

OZ_BI_define(vector_scal_mul,2,1)
{
  OZ_declareFloat(0,y);
  OZ_declareVector(1,x);
  Vector * v = new Vector
    (x->v1*y,x->v2*y,x->v3*y);
  OZ_RETURN(OZ_extension(v));
}
OZ_BI_end

OZ_BI_define(vector_dot_prod,2,1)
{
  OZ_declareVector(0,x);
  OZ_declareVector(1,y);
  OZ_RETURN(OZ_float(x->v1*y->v1+x->v2*y->v2+x->v3*y->v3));
}
OZ_BI_end

OZ_BI_define(vector_sub,2,1)
{
  OZ_declareVector(0,x);
  OZ_declareVector(1,y);
  Vector * v = new Vector
    (x->v1-y->v1,x->v2-y->v2,x->v3-y->v3);
  OZ_RETURN(OZ_extension(v));
}
OZ_BI_end

OZ_BI_define(vector_add,2,1)
{
  OZ_declareVector(0,x);
  OZ_declareVector(1,y);
  Vector * v = new Vector
    (x->v1+y->v1,x->v2+y->v2,x->v3+y->v3);
  OZ_RETURN(OZ_extension(v));
}
OZ_BI_end

OZ_BI_define(vector_rez,1,1)
{
  OZ_declareVector(0,x);
  Vector * v = new Vector(1.0/x->v1,1.0/x->v2,1.0/x->v3);
  OZ_RETURN(OZ_extension(v));
}
OZ_BI_end

OZ_BI_define(vector_neg,1,1)
{
  OZ_declareVector(0,x);
  Vector * v = new Vector(- x->v1,- x->v2,- x->v3);
  OZ_RETURN(OZ_extension(v));
}
OZ_BI_end

OZ_C_proc_interface * oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"matrix_is",1,1,matrix_is},
    {"vector_is",1,1,vector_is},
    {"matrix_new",12,1,matrix_new},
    {"vector_new",3,1,vector_new},
    {"matrix_get",3,1,matrix_get},
    {"vector_get",2,1,vector_get},
    {"matrix_put",4,0,matrix_put},
    {"vector_put",3,0,vector_put},
    {"matrix_mul",2,1,matrix_mul},
    {"matrix_rotz",1,1,matrix_rotz},
    {"matrix_roty",1,1,matrix_roty},
    {"matrix_rotx",1,1,matrix_rotx},
    {"matrix_uscale",1,1,matrix_uscale},
    {"matrix_scale",3,1,matrix_scale},
    {"matrix_translation",3,1,matrix_translation},
    {"vector_abs",1,1,vector_abs},
    {"vector_normalize",1,1,vector_normalize},
    {"matrix_point_mul",2,1,matrix_point_mul},
    {"matrix_vector_mul",2,1,matrix_vector_mul},
    {"vector_scal_mul",2,1,vector_scal_mul},
    {"vector_dot_prod",2,1,vector_dot_prod},
    {"vector_sub",2,1,vector_sub},
    {"vector_add",2,1,vector_add},
    {"vector_rez",1,1,vector_rez},
    {"vector_neg",1,1,vector_neg},
    {0,0,0,0}
  };
  Matrix::id = oz_newUniqueId();
  Vector::id = oz_newUniqueId();
  return table;
}

char oz_module_name[] = "Geometry";

