#include <stdlib.h>
#include <stdarg.h>

#define SCM_ERR_CANNOT_APPLY	50
#define SCM_ERR_WRONG_ARITY	51
#define SCM_ERR_INTERNAL	52
#define SCM_ERR_MISSING_ARGS	53
#define SCM_ERR_OUT_OF_EXTENT	59
#define SCM_ERR_CAR		60
#define SCM_ERR_CDR		61
#define SCM_ERR_SET_CAR		62
#define SCM_ERR_SET_CDR		63
#define SCM_ERR_PLUS		70
#define SCM_ERR_MINUS		71
#define SCM_ERR_EQNP  77
#define SCM_ERR_CANT_ALLOC	100

#define SCM_STACK_HIGHER <=

#define SCM_error(code) SCM_signal_error(code,__LINE__,__FILE__)

typedef union SCM_object *SCM;

#define SCM_FixnumP(x) ((unsigned long)(x) & (unsigned long)1)
#define SCM_Fixnum2int(x) ((long)(x) >> 1)
#define SCM_Int2fixnum(i) ((SCM)(((i) << 1) | 1))

union SCM_object {
  struct SCM_pair {
    SCM cdr;
    SCM car;
  } pair;

  struct SCM_string {
    char Cstring[8];
  } string;

  struct SCM_symbol {
    SCM pname;
  } symbol;

  struct SCM_box {
    SCM content;
  } box;

  struct SCM_subr {
    SCM (*behavior)();
    long arity;
  } subr;

  struct SCM_closure {
    SCM (*behavior)();
    long arity;
    SCM environment[1];
  } closure;

  struct SCM_escape {
    struct SCM_jmp_buf *stack_address;
  } escape;
};

enum SCM_tag {
  SCM_NULL_TAG = 0xaaa0,
  SCM_PAIR_TAG = 0xaaa1,
  SCM_BOOLEAN_TAG = 0xaaa2,
  SCM_UNDEFINED_TAG = 0xaaa3,
  SCM_SYMBOL_TAG = 0xaaa4,
  SCM_STRING_TAG = 0xaaa5,
  SCM_SUBR_TAG = 0xaaa6,
  SCM_CLOSURE_TAG = 0xaaa7,
  SCM_ESCAPE_TAG = 0xaaa8,
};

union SCM_header {
  enum SCM_tag tag;
  SCM ignored;
};

union SCM_unwrapped_object {
  struct SCM_unwrapped_immediate_object {
    union SCM_header header;
  } object;

  struct SCM_unwrapped_pair {
    union SCM_header header;
    SCM cdr;
    SCM car;
  } pair;

  struct SCM_unwrapped_string {
    union SCM_header header;
    char Cstring[8];
  } string;

  struct SCM_unwrapped_symbol {
    union SCM_header header;
    SCM pname;
  } symbol;

  struct SCM_unwrapped_subr {
    union SCM_header header;
    SCM (*behavior)();
    long arity;
  } subr;

  struct SCM_unwrapped_closure {
    union SCM_header header;
    SCM (*behavior)();
    long arity;
    SCM environment[1];
  } closure;

  struct SCM_unwrapped_escape {
    union SCM_header header;
    struct SCM_jmp_buf *stack_address;
  } escape;
};

typedef union SCM_unwrapped_object *SCMref;

#define SCM_Wrap(x) ((SCM) (((union SCM_header *) x) + 1))
#define SCM_Unwrap(x) ((SCMref) (((union SCM_header *) x) - 1))
#define SCM_2tag(x) ((SCM_Unwrap((SCM) x))->object.header.tag)

#define SCM_CfunctionAddress(Cfunction) ((SCM (*)(void)) Cfunction)

// 10.9.1  Declaring Values
// ========================

#define SCM_DefinePair(pair, car, cdr) \
  static struct SCM_unwrapped_pair pair = {{SCM_PAIR_TAG}, cdr, car}

#define SCM_DefineSymbol(symbol, pname) \
  static struct SCM_unwrapped_symbol symbol = {{SCM_SYMBOL_TAG}, pname}

#define SCM_DefineString(Cname, string) \
  struct Cname##_struct { \
    union SCM_header header; \
    char Cstring[1 + sizeof(string)]; \
  }; \
  static struct Cname##_struct Cname = \
    {{SCM_STRING_TAG}, string}


#define SCM_DefineImmediateObject(name, tag) \
  struct SCM_unwrapped_immediate_object name = {{tag}}

#define SCM_true SCM_Wrap(&SCM_true_object)
#define SCM_false SCM_Wrap(&SCM_false_object)
#define SCM_nil SCM_Wrap(&SCM_nil_object)

#define SCM_2bool(i) ((i) ? SCM_true : SCM_false)

#define SCM_Car(x) (SCM_Unwrap(x)->pair.car)
#define SCM_Cdr(x) (SCM_Unwrap(x)->pair.cdr)
#define SCM_NullP(x) ((x) == SCM_nil)
#define SCM_PairP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_PAIR_TAG))
#define SCM_SymbolP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_SYMBOL_TAG))
#define SCM_StringP(x) \
  ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_STRING_TAG))
#define SCM_EqP(x, y) ((x) == (y))

#define SCM_Plus(x, y) \
  ( ( SCM_FixnumP(x) && SCM_FixnumP(y)) \
    ? SCM_Int2fixnum(SCM_Fixnum2int(x) + SCM_Fixnum2int(y)) \
    : SCM_error(SCM_ERR_PLUS))

#define SCM_Minus(x, y) \
  ( ( SCM_FixnumP(x) && SCM_FixnumP(y)) \
    ? SCM_Int2fixnum(SCM_Fixnum2int(x) - SCM_Fixnum2int(y)) \
    : SCM_error(SCM_ERR_MINUS))

#define SCM_GtP(x, y) \
  ( ( SCM_FixnumP(x) && SCM_FixnumP(y)) \
    ? SCM_2bool(SCM_Fixnum2int(x) > SCM_Fixnum2int(y)) \
    : SCM_error(SCM_ERR_GTP))

#define SCM_EqnP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) == SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_EQNP) )


// 10.9.2  Global Variables
// ========================

#define SCM_CheckedGlobal(Cname) \
  ((Cname != SCM_undefined) ? Cname : SCM_error(SCM_ERR_UNINITIALIZED))

#define SCM_DefineInitializedGlobalVariable(Cname, string, value) \
  SCM Cname = SCM_Wrap(value)

#define SCM_DefineGlobalVariable(Cname, string) \
  SCM_DefineInitializedGlobalVariable(Cname, string, &SCM_undefined_object)

#define SCM_undefined SCM_Wrap(&SCM_undefined_object)


#define SCM_DefinePredefinedFunctionVariable(subr, string, arity, Cfunction) \
  static struct SCM_unwrapped_subr subr##_object = \
    {{SCM_SUBR_TAG}, Cfunction, arity}; \
  SCM_DefineInitializedGlobalVariable(subr, string, &(subr##_object))

#define SCM_Content(e) ((e)->box.content)


// 10.9.3  Defining Functions
// ==========================

#define SCM_DefineClosure(struct_name, fields) \
  struct struct_name { \
    SCM (*behavior)(void); \
    long arity; \
    fields \
  }

#define SCM_MinimalArity(i) (-(i)-1)

#define SCM_DeclareFunction(Cname) \
  SCM Cname (struct Cname *self_, unsigned long size_, \
             va_list arguments_)

#define SCM_DeclareLocalVariable(Cname, rank) \
  SCM Cname = va_arg(arguments_, SCM)

#define SCM_DeclareLocalDottedVariable(Cname, rank) \
  SCM Cname = SCM_list(size_ - rank, arguments_)

#define SCM_Free(Cname) ((*self_).Cname)

#define SCM_invoke0(f)       SCM_invoke(f,0)
#define SCM_invoke1(f,x)     SCM_invoke(f,1,x)
#define SCM_invoke2(f,x,y)   SCM_invoke(f,2,x,y)
#define SCM_invoke3(f,x,y,z) SCM_invoke(f,3,x,y,z)


////

extern SCM SCM_signal_error (unsigned long code, unsigned long line, char *file);

extern SCM SCM_cons (SCM, SCM);
extern SCM SCM_car (SCM);
extern SCM SCM_cdr (SCM);

extern SCM SCM_eqp (SCM, SCM);
extern SCM SCM_eqnp (SCM, SCM);

extern SCM SCM_invoke(SCM fun, unsigned long number, ...);
extern SCM SCM_close(SCM (*Cfunc)(void), long arity, unsigned long size, ...);
extern SCM SCM_print(SCM x);
extern SCM SCM_list (unsigned long count, va_list arguments);


extern struct SCM_unwrapped_immediate_object SCM_true_object;      /*    #t  */
extern struct SCM_unwrapped_immediate_object SCM_false_object;     /*    #f  */
extern struct SCM_unwrapped_immediate_object SCM_nil_object;       /*    ()  */
extern struct SCM_unwrapped_immediate_object SCM_undefined_object; /* #<UFO> */
