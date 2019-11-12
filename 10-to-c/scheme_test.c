/* Compiler to C $Revision: 4.1 $
*/

#include "scheme.h"

/* Quotations: */
SCM_DefineString(thing4_object,"result");
#define thing4 SCM_Wrap(&thing4_object)
SCM_DefineSymbol(thing3_object,thing4);    /* result */
#define thing3 SCM_Wrap(&thing3_object)
#define thing2 SCM_nil /* '() */
#define thing1 SCM_Int2fixnum(20)
#define thing0 SCM_Int2fixnum(10)

/* Functions: */
SCM_DefineClosure(function_0, );

SCM_DeclareFunction(function_0) {
SCM_DeclareLocalDottedVariable(x,0);
return x;
}

SCM_DefineClosure(function_1, );

SCM_DeclareFunction(function_1) {
SCM y_5; SCM x_4; SCM list_3;
return (list_3=SCM_close(SCM_CfunctionAddress(function_0),-1,0),
x_4=thing0,
y_5=thing1,
SCM_invoke3(list_3,
SCM_Minus(x_4,
y_5),
thing2,
thing3));
}


/* Expression: */
void main(void) {
  SCM_print(SCM_invoke0(SCM_close(SCM_CfunctionAddress(function_1),0,0)));
  exit(0);
}

/* End of generated code. */
