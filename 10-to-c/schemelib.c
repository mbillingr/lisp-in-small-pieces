#include "scheme.h"

SCM_DefineInitializedGlobalVariable(NIL, "NIL", &SCM_nil_object);
SCM_DefineInitializedGlobalVariable(F, "F", &SCM_false_object);
SCM_DefineInitializedGlobalVariable(T, "T", &SCM_true_object);

SCM_DefinePredefinedFunctionVariable(CAR, "CAR", 1, SCM_car);
SCM_DefinePredefinedFunctionVariable(CDR, "CDR", 1, SCM_cdr);
SCM_DefinePredefinedFunctionVariable(CONS, "CONS", 2, SCM_cons);
SCM_DefinePredefinedFunctionVariable(EQN, "=", 2, SCM_eqnp);
SCM_DefinePredefinedFunctionVariable(EQ, "EQ?", 2, SCM_eqp);
