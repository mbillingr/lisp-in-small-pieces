#include "scheme.h"
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

SCM_DefineImmediateObject(SCM_true_object, SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_false_object, SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_nil_object, SCM_NULL_TAG);
SCM_DefineImmediateObject(SCM_undefined_object, SCM_UNDEFINED_TAG);

// 10.10.1  Allocation
// ==========================

SCM SCM_cons(SCM x, SCM y) {
  SCMref cell = (SCMref) malloc(sizeof(struct SCM_unwrapped_pair));
  if(cell == (SCMref) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  cell->pair.header.tag = SCM_PAIR_TAG;
  cell->pair.car = x;
  cell->pair.cdr = y;
  return SCM_Wrap(cell);
}

SCM SCM_close(SCM (*Cfunction)(void), long arity, unsigned long size, ...) {
  SCMref result = (SCMref) malloc(sizeof(struct SCM_unwrapped_closure)
                                  + (size - 1) * sizeof(SCM));
  unsigned long i;
  va_list args;
  if(result == (SCMref) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  result->closure.header.tag = SCM_CLOSURE_TAG;
  result->closure.behavior = Cfunction;
  result->closure.arity = arity;
  va_start(args, size);
  for(i=0; i<size; i++) {
    result->closure.environment[i] = va_arg(args, SCM);
  }
  va_end(args);
  return SCM_Wrap(result);
}

////

SCM SCM_signal_error (unsigned long code, unsigned long line, char *file) {
  fflush(stdout);
  fprintf(stderr,"Error %u, Line %u, File %s.\n",code,line,file);
  exit(code);
}

SCM SCM_car (SCM x) {
  if ( SCM_PairP(x) ) {
    return SCM_Car(x);
  } else return SCM_error(SCM_ERR_CAR);
}

SCM SCM_cdr (SCM x) {
  if ( SCM_PairP(x) ) {
    return SCM_Cdr(x);
  } else return SCM_error(SCM_ERR_CDR);
}

SCM SCM_set_car(SCM x, SCM y) {
  if(SCM_PairP(x)) {
    SCM_Unwrap(x)->pair.car = y;
    return x;
  } else return SCM_error(SCM_ERR_SET_CAR);
}

SCM SCM_set_cdr(SCM x, SCM y) {
  if(SCM_PairP(x)) {
    SCM_Unwrap(x)->pair.cdr = y;
    return x;
  } else return SCM_error(SCM_ERR_SET_CDR);
}

SCM SCM_list(unsigned long count, va_list arguments) {
  if(count == 0) {
    return SCM_nil;
  } else {
    SCM arg = va_arg(arguments, SCM);
    return SCM_cons(arg, SCM_list(count-1, arguments));
  }
}

SCM SCM_eqp (SCM x, SCM y) {
  return SCM_2bool(SCM_EqP(x, y));
}

SCM SCM_eqnp (SCM x, SCM y) {
  return SCM_EqnP(x, y);
}


SCM SCM_invoke(SCM function, unsigned long number, ...) {
  if ( SCM_FixnumP(function) ) {
    return SCM_error(SCM_ERR_CANNOT_APPLY); /* Cannot apply a number! */
  } else {
    switch SCM_2tag(function) {
    case SCM_SUBR_TAG: {
      SCM (*behavior)(void) = (SCM_Unwrap(function)->subr).behavior;
      long arity = (SCM_Unwrap(function)->subr).arity;
      SCM result;
      if ( arity >= 0 ) {         /* Fixed arity subr */
        if ( arity != number ) {
          return SCM_error(SCM_ERR_WRONG_ARITY); /* Wrong arity! */
        } else {
          if ( arity == 0) {
            result = behavior();
          } else {
            va_list args;
            va_start(args,number);
            { SCM a0 ;
              a0 = va_arg(args,SCM);
              if ( arity == 1 ) {
                result = ((SCM (*)(SCM)) *behavior)(a0);
              } else {
                SCM a1 ;
                a1 = va_arg(args,SCM);
                if ( arity == 2 ) {
                  result = ((SCM (*)(SCM,SCM)) *behavior)(a0,a1);
                } else {
                  SCM a2 ;
                  a2 = va_arg(args,SCM);
                  if ( arity == 3 ) {
                    result = ((SCM (*)(SCM,SCM,SCM)) *behavior)(a0,a1,a2);
                  } else {
                    /* No fixed arity subr with more than 3 variables */
                    return SCM_error(SCM_ERR_INTERNAL);
                  }
                }
              }
            }
            va_end(args);
          }
          return result;
        }
      } else {                  /* Nary subr */
        long min_arity = SCM_MinimalArity(arity) ;
        if ( number < min_arity ) {
          return SCM_error(SCM_ERR_MISSING_ARGS); /* Too less arguments! */
        } else {
          va_list args;
          SCM result;
          va_start(args,number);
          result = ((SCM (*)(unsigned long,va_list)) *behavior)(number,args);
          va_end(args);
          return result;
        }
      }
    }
    case SCM_CLOSURE_TAG: {
      SCM (*behavior)(void) = (SCM_Unwrap(function)->closure).behavior ;
      long arity = (SCM_Unwrap(function)->closure).arity ;
      SCM result;
      va_list args;
      va_start(args,number);
      if ( arity >= 0 ) {
        if ( arity != number ) { /* Wrong arity! */
          return SCM_error(SCM_ERR_WRONG_ARITY);
        } else {
          result = ((SCM (*)(SCM,unsigned long,va_list)) *behavior)(function,number,args);
        }
      } else {
        long min_arity = SCM_MinimalArity(arity) ;
        if ( number < min_arity ) {
          return SCM_error(SCM_ERR_MISSING_ARGS);     /* Too less arguments! */
        } else {
          result = ((SCM (*)(SCM,unsigned long,va_list)) *behavior)(function,number,args);
        }
      }
      va_end(args);
      return result;
    }
    /*case SCM_ESCAPE_TAG: {
      if ( number == 1) {
        va_list args;
        va_start(args,number);
        jumpvalue = va_arg(args,SCM);
        va_end(args);
        { struct SCM_jmp_buf *address =
            SCM_Unwrap(function)->escape.stack_address;
          if ( SCM_EqP(address->back_pointer,function)
              && ( (void *) &address SCM_STACK_HIGHER (void *) address ) ) {
            longjmp(address->jb,1);
          } else {
            return SCM_error(SCM_ERR_OUT_OF_EXTENT);
          }
        }
      } else {
        return SCM_error(SCM_ERR_MISSING_ARGS);
      }
    }*/
    default: {
      return SCM_error(SCM_ERR_CANNOT_APPLY);       /* Cannot apply! */
    }
    }
  }
}


SCM SCM_prin (SCM x);

void SCM_prin_list (SCM x) {
  if ( SCM_FixnumP(x) ) {
    fprintf(stdout," . %d",SCM_Fixnum2int(x));
  } else {
    switch SCM_2tag(x) {
    case SCM_NULL_TAG: {
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout," ");
      SCM_prin(SCM_car(x));
      SCM_prin_list(SCM_cdr(x));
      break;
    }
    default: {
      fprintf(stdout," . ");
      SCM_prin(x);
      break;
    }
    }
  }
}

SCM SCM_prin (SCM x) {
  if ( SCM_FixnumP(x) ) {
    fprintf(stdout,"%d",SCM_Fixnum2int(x));
  } else {
    switch SCM_2tag(x) {
    case SCM_NULL_TAG: {
      fprintf(stdout,"()");
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout,"(");
      SCM_prin(SCM_Car(x));
      SCM_prin_list(SCM_cdr(x));
      fprintf(stdout,")");
      break;
    }
    case SCM_BOOLEAN_TAG: {
      fprintf(stdout,"#%c",(SCM_EqP(x,SCM_true)?'T':'F'));
      break;
    }
    case SCM_UNDEFINED_TAG: {
      fprintf(stdout,"#<UFO>");
      break;
    }
    case SCM_SYMBOL_TAG: {
      SCM str = SCM_Unwrap(x)->symbol.pname;
      char *Cstring = SCM_Unwrap(str)->string.Cstring;
      fprintf(stdout,"%s",Cstring);
      break;
    }
    case SCM_STRING_TAG: {
      char *Cstring = SCM_Unwrap(x)->string.Cstring;
      fprintf(stdout,"\"%s\"",Cstring);
      break;
    }
    case SCM_SUBR_TAG: {
      fprintf(stdout,"#<Subr@%p>",(void *)(x));
      break;
    }
    case SCM_CLOSURE_TAG: {
      fprintf(stdout,"#<Closure@%p>",(void *)(x));
      break;
    }
    case SCM_ESCAPE_TAG: {
      fprintf(stdout,"#<Continuation@%p>",(void *)(x));
      break;
    }
    default:
      fprintf(stdout,"#<Something@%p>",(void *)(x));
      break;
    }
  }
  return (x);
}

SCM SCM_print (SCM x) {
  SCM_prin(x);
  printf("\n");
  return (x);
}
