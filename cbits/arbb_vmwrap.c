
#include <arbb_vmapi.h>
#include "arbb_vmwrap.h"

#include <stdio.h>
#include <stdlib.h>

void* arbb_wrap_get_default_context() {
  arbb_context_t context;
  arbb_get_default_context(&context, 0);
  return context.ptr;
}

void* arbb_wrap_get_scalar_type(void *context, arbb_scalar_type_t t) {
  arbb_type_t type;           
  arbb_context_t ctx; 
  ctx.ptr = context;
                         
  arbb_get_scalar_type(ctx, &type, t, 0);   

  return type.ptr;
}

void* arbb_wrap_get_binary_function_type(void *context,
					 void *in1,
					 void *in2,
					 void *out){
  arbb_context_t ctx;
  arbb_type_t inputs[2]; 
  arbb_type_t outputs[1]; 
  arbb_type_t fn_type;

  ctx.ptr = context;
  
  inputs[0].ptr = in1;
  inputs[1].ptr = in2;
  outputs[0].ptr = out;
 
  
  arbb_get_function_type(ctx, &fn_type,
			 1, outputs,      
			 2, inputs,
			 0);

  return fn_type.ptr;

} 

/* how to do this ? */
void* arbb_wrap_get_function_type(void *context, 
				  arbb_type_t* ot, 
				  arbb_type_t* it, int outs, int ins) {
  int i;
  arbb_context_t ctx; 
  arbb_type_t fn_type;
  
  ctx.ptr = context;

  arbb_get_function_type(ctx, &fn_type,
    outs, ot,      
    ins, it,
    0);

  return fn_type.ptr;
}

void *arbb_wrap_begin_function(void *context, 
			       void *fn_type, 
			       char *name) {   /* for pretty prenting only, I Think */
 arbb_function_t function;                          
 arbb_type_t t; 
 arbb_context_t ctx;

 ctx.ptr = context;
 t.ptr = fn_type;

 arbb_begin_function(ctx, &function, t, name, 0 /* is remote ? */, 0); 
									      									      
 return function.ptr;
}

/* , b, c; */ 
void *arbb_wrap_get_parameter(void *fn, int io, int n) {
  arbb_variable_t a;
  arbb_function_t function;
  
  function.ptr = fn;
 
  arbb_get_parameter(function, &a, io,  n, 0);
  
  return a.ptr;
}

void arbb_wrap_op(void *fnt, 
		   arbb_opcode_t op, 
		   arbb_variable_t *ot, 
		   arbb_variable_t *it) {
  
  arbb_function_t function;
  function.ptr = fnt;

  arbb_op(function, op, ot, it, 0, 0); 
  
}

void arbb_wrap_end_function(void *fnt) {
  arbb_function_t function; 
  function.ptr = fnt;
  arbb_end_function(function, 0);                    
}

void arbb_wrap_compile(void *fnt) {
  arbb_function_t function;
  function.ptr = fnt;
  arbb_compile(function, 0);
}

void *arbb_wrap_set_binding_null(){ 
  arbb_binding_t null_binding;
  arbb_set_binding_null(&null_binding); /* what is this? */
  
  return null_binding.ptr;
}

void *arbb_wrap_create_constant(void *ctx, 
				void *t,
				void *data) { 
  arbb_context_t context;
  arbb_type_t type;
  arbb_global_variable_t ga; 
  
  context.ptr = ctx; 
  type.ptr = t;
  arbb_create_constant(context, &ga, type, data, 0, 0); 
  
  return ga.ptr;
}

void *arbb_wrap_variable_from_global(void *ctx, void *g) {
  
  arbb_context_t context; 
  context.ptr = ctx;
  
  arbb_global_variable_t ga; 
  ga.ptr = g; 
  
  arbb_variable_t var;
  
  arbb_get_variable_from_global(context, &var, ga, 0); 

  return var.ptr;
}
/* what are the 2 zeroes about */
void *arbb_wrap_create_global(void *ctx, void *t, char *name, void *bin) {
  
  arbb_context_t context; 
  context.ptr = ctx; 
  
  arbb_type_t type;
  type.ptr = t; 
  
  arbb_binding_t null_binding;
  null_binding.ptr = bin;

  arbb_global_variable_t gc;

  arbb_create_global(context, &gc, type, name , null_binding, 0, 0); 
  
  return gc.ptr;
}

void arbb_wrap_execute(void *fnc, 
	     arbb_variable_t *out, 
	     arbb_variable_t *in){

  arbb_function_t function;
  function.ptr = fnc;
	      
  arbb_execute(function, out, in, 0);
}

float arbb_wrap_read_scalar_float(void *ctx, void *var) {
  float result;
  
  arbb_context_t context;
  context.ptr = ctx;

  arbb_variable_t out;
  out.ptr = var;

  arbb_read_scalar(context, out, &result, 0);
  return result;
}


void *arbb_wrap_serialize_function(void *fnt) {
  
  arbb_function_t function;

  function.ptr = fnt;

  arbb_string_t str; 
  arbb_serialize_function(function, &str,0);
  return str.ptr;
}


const char *arbb_wrap_get_c_string(void *str){ 
  arbb_string_t s;
  s.ptr = str;
  return arbb_get_c_string(s);
}

void arbb_wrap_free_string(void *str){ 
  arbb_string_t s; 
  s.ptr = str;
  arbb_free_string(s);
}
