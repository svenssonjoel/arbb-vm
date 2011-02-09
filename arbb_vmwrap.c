
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


/* -------------------------------------------------------------------------
   Experiments
   ----------------------------------------------------------------------- */

void fun4(void *ctx, void *t, void *fnt) {
  arbb_context_t context;                /* what is a context? */
  context.ptr = ctx;
 
  arbb_type_t type;
  type.ptr = t;
  
  arbb_type_t fn_type;
  fn_type.ptr = fnt;

  arbb_function_t function;                         
  {

     /*  It seems you do not need to name the function (replace "add" by NULL, still works)  */
    arbb_begin_function(context, &function, fn_type, "add", 0, 0); /* float32 add(float32, float32)  */
      arbb_variable_t a, b, c;                                     
      enum { is_input, is_output };  /* is_input = 0, is_output = 1 */
      arbb_get_parameter(function, &a, is_input,  0, 0);           
      arbb_get_parameter(function, &b, is_input,  1, 0);     /* 1 is the position in the list of inputs  */
      arbb_get_parameter(function, &c, is_output, 0, 0);

      arbb_variable_t in[] = { a, b };                       /* where are inputs */
      arbb_variable_t out[] = { c };                         /* where is output */
      arbb_op(function, arbb_op_add, out, in, 0, 0);         /* perform an operation! */
    arbb_end_function(function, 0);                          /* } */ 
  }

  arbb_compile(function, 0);


  /* -------------------------------------------------------------------------- 

     -------------------------------------------------------------------------- */
   
  float data[] = { 20.0f, 140.0f };  
  /* int data[] = { 0x0f00ffff, 0x0f00ffff}; */
  arbb_binding_t null_binding;          /* what is this? */ 
  arbb_set_binding_null(&null_binding); /* what is this? */
 
  arbb_global_variable_t ga, gb, gc;    
  arbb_create_constant(context, &ga, type, data + 0, 0, 0); 
  arbb_create_constant(context, &gb, type, data + 1, 0, 0);  /* "Transferring" data to arbb "unit" ? */

  arbb_variable_t in[2];
  arbb_get_variable_from_global(context, in + 0, ga, 0); 
  arbb_get_variable_from_global(context, in + 1, gb, 0); /* hook constants to inputs */

  arbb_variable_t out[1];

    /* The name can be NULL here as well and nothing breaks */
  arbb_create_global(context, &gc, type, "result" , null_binding, 0, 0); /* this on is named ... */
  arbb_get_variable_from_global(context, out + 0, gc, 0);

  arbb_execute(function, out, in, 0); /* run the computation */


  float result = 0;
  arbb_read_scalar(context, out[0], &result, 0);
  printf("%f\n", result);

}


void fun3(void *ctx, void *t) {
  arbb_context_t context;                /* what is a context? */
  context.ptr = ctx;
  /* arbb_get_default_context(&context, 0);*/
  
  arbb_type_t type;
  type.ptr = t;
  //arbb_get_scalar_type(context, &type, arbb_f32, 0);  
  
  arbb_type_t inputs[]  = { type, type };              /* function input list specification   */
  arbb_type_t outputs[] = { type };                    /* function output specification       */
  
  arbb_type_t fn_type;
  arbb_get_function_type(context, &fn_type,
    sizeof(outputs) / sizeof(*outputs), outputs,       /* \name -> float32 name(float32, float32)   */
    sizeof(inputs)  / sizeof(*inputs), inputs,
    0);

  arbb_function_t function;                         
  {

     /*  It seems you do not need to name the function (replace "add" by NULL, still works)  */
    arbb_begin_function(context, &function, fn_type, "add", 0, 0); /* float32 add(float32, float32)  */
      arbb_variable_t a, b, c;                                     
      enum { is_input, is_output };  /* is_input = 0, is_output = 1 */
      arbb_get_parameter(function, &a, is_input,  0, 0);           
      arbb_get_parameter(function, &b, is_input,  1, 0);     /* 1 is the position in the list of inputs  */
      arbb_get_parameter(function, &c, is_output, 0, 0);

      arbb_variable_t in[] = { a, b };                       /* where are inputs */
      arbb_variable_t out[] = { c };                         /* where is output */
      arbb_op(function, arbb_op_add, out, in, 0, 0);         /* perform an operation! */
    arbb_end_function(function, 0);                          /* } */ 
  }

  arbb_compile(function, 0);


  /* -------------------------------------------------------------------------- 

     -------------------------------------------------------------------------- */
   
  float data[] = { 20.0f, 140.0f };  
  /* int data[] = { 0x0f00ffff, 0x0f00ffff}; */
  arbb_binding_t null_binding;          /* what is this? */ 
  arbb_set_binding_null(&null_binding); /* what is this? */
 
  arbb_global_variable_t ga, gb, gc;    
  arbb_create_constant(context, &ga, type, data + 0, 0, 0); 
  arbb_create_constant(context, &gb, type, data + 1, 0, 0);  /* "Transferring" data to arbb "unit" ? */

  arbb_variable_t in[2];
  arbb_get_variable_from_global(context, in + 0, ga, 0); 
  arbb_get_variable_from_global(context, in + 1, gb, 0); /* hook constants to inputs */

  arbb_variable_t out[1];

    /* The name can be NULL here as well and nothing breaks */
  arbb_create_global(context, &gc, type, "result" , null_binding, 0, 0); /* this on is named ... */
  arbb_get_variable_from_global(context, out + 0, gc, 0);

  arbb_execute(function, out, in, 0); /* run the computation */


  float result = 0;
  arbb_read_scalar(context, out[0], &result, 0);
  printf("%f\n", result);

}


void fun2(void *ptr) {
  arbb_context_t context;                /* what is a context? */
  context.ptr = ptr;
  /* arbb_get_default_context(&context, 0);*/
  
  arbb_type_t type;                                    /*                                     */
  arbb_get_scalar_type(context, &type, arbb_f32, 0);   /*  float32 type;                      */
  
  arbb_type_t inputs[]  = { type, type };              /* function input list specification   */
  arbb_type_t outputs[] = { type };                    /* function output specification       */
  
  arbb_type_t fn_type;
  arbb_get_function_type(context, &fn_type,
    sizeof(outputs) / sizeof(*outputs), outputs,       /* \name -> float32 name(float32, float32)   */
    sizeof(inputs)  / sizeof(*inputs), inputs,
    0);

  arbb_function_t function;                         
  {

     /*  It seems you do not need to name the function (replace "add" by NULL, still works)  */
    arbb_begin_function(context, &function, fn_type, "add", 0, 0); /* float32 add(float32, float32)  */
      arbb_variable_t a, b, c;                                     
      enum { is_input, is_output };  /* is_input = 0, is_output = 1 */
      arbb_get_parameter(function, &a, is_input,  0, 0);           
      arbb_get_parameter(function, &b, is_input,  1, 0);     /* 1 is the position in the list of inputs  */
      arbb_get_parameter(function, &c, is_output, 0, 0);

      arbb_variable_t in[] = { a, b };                       /* where are inputs */
      arbb_variable_t out[] = { c };                         /* where is output */
      arbb_op(function, arbb_op_add, out, in, 0, 0);         /* perform an operation! */
    arbb_end_function(function, 0);                          /* } */ 
  }

  arbb_compile(function, 0);


  /* -------------------------------------------------------------------------- 

     -------------------------------------------------------------------------- */
   
  float data[] = { 20.0f, 140.0f };  
  /* int data[] = { 0x0f00ffff, 0x0f00ffff}; */
  arbb_binding_t null_binding;          /* what is this? */ 
  arbb_set_binding_null(&null_binding); /* what is this? */
 
  arbb_global_variable_t ga, gb, gc;    
  arbb_create_constant(context, &ga, type, data + 0, 0, 0); 
  arbb_create_constant(context, &gb, type, data + 1, 0, 0);  /* "Transferring" data to arbb "unit" ? */

  arbb_variable_t in[2];
  arbb_get_variable_from_global(context, in + 0, ga, 0); 
  arbb_get_variable_from_global(context, in + 1, gb, 0); /* hook constants to inputs */

  arbb_variable_t out[1];

    /* The name can be NULL here as well and nothing breaks */
  arbb_create_global(context, &gc, type, "result" , null_binding, 0, 0); /* this on is named ... */
  arbb_get_variable_from_global(context, out + 0, gc, 0);

  arbb_execute(function, out, in, 0); /* run the computation */


  float result = 0;
  arbb_read_scalar(context, out[0], &result, 0);
  printf("%f\n", result);

}



/* fun1. initial test of linking against arbb, tbb, */ 
void fun1() {
  arbb_context_t context;                /* what is a context? */
  arbb_get_default_context(&context, 0); /* are there other "contexts" than default? */ 
  
  arbb_type_t type;                                    /*                                     */
  arbb_get_scalar_type(context, &type, arbb_f32, 0);   /*  float32 type;                      */
  
  arbb_type_t inputs[]  = { type, type };              /* function input list specification   */
  arbb_type_t outputs[] = { type };                    /* function output specification       */
  
  arbb_type_t fn_type;
  arbb_get_function_type(context, &fn_type,
    sizeof(outputs) / sizeof(*outputs), outputs,       /* \name -> float32 name(float32, float32)   */
    sizeof(inputs)  / sizeof(*inputs), inputs,
    0);

  arbb_function_t function;                         
  {

     /*  It seems you do not need to name the function (replace "add" by NULL, still works)  */
    arbb_begin_function(context, &function, fn_type, "add", 0, 0); /* float32 add(float32, float32)  */
      arbb_variable_t a, b, c;                                     
      enum { is_input, is_output };  /* is_input = 0, is_output = 1 */
      arbb_get_parameter(function, &a, is_input,  0, 0);           
      arbb_get_parameter(function, &b, is_input,  1, 0);     /* 1 is the position in the list of inputs  */
      arbb_get_parameter(function, &c, is_output, 0, 0);

      arbb_variable_t in[] = { a, b };                       /* where are inputs */
      arbb_variable_t out[] = { c };                         /* where is output */
      arbb_op(function, arbb_op_add, out, in, 0, 0);         /* perform an operation! */
    arbb_end_function(function, 0);                          /* } */ 
  }

  arbb_compile(function, 0);


  /* -------------------------------------------------------------------------- 

     -------------------------------------------------------------------------- */
   
  float data[] = { 20.0f, 140.0f };  
  /* int data[] = { 0x0f00ffff, 0x0f00ffff}; */
  arbb_binding_t null_binding;          /* what is this? */ 
  arbb_set_binding_null(&null_binding); /* what is this? */
 
  arbb_global_variable_t ga, gb, gc;    
  arbb_create_constant(context, &ga, type, data + 0, 0, 0); 
  arbb_create_constant(context, &gb, type, data + 1, 0, 0);  /* "Transferring" data to arbb "unit" ? */

  arbb_variable_t in[2];
  arbb_get_variable_from_global(context, in + 0, ga, 0); 
  arbb_get_variable_from_global(context, in + 1, gb, 0); /* hook constants to inputs */

  arbb_variable_t out[1];

    /* The name can be NULL here as well and nothing breaks */
  arbb_create_global(context, &gc, type, "result" , null_binding, 0, 0); /* this on is named ... */
  arbb_get_variable_from_global(context, out + 0, gc, 0);

  arbb_execute(function, out, in, 0); /* run the computation */


  float result = 0;
  arbb_read_scalar(context, out[0], &result, 0);
  printf("%f\n", result);

}


/* 
  #Are the names (of function and result) only for pretty printing ? 
     if yes skip that functionality from Haskell side ?
     
  #What is the set_binding_null thing ?
  
  #Are there other contexts than default?

  #Why is everything in the vm api a void pointer wrapped in a struct ? 
*/
