



void* arbb_wrap_get_default_context();
void* arbb_wrap_get_scalar_type(void *context, arbb_scalar_type_t t);

void* arbb_wrap_get_binary_function_type(void *context,
					 void *in1,
					 void *in2,
					 void *out);
/*
void* arbb_wrap_get_function_type(void *context, 
                                  void* ot[], 
				  void* it[], 
				  int ins, 
				  int outs);
*/

void fun1(); 

void fun2(void *);

void fun3(void *, void *);

void fun4(void *, void *, void *);
