



void* arbb_wrap_get_default_context();
void* arbb_wrap_get_scalar_type(void *context, arbb_scalar_type_t t);

void* arbb_wrap_get_binary_function_type(void *context,
					 void *in1,
					 void *in2,
					 void *out);


void* arbb_wrap_get_function_type(void *context, 
				  arbb_type_t* ot, 
				  arbb_type_t* it, int, int);


void *arbb_wrap_begin_function(void *context, 
			       void *fn_type, 
			       char *name); 


void *arbb_wrap_get_parameter(void *fn, int io, int n); 

void arbb_wrap_op(void *fnt, 
		  arbb_opcode_t op, 
		  arbb_variable_t *ot, 
		  arbb_variable_t *it);

void arbb_wrap_end_function(void *);

void arbb_wrap_compile(void *);

void *arbb_wrap_set_binding_null(); 

void *arbb_wrap_create_constant(void *ctx, 
				void *t,
				void *data);

void *arbb_wrap_variable_from_global(void *ctx, void *g);

void *arbb_wrap_create_global(void *ctx, void *t, char *name, void *bin);

void arbb_wrap_execute(void *fnc, 
		       arbb_variable_t *out, 
		       arbb_variable_t *in);

float arbb_wrap_read_scalar_float(void *ctx, void *var);


void *arbb_wrap_serialize_function(void *fnt);

const char *arbb_wrap_get_c_string(void *str);

void arbb_wrap_free_string(void *str); 
