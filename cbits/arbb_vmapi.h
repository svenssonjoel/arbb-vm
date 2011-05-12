/****
***** Copyright 2010-2011 Intel Corporation All Rights Reserved.
*****
***** The source code, information and material contained herein are owned by Intel Corporation or its suppliers  *****
***** or licensors, and title to such Material remains with Intel Corporation or its suppliers or licensors.      *****
***** The Material contains proprietary information of Intel or its suppliers and licensors. The Material is      *****
***** protected by worldwide copyright laws and treaty provisions. No part of the Material may be used, copied,   *****
***** reproduced, modified, published, uploaded, posted, transmitted, distributed or disclosed in any way without *****
***** Intel's prior express written permission.
*****
***** No license under any patent, copyright or other intellectual property rights in the material is granted to  *****
***** or conferred upon you, either expressly, by implication, inducement, estoppel or otherwise. Any license     *****
***** under such intellectual property rights must be express and approved by Intel in writing.
****/

#ifndef ARBB_VM_API_H
#define ARBB_VM_API_H

#if defined(_WIN32)
#  if defined(ARBB_DLL_EXPORTS)
#    define ARBB_VM_EXPORT __declspec(dllexport)
#  else
#    define ARBB_VM_EXPORT __declspec(dllimport)
#  endif
#else
#  if defined(ARBB_DLL_EXPORTS)
#    define ARBB_VM_EXPORT __attribute__((visibility("default")))
#  else
#    define ARBB_VM_EXPORT
#  endif
#endif

#if defined(_WIN32) && !defined(__GNUC__)
  typedef __int64 int64_t;
  typedef unsigned __int64 uint64_t;
  typedef __int32 int32_t;
  typedef unsigned __int32 uint32_t;
#else
  #include <stdint.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/// @addtogroup arbb_virtual_machine
/// @{

/// @defgroup arbb_virtual_machine_opaque_types Opaque Types
/// Opaque types provided by the virtual machine API.
/// @{

/// A context within which VM calls are being made.
typedef void* arbb_context_t;

/// A reference-countable object in the VM API.
/// @see @ref arbb_virtual_machine_refcounting
typedef void* arbb_refcountable_t;

/// A structure representing detailed error information from a function call.
///
/// @see @ref arbb_virtual_machine_errors
typedef void* arbb_error_details_t;

/// A global variable or a constant.
/// This structure can be converted to an arbb_refcountable_t.
typedef void* arbb_global_variable_t;

/// A variable in the VM.
typedef void* arbb_variable_t;

/// A function in the VM.
///
/// @see @ref arbb_virtual_machine_functions
typedef void* arbb_function_t;

/// A type for a variable or a function.
typedef void* arbb_type_t;

/// A binding specification to indicate a binding between the host data and global variables.
///
/// @see @ref arbb_virtual_machine_binding
typedef void* arbb_binding_t;

/// A string returned by the VM API.
typedef void* arbb_string_t;

/// A C/C++ stack trace.
/// @see arbb_cxx_store_stack_trace, arbb_cxx_release_stack_trace, arbb_cxx_get_frame_count
typedef void* arbb_cxx_stack_trace_t;

/// A frame in a C/C++ stack trace.
/// @see arbb_cxx_get_frame, arbb_cxx_get_frame_property
typedef void* arbb_cxx_frame_t;

/// A set of associated key-value pairs that may be attached to Intel&reg; ArBB basic
/// block statements or function definitions.
/// @see arbb_create_attribute_map
typedef void* arbb_attribute_map_t;

/// Generic opaque type used to represent any opaque type. 
typedef void* arbb_opaque_type_t;

/// The set of the opaque types used by the VM API.
typedef enum {
  arbb_opaque_context,
  arbb_opaque_refcountable,
  arbb_opaque_error_details,
  arbb_opaque_global_variable,
  arbb_opaque_variable,
  arbb_opaque_function,
  arbb_opaque_type,
  arbb_opaque_binding,
  arbb_opaque_string,
  arbb_opaque_cxx_stack_trace,
  arbb_opaque_cxx_frame,
  arbb_opaque_attribute_map
} arbb_opaque_tag_t;

/// @}

/// @defgroup arbb_virtual_machine_null_function Null Object Functions
/// @{

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_refcountable_null(arbb_refcountable_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_refcountable_null(arbb_refcountable_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_error_details_null(arbb_error_details_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_error_details_null(arbb_error_details_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_string_null(arbb_string_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_string_null(arbb_string_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_context_null(arbb_context_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_context_null(arbb_context_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_function_null(arbb_function_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_function_null(arbb_function_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_variable_null(arbb_variable_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_variable_null(arbb_variable_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_global_variable_null(arbb_global_variable_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_global_variable_null(arbb_global_variable_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_binding_null(arbb_binding_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_binding_null(arbb_binding_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_type_null(arbb_type_t object);

/// Sets @p *object to be a null object.
ARBB_VM_EXPORT
void arbb_set_type_null(arbb_type_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_cxx_is_stack_trace_null(arbb_cxx_stack_trace_t object);

/// Sets @p *object to be a null object
ARBB_VM_EXPORT
void arbb_cxx_set_stack_trace_null(arbb_cxx_stack_trace_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_cxx_is_frame_null(arbb_cxx_frame_t object);

/// Sets @p *object to be a null object
ARBB_VM_EXPORT
void arbb_cxx_set_frame_null(arbb_cxx_frame_t* object);

/// Returns 1 if @p object is a null object; otherwise, returns 0.
/// @return 1 if @p object is a null object, 0 otherwise.
ARBB_VM_EXPORT
int arbb_is_attribute_map_null(arbb_attribute_map_t object);

/// Sets @p *object to be a null object
ARBB_VM_EXPORT
void arbb_set_attribute_map_null(arbb_attribute_map_t* object);

/// @}

/// @defgroup arbb_virtual_machine_errors Error Handling
/// @{

/// An error code representing the result of a function.
typedef enum {
  arbb_error_none, ///< No error occurred.
  arbb_error_invalid_argument, ///< At least one argument provided to a function was invalid. For example, a null object was provided to a parameter that must not be null.
  arbb_error_scoping, ///< An API operation was attempted in an illegal scope. For example, an attempt to add a control flow operation was made without having a currently defined function.
  arbb_error_out_of_bounds, ///< An attempt to access a container out of bounds was made.
  arbb_error_arithmetic, ///< Arithmetic exception occurred, such as overflow, underflow, or division by 0.
  arbb_error_bad_alloc, ///< Memory allocation failed.
  arbb_error_uninitialized_access, ///< Undefined variable was used.
  arbb_error_internal, ///< Unexpected internal error occurred.
} arbb_error_t;

/// Returns a character string with an informative error message
/// corresponding to @p error_details.
///
/// @return Descriptive error message. If a null @p error_details object
/// is passed in, a null pointer is returned.
ARBB_VM_EXPORT
const char* arbb_get_error_message(arbb_error_details_t error_details);

/// Returns an error code corresponding to the @p error_details
/// object.
///
/// @return Error code. If a null @p error_details object is passed
/// in, ::arbb_error_none is returned.
ARBB_VM_EXPORT
arbb_error_t arbb_get_error_code(arbb_error_details_t error_details);

/// Frees the resources for @p error_details. If @p error_details is a
/// null object, this function has no effect.
ARBB_VM_EXPORT
void arbb_free_error_details(arbb_error_details_t error_details);

/// @}

/// @defgroup arbb_virtual_machine_contexts Contexts
/// @{

/// Retrieves the context for the default ArBB VM implementation. VM
/// operations may then be issued to the default ArBB VM implementation using
/// the returned context.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the reference was successfully decremented.
///  - ::arbb_error_invalid_argument if @p out_context is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_get_default_context(arbb_context_t* out_context,
                                      arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_refcounting Reference Counting
/// These functions enable conversion from specific object types that
/// are reference-counted to @ref arbb_refcountable_t instances.
/// @{

/// Converts the given @ref arbb_function_t to an @ref arbb_refcountable_t instance.
///
/// @return An @ref arbb_refcountable_t instance that can be used with arbb_acquire_ref() and arbb_release_ref()
ARBB_VM_EXPORT
arbb_refcountable_t arbb_function_to_refcountable(arbb_function_t convertible);

/// Converts the given @ref arbb_global_variable_t to an @ref arbb_refcountable_t instance.
///
/// @return An @ref arbb_refcountable_t instance that can be used with arbb_acquire_ref() and arbb_release_ref()
ARBB_VM_EXPORT
arbb_refcountable_t arbb_global_variable_to_refcountable(arbb_global_variable_t convertible);

/// Increments the reference count of @p refcountable.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the reference was successfully incremented.
///  - ::arbb_error_invalid_argument if @p refcountable is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_acquire_ref(arbb_refcountable_t refcountable,
                              arbb_error_details_t* details);

/// Decrements the reference count of @p refcountable. If the reference
/// count of the object drops to zero, its resources are reclaimed.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the reference was successfully decremented.
///  - ::arbb_error_invalid_argument if @p refcountable is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_release_ref(arbb_refcountable_t refcountable,
                              arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_strings String Objects
/// @{

/// Retrieves the C string associated with @p string.
///
/// @return The C string held by @p string.
ARBB_VM_EXPORT
const char* arbb_get_c_string(arbb_string_t string);

/// Frees any resources associated with @p string.
ARBB_VM_EXPORT
void arbb_free_string(arbb_string_t string);

/// @}

/// @defgroup arbb_virtual_machine_types Types
/// All variables in the VM have a corresponding type represented by
/// an @ref arbb_type_t object.
///
/// @{

/// The fixed set of scalar types provided.
typedef enum {
  arbb_i8, ///< 8-bit signed integer
  arbb_i16, ///< 16-bit signed integer
  arbb_i32, ///< 32-bit signed integer
  arbb_i64, ///< 64-bit signed integer
  arbb_u8, ///< 8-bit unsigned integer
  arbb_u16, ///< 16-bit unsigned integer
  arbb_u32, ///< 32-bit unsigned integer
  arbb_u64, ///< 64-bit unsigned integer
  arbb_f32, ///< 32-bit floating-point number
  arbb_f64, ///< 64-bit floating-point number
  arbb_boolean, ///< Boolean <tt>true</tt>/<tt>false</tt> value
  arbb_usize, ///< Unsigned array index or size
  arbb_isize, ///< Signed array index or size
} arbb_scalar_type_t;

/// Sets @p out_type to the @ref arbb_type_t object corresponding to a
/// scalar of type @p scalar_type.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_type is a null pointer.
///  - ::arbb_error_invalid_argument if @p scalar_type is an invalid value.
ARBB_VM_EXPORT
arbb_error_t arbb_get_scalar_type(arbb_context_t context,
                                  arbb_type_t* out_type,
                                  arbb_scalar_type_t scalar_type,
                                  arbb_error_details_t* details);

/// Returns the number of bytes in the representation of @p type.
//
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_size is a null pointer.
///  - ::arbb_error_invalid_argument if @p type is a null object.
///  - ::arbb_error_invalid_argument if @p type is not a scalar type.
ARBB_VM_EXPORT
arbb_error_t arbb_sizeof_type(arbb_context_t context,
                              uint64_t* out_size,
                              arbb_type_t type,
                              arbb_error_details_t* details);

/// Sets @p out_type to the @ref arbb_type_t object corresponding to a
/// dense container with the specified element type and dimensionality.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_type is a null pointer.
///  - ::arbb_error_invalid_argument if @p dimensionality is not 1, 2, or 3.
///  - ::arbb_error_invalid_argument if @p element_type is a null object.
///  - ::arbb_error_invalid_argument if @p element_type is not a composable type.
ARBB_VM_EXPORT
arbb_error_t arbb_get_dense_type(arbb_context_t context,
                                 arbb_type_t* out_type,
                                 arbb_type_t element_type,
                                 unsigned int dimensionality,
                                 arbb_error_details_t* details);

/// Sets @p *out_type to the @ref arbb_type_t object corresponding to a nested
/// container with the specified element type.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_type is a null pointer.
///  - ::arbb_error_invalid_argument if @p element_type is a null object.
///  - ::arbb_error_invalid_argument if @p element_type is not a composable type.
ARBB_VM_EXPORT
arbb_error_t arbb_get_nested_type(arbb_context_t context,
                                  arbb_type_t* out_type,
                                  arbb_type_t element_type,
                                  arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_constants Constants
/// Constants are variables of scalar type whose values never change.
/// @{

/// Creates a scalar constant of the given type. The data for the
/// constant is copied in from the pointer argument passed
/// to the @p data parameter.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_var is a null pointer.
///  - ::arbb_error_invalid_argument if @p type is a null object.
///  - ::arbb_error_invalid_argument if @p type is not a scalar type.
///  - ::arbb_error_invalid_argument if @p data is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_create_constant(arbb_context_t context,
                                  arbb_global_variable_t* out_var,
                                  arbb_type_t type,
                                  void* data,
                                  void* debug_data_ptr,
                                  arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_globals Globals
/// Global variables may be used in multiple functions, and may have
/// their values changed at run time.
/// @{

/// Creates a new global variable.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_var is a null pointer.
///  - ::arbb_error_invalid_argument if @p type is a null object.
///  - ::arbb_error_invalid_argument if @p type is a function type.
///  - ::arbb_error_invalid_argument if @p binding is not a null object and does not match the type being constructed.
ARBB_VM_EXPORT
arbb_error_t arbb_create_global(arbb_context_t context,
                                arbb_global_variable_t* out_var,
                                arbb_type_t type,
                                const char *name,
                                arbb_binding_t binding,
                                void* debug_data_ptr,
                                arbb_error_details_t* details);


/// Sets @p out_var to the @ref arbb_variable_t object wrapped by the
/// given @ref arbb_global_variable_t object.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_var is a null pointer.
///  - ::arbb_error_invalid_argument if @p global_var is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_get_variable_from_global(arbb_context_t context,
                                           arbb_variable_t* out_var,
                                           arbb_global_variable_t global_var,
                                           arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_functions Functions
/// @{

/// Adds a new function type with the specified prototype. The prototype consists of a
/// number of input and output parameters and their corresponding types.
/// The @p input_types and @p output_types parameters should be arrays with @p num_inputs and
/// @p num_outputs entries, respectively.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_type is a null pointer.
///  - ::arbb_error_invalid_argument if @p num_inputs is greater than 0 and @p input_types is a null pointer.
///  - ::arbb_error_invalid_argument if @p num_outputs is greater than 0 and @p output_types is a null pointer.
///  - ::arbb_error_invalid_argument if any entry in @p input_types is a null object.
///  - ::arbb_error_invalid_argument if any entry in @p output_types is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_get_function_type(arbb_context_t context,
                                    arbb_type_t* out_type,
                                    unsigned int num_outputs,
                                    const arbb_type_t* output_types,
                                    unsigned int num_inputs,
                                    const arbb_type_t* input_types,
                                    arbb_error_details_t* details);

/// Converts the provided @p function_type to a function type with the
/// same signature, but with the outputs aliased to a subset of
/// the inputs.
///
/// @param context The context in which to perform the operation.
/// @param out_type The location in which to store the new type.
/// @param function_type The function type on which the new type will
/// be based.
/// @param parameter_aliases An array of N integers, where N is the
/// number of outputs, specifying the indices of the inputs aliased
/// with each corresponding output.
/// @param details If non-null, this parameter contains error details
/// in case of an error.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_type is a null pointer.
///  - ::arbb_error_invalid_argument if @p function_type is a null object.
///  - ::arbb_error_invalid_argument if @p parameter_aliases is a null pointer.
///  - ::arbb_error_invalid_argument if any entry in @p parameter_aliases is greater than or equal to the number of inputs in @p function_type.
ARBB_VM_EXPORT
arbb_error_t arbb_get_function_type_parameter_alias(arbb_context_t context,
                                                    arbb_type_t* out_type,
                                                    arbb_type_t function_type,
                                                    const int* parameter_aliases,
                                                    arbb_error_details_t* details);




/// Creates a new function of the specified function type and begins
/// defining it. @p is_global must be non-zero if the function is going
/// to be executed with @ref arbb_execute. @p is_global must be 0 if the function
/// is going to be used as the callee argument of @ref arbb_call_op.
/// This function is not thread safe. 
/// You should build Intel&reg; ArBB functions in the main thread only.<br>
/// <b>Thread Safety </b><br>
/// A thread is a sub-process of a program that can access data
/// anywhere in the program. All programs have the main thread.
/// An application may use multiple threads either directly, by using operating system APIs,
/// or indirectly, for example, by using the Intel&reg; Threading Building Blocks (Intel&reg; TBB). 
/// In this case, you must ensure that the application only calls
/// functions from two threads simultaneously if they are thread safe.
/// An Intel&reg; ArBB function is thread safe if more than one thread can execute
/// it simultaneously and each execution does not have outputs
/// that are input or output parameters of another execution.
///
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_function is a null pointer.
///  - ::arbb_error_invalid_argument if @p function_type is not a valid function type.
///  - ::arbb_error_invalid_argument if @p name is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_begin_function(arbb_context_t context,
                                 arbb_function_t* out_function,
                                 arbb_type_t function_type,
                                 const char* name,
                                 int is_global,
                                 arbb_error_details_t* details);

/// Aborts the definition of the specified function.
///
/// This frees any resources, such as local variables associated with
/// the function. The function stops being a valid object.
///
/// This function should be used when an application error occurs
/// during function definition.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if the function is a null object.
///  - ::arbb_error_scoping if the given function is not being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_abort_function(arbb_function_t function,
                                 arbb_error_details_t* details);

/// Finishes the definition of the specified function.
///
/// @note This function is not thread safe. 
/// You should build Intel&reg; ArBB functions in the main thread only.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if function is a null object.
///  - ::arbb_error_scoping if the given function is not being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_end_function(arbb_function_t function,
                               arbb_error_details_t* details);

/// Sets @p *out_var to the variable corresponding to the input or
/// output of the given function at the position given by @p index.
/// If @p get_output is non-zero, an output is returned. Otherwise, an
/// input is returned.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p out_var is a null pointer.
///  - ::arbb_error_invalid_argument if @p index is greater than or equal
///    to the number of input or output parameters of the function, depending on the
///    value of @p get_output.
///  - ::arbb_error_scoping if @p function is not being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_get_parameter(arbb_function_t function,
                                arbb_variable_t* out_var,
                                int get_output,
                                unsigned int index,
                                arbb_error_details_t* details);

/// Serializes the intermediate representation of @p function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p out_text is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_serialize_function(arbb_function_t function,
                                     arbb_string_t* out_text,
                                     arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_locals Local Variables
/// @{

/// Creates a new local variable within the given function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p out_var is a null pointer.
///  - ::arbb_error_invalid_argument if @p type is a null object.
///  - ::arbb_error_invalid_argument if @p type is a function type.
///  - ::arbb_error_scoping if @p function is not being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_create_local(arbb_function_t function,
                               arbb_variable_t* out_var,
                               arbb_type_t type,
                               const char* name,
                               arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_operations Operations
/// @{

/// The set of operations that can be performed using arbb_op() or
/// arbb_op_dynamic().
typedef enum {
  // element-wise operations
  arbb_op_abs,
  arbb_op_acos,
  arbb_op_asin,
  arbb_op_atan,
  arbb_op_ceil,
  arbb_op_copy,
  arbb_op_cos,
  arbb_op_cosh,
  arbb_op_exp,
  arbb_op_exp10,
  arbb_op_floor,
  arbb_op_ln,
  arbb_op_log10,
  arbb_op_log_not,
  arbb_op_bit_not,
  arbb_op_rcp,
  arbb_op_round,
  arbb_op_rsqrt,
  arbb_op_sin,
  arbb_op_sinh,
  arbb_op_sqrt,
  arbb_op_tan,
  arbb_op_tanh,
  arbb_op_neg,
  arbb_op_add,
  arbb_op_bit_and,
  arbb_op_atan2,
  arbb_op_compare,
  arbb_op_div,
  arbb_op_equal,
  arbb_op_geq,
  arbb_op_greater,
  arbb_op_bit_or,
  arbb_op_leq,
  arbb_op_less,
  arbb_op_log_and,
  arbb_op_log_or,
  arbb_op_lsh,
  arbb_op_max,
  arbb_op_min,
  arbb_op_mod,
  arbb_op_mul,
  arbb_op_neq,
  arbb_op_pow,
  arbb_op_rsh,
  arbb_op_sub,
  arbb_op_bit_xor,
  arbb_op_select,

  // reorder operations
  arbb_op_gather,
  arbb_op_scatter,
  arbb_op_pack,
  arbb_op_unpack,
  arbb_op_shuffle,
  arbb_op_unshuffle,
  arbb_op_repeat,
  arbb_op_distribute,
  arbb_op_repeat_row,
  arbb_op_repeat_col,
  arbb_op_repeat_page,
  arbb_op_transpose,
  arbb_op_swap_col,
  arbb_op_swap_row,
  arbb_op_swap_page,
  arbb_op_shift_constant,
  arbb_op_shift_clamp,
  arbb_op_rotate,
  arbb_op_shift_constant_reverse,
  arbb_op_shift_clamp_reverse,
  arbb_op_rotate_reverse,
  arbb_op_reverse,

  // facility operations
  arbb_op_length,
  arbb_op_apply_nesting,
  arbb_op_get_nesting,
  arbb_op_cat,
  arbb_op_cast,
  arbb_op_extract,
  arbb_op_split,
  arbb_op_unsplit,
  arbb_op_index,
  arbb_op_mask,
  arbb_op_copy_nesting,
  arbb_op_flatten,
  arbb_op_const_vector,
  arbb_op_sort,
  arbb_op_sort_rank,
  arbb_op_replace,
  arbb_op_wall_clock,
  arbb_op_set_regular_nesting,
  arbb_op_replace_row,
  arbb_op_replace_col,
  arbb_op_replace_page,
  arbb_op_get_nrows,
  arbb_op_get_ncols,
  arbb_op_get_npages,
  arbb_op_replace_2d_section,
  arbb_op_extract_row,
  arbb_op_extract_col,
  arbb_op_extract_page,
  arbb_op_section,
  arbb_op_segment,
  arbb_op_replace_segment,
  arbb_op_alloc,
  arbb_op_replace_element,
  arbb_op_get_elt_coord,
  arbb_op_reset_dbg,
  arbb_op_set_dbg,
  arbb_op_bitwise_cast,
  arbb_op_get_neighbor,
  arbb_op_expect_size,

  // collective operations
  arbb_op_add_reduce,
  arbb_op_mul_reduce,
  arbb_op_max_reduce,
  arbb_op_max_reduce_loc,
  arbb_op_min_reduce,
  arbb_op_min_reduce_loc,
  arbb_op_and_reduce,
  arbb_op_ior_reduce,
  arbb_op_xor_reduce,

  arbb_op_add_scan,
  arbb_op_mul_scan,
  arbb_op_max_scan,
  arbb_op_min_scan,
  arbb_op_and_scan,
  arbb_op_ior_scan,
  arbb_op_xor_scan,

  arbb_op_add_merge,
  arbb_op_add_merge_scalar,

  // runtime assertions
  arbb_op_assert,
} arbb_opcode_t;

/// Adds a new instruction to the given function or executes the
/// provided operation. The following cases are possible:
/// <ul>
/// <li>If @p function is a null pointer, all input
/// and output parameters must be globals. In this case, the operation executes
/// immediately. The number of input and output parameters of the given <tt>opcode</tt>
/// must be static. The arguments passed to the @p outputs and @p inputs
/// parameters must be arrays of length matching the operation
/// arity. </li>
/// <li>If the @p attributes parameter is not a null pointer, the attribute map pointed to
/// by @p attributes is attached to the operation. Intel&reg; ArBB assumes ownership
/// of the attribute map and sets * @p attributes to a null object.</li>
/// </ul>
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p outputs is a null pointer.
///  - ::arbb_error_invalid_argument if @p inputs is a null pointer.
///  - ::arbb_error_invalid_argument if any entry in @p outputs is a null object.
///  - ::arbb_error_invalid_argument if any entry in @p inputs is a null object.
///  - ::arbb_error_invalid_argument if @p opcode does not have a static number of arguments.
///  - ::arbb_error_scoping if @p function is not currently being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_op(arbb_function_t function,
                     arbb_opcode_t opcode,
                     const arbb_variable_t* outputs,
                     const arbb_variable_t* inputs,
                     void* debug_data_ptrs[],
                     arbb_attribute_map_t* attributes,
                     arbb_error_details_t* details);

/// Adds a new instruction to the given function or executes the
/// provided operation. The following cases are possible:
/// <ul>
/// <li>If @p function is a null pointer, all input
/// and output parameters must be globals. In this case, the operation executes
/// immediately. The provided <tt>opcode</tt> must have a dynamic number of
/// input and/or output parameters. The arguments passed to the @p outputs and
/// @p inputs parameters must be arrays of length @p num_outputs and
/// @p num_inputs, respectively.</li> 
/// <li>If @p attributes is not a null pointer, the
/// attribute map pointed to by @p attributes is attached to the operation.
/// Intel&reg; ArBB assumes ownership of the attribute map and sets * @p attributes
/// to a null object.</li>
/// </ul>
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p outputs is a null pointer.
///  - ::arbb_error_invalid_argument if @p inputs is a null pointer.
///  - ::arbb_error_invalid_argument if any entry in @p outputs is a null object.
///  - ::arbb_error_invalid_argument if any entry in @p inputs is a null object.
///  - ::arbb_error_invalid_argument if the opcode has a static number of arguments.
///  - ::arbb_error_invalid_argument if either the @p inputs or @p outputs of the
///    opcode has a static size that does not match the provided size.
///  - ::arbb_error_invalid_argument if @p attributes is not a null pointer but
///    points to a null object.
///  - ::arbb_error_scoping if the given function is not currently being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_op_dynamic(arbb_function_t function,
                             arbb_opcode_t opcode,
                             unsigned int num_outputs,
                             const arbb_variable_t* outputs,
                             unsigned int num_inputs,
                             const arbb_variable_t* inputs,
                             void* debug_data_ptrs[],
                             arbb_attribute_map_t* attributes,
                             arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_calls Function Calls
/// @{

/// The set of ways in which functions may be called using arbb_call_op().
///
/// @see arbb_call_op()
typedef enum {
  arbb_op_call, ///< Performs a plain call operation.
  arbb_op_map ///< Performs a map operation.
} arbb_call_opcode_t;

/// Adds a new calling instruction to the given function. The number and
/// types of input and output parameters passed in must match the function
/// signature, as follows:
/// <ul> 
/// <li>For plain call operations, the function signature must
/// match exactly. </li>
/// <li>For map operations, output types must be containers
/// of the corresponding function signature types, and input types must
/// either match exactly or be containers of the corresponding function
/// signature types.</li>
/// </ul>
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p caller is a null object.
///  - ::arbb_error_invalid_argument if @p callee is a null object.
///  - ::arbb_error_invalid_argument if @p outputs is a null pointer.
///  - ::arbb_error_invalid_argument if @p inputs is a null pointer.
///  - ::arbb_error_invalid_argument if any entry in @p outputs is a null object.
///  - ::arbb_error_invalid_argument if any entry in @p inputs is a null object.
///  - ::arbb_error_invalid_argument if any variable provided in @p inputs
///      or @p outputs does not match the function signature appropriately.
///  - ::arbb_error_scoping if @p caller is not currently being defined.
///  - ::arbb_error_scoping if @p callee has not been defined yet.
ARBB_VM_EXPORT
arbb_error_t arbb_call_op(arbb_function_t caller,
                          arbb_call_opcode_t opcode,
                          arbb_function_t callee,
                          const arbb_variable_t* outputs,
                          const arbb_variable_t* inputs,
                          arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_if_statements If Statements
/// @{

/// Begins an <tt>if</tt> statement the body of which is executed if
/// the provided condition is met.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p condition is a null object.
///  - ::arbb_error_invalid_argument if @p condition is not a scalar of type ::arbb_boolean.
///  - ::arbb_error_scoping if the given @p function is not currently being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_if(arbb_function_t function,
                     arbb_variable_t condition,
                     arbb_error_details_t* details);

/// Adds an <tt>else</tt> part to the current <tt>if</tt> statement.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no <tt>if</tt> statement is being defined.
///  - ::arbb_error_scoping if the current <tt>if</tt> statement already contains a call to arbb_else().
ARBB_VM_EXPORT
arbb_error_t arbb_else(arbb_function_t function,
                       arbb_error_details_t* details);

/// Ends the current <tt>if</tt> statement.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no <tt>if</tt> statement is being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_end_if(arbb_function_t function,
                         arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_loops Loops
/// @{

/// The set of permitted loop types.
///
/// @see arbb_begin_loop()
typedef enum {
  arbb_loop_for, ///< A <tt>for</tt> loop with an initializer, condition, step, and body block.
  arbb_loop_while, ///< A <tt>while</tt> loop with a condition and a body block.
} arbb_loop_type_t;

/// The set of permitted loop block types.
///
/// @see arbb_begin_loop_block()
typedef enum {
  arbb_loop_block_init, ///< An initializer block.
  arbb_loop_block_cond, ///< A condition block.
  arbb_loop_block_body, ///< A body block.
  arbb_loop_block_step, ///< A step block.
} arbb_loop_block_t;

/// Begins a structured loop of the provided type. This call must be followed
/// by calls to arbb_begin_loop_block() and a call to
/// arbb_end_loop(). No statements can be added before the first call to
/// arbb_begin_loop_block().
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if @p function is not currently being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_begin_loop(arbb_function_t function,
                             arbb_loop_type_t loop_type,
                             arbb_error_details_t* details);

/// Begins a loop block within a given control flow statement, 
/// such as a condition block or a body block.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no loop statement is being defined.
///  - ::arbb_error_scoping if @p block_type is not appropriate for the loop being defined.
///  - ::arbb_error_scoping if @p block_type was already defined in the current loop.
ARBB_VM_EXPORT
arbb_error_t arbb_begin_loop_block(arbb_function_t function,
                                   arbb_loop_block_t block_type,
                                   arbb_error_details_t* details);

/// Exits the structured loop currently being defined if the provided condition
/// is true.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p condition is a null object.
///  - ::arbb_error_invalid_argument if @p condition is not a scalar of type ::arbb_boolean.
///  - ::arbb_error_scoping if no loop statement is being defined.
///  - ::arbb_error_scoping if the current loop block is not an ::arbb_loop_block_cond.
///  - ::arbb_error_scoping if a loop condition was already defined in the current loop.
ARBB_VM_EXPORT
arbb_error_t arbb_loop_condition(arbb_function_t function,
                                 arbb_variable_t condition,
                                 arbb_error_details_t* details);

/// Ends the structured loop currently being defined.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no loop statement is being defined.
///  - ::arbb_error_scoping if all required loop blocks were not defined in the loop.
ARBB_VM_EXPORT
arbb_error_t arbb_end_loop(arbb_function_t function,
                           arbb_error_details_t* details);

/// Inserts a break statement into the current function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no loop statement is being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_break(arbb_function_t function,
                        arbb_error_details_t* details);

/// Inserts a <tt>continue</tt> statement into the current function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_scoping if no loop statement is being defined.
ARBB_VM_EXPORT
arbb_error_t arbb_continue(arbb_function_t function,
                           arbb_error_details_t* details);


/// @}

/// @defgroup arbb_virtual_machine_attributes Attributes
/// @{

/// A key that you can use to index attribute values.
/// @see arbb_get_attribute_key
typedef uint32_t arbb_attribute_key_t;

/// An attribute value.
typedef union {
    int32_t as_int32;
    char* as_string;
    void* as_pointer;
} arbb_attribute_value_t;

/// An attribute key-value pair.
/// @see arbb_create_attribute_map
typedef struct {
  arbb_attribute_key_t key;
  arbb_attribute_value_t value;
} arbb_attribute_key_value_t;

/// The set of supported attribute value types.
/// @see arbb_lookup_attribute
typedef enum {
  arbb_attribute_int32, ///< A 32-bit signed integer. The result of @ref arbb_lookup_attribute is of type <tt>int32_t</tt>
  arbb_attribute_string, ///< A null-terminated character string. The result of @ref arbb_lookup_attribute is of type <tt>const char*</tt>
  arbb_attribute_pointer, ///< A void pointer. The result of @ref arbb_lookup_attribute is only safe to cast to the same type as passed to @ref arbb_create_attribute_map.
} arbb_attribute_type_t;

/// Returns an <tt>attribute_key_t</tt> object with identifier @p name
/// used to index attributes of the type specified by the @p type parameter
/// in an @ref arbb_attribute_map_t object.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p name is a null pointer.
///  - ::arbb_error_invalid_argument if @p out_key is a null pointer.
///  - ::arbb_error_invalid_argument if @p name is not a built-in
///      attribute key identifier and does not start with a '<tt>_</tt>' 
///      character.
///  - ::arbb_error_invalid_argument if an attribute key has already been
///      retrieved with the identifier @p name but with the type that is different
///      from the one specified by the @p type parameter.
ARBB_VM_EXPORT
arbb_error_t arbb_get_attribute_key(arbb_context_t context,
                                   const char* name,
                                   arbb_attribute_type_t type,
                                   arbb_attribute_key_t* out_key,
                                   arbb_error_details_t* details);

/// Creates an <tt>arbb_attribute_map_t</tt> object containing the key-value pairs
/// specified by the @p attributes array.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p attribute_count is 0.
///  - ::arbb_error_invalid_argument if @p attributes is a null pointer.
///  - ::arbb_error_invalid_argument if @p attributes contains the same
///      key more than once.
///  - ::arbb_error_invalid_argument if @p out_map is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_create_attribute_map(arbb_context_t context,
                                  unsigned int attribute_count,
                                  const arbb_attribute_key_value_t* attributes,
                                  arbb_attribute_map_t* out_map,
                                  arbb_error_details_t* details);

/// Frees the resources for @p object.
ARBB_VM_EXPORT
void arbb_free_attribute_map(arbb_attribute_map_t object);

/// Sets the pointer @p out_value to the attribute value associated with
/// @p key in @p attributes.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p attributes is a null object.
///  - ::arbb_error_invalid_argument if @p out_value is a null pointer.
///  - ::arbb_error_invalid_argument if @p attributes contains no values
///      associated with @p key.
ARBB_VM_EXPORT
arbb_error_t arbb_lookup_attribute(arbb_context_t context,
                                   arbb_attribute_map_t attributes,
                                   arbb_attribute_key_t key,
                                   void** out_value,
                                   arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_scalar_access Scalar Data Access
/// @{

/// Copies data out of the given global scalar to the host.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p variable is a null object.
///  - ::arbb_error_invalid_argument if @p out_data is a null pointer.
///  - ::arbb_error_invalid_argument if @p variable is not a global variable.
///  - ::arbb_error_invalid_argument if @p variable is not a scalar.
ARBB_VM_EXPORT
arbb_error_t arbb_read_scalar(arbb_context_t context,
                              arbb_variable_t variable,
                              void* out_data,
                              arbb_error_details_t* details);

/// Copies data into the given global variable from the host.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p variable is a null object.
///  - ::arbb_error_invalid_argument if @p out_data is a null pointer.
///  - ::arbb_error_invalid_argument if @p variable is not a global variable.
///  - ::arbb_error_invalid_argument if @p variable is not a scalar.
ARBB_VM_EXPORT
arbb_error_t arbb_write_scalar(arbb_context_t context,
                               arbb_variable_t variable,
                               const void* data,
                               arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_binding Container Bindings
/// @{

/// Creates a binding suitable for a dense container with the
/// specified parameters.
///
/// The number of elements in the @p sizes and @p byte_pitches arrays must match the value of the
/// @p dimensionality parameter.
/// The @p byte_pitches[i] specifies the number of bytes between columns/rows/pages in
/// the user data for i = 0, 1, 2, respectively.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @c context is a null object.
///  - ::arbb_error_invalid_argument if @c dimensionality is not 1, 2, or 3.
///  - ::arbb_error_invalid_argument if @c sizes is a null pointer.
///  - ::arbb_error_invalid_argument if @c user_data is a null pointer.
///  - ::arbb_error_invalid_argument if @c byte_pitches is a null pointer.
///  - ::arbb_error_invalid_argument if any value in @p byte_pitches is smaller than the element size.
///  - ::arbb_error_invalid_argument if @c out_binding is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_create_dense_binding(arbb_context_t context,
                                       arbb_binding_t* out_binding,
                                       void* user_data,
                                       unsigned int dimensionality,
                                       const uint64_t* sizes,
                                       const uint64_t* byte_pitches,
                                       arbb_error_details_t* details);

/// Releases all resources associated with the @p binding.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p binding is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_free_binding(arbb_context_t context,
                               arbb_binding_t binding,
                               arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_container_mapping Container Data Mapping
/// @{

/// The set of access modes permitted for arbb_map_to_host().
///
/// @see arbb_map_to_host()
typedef enum {
  arbb_read_only_range,
  arbb_write_only_range,
  arbb_read_write_range,
} arbb_range_access_mode_t;

/// Maps the global container provided in @p container into the host
/// address space.
///
/// The mapping is valid until the next virtual machine operation
/// that directly or indirectly accesses the given variable.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p container is a null object.
///  - ::arbb_error_invalid_argument if @p out_data is a null pointer.
///  - ::arbb_error_invalid_argument if @p out_byte_pitch is a null pointer.
///  - ::arbb_error_invalid_argument if @p container is not a global variable.
///  - ::arbb_error_invalid_argument if @p container is not a dense container.
ARBB_VM_EXPORT
arbb_error_t arbb_map_to_host(arbb_context_t context,
                              arbb_variable_t container,
                              void** out_data,
                              uint64_t* out_byte_pitch,
                              arbb_range_access_mode_t mode,
                              arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_execution Execution and Compilation
/// @{

/// Executes the given function.
///
/// This call is thread safe, but you must also ensure that each execution
/// does not have outputs that are input or output parameters of any other
/// concurrent execution of the same function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p function has not been completely defined.
///  - ::arbb_error_invalid_argument if @p outputs is a null pointer and the function has at least one output.
///  - ::arbb_error_invalid_argument if @p inputs is a null pointer and the function has at least one input.
///  - ::arbb_error_invalid_argument if any variable in @p inputs or @p outputs does not match the function parameters.
///  - ::arbb_error_invalid_argument if any variable in @p inputs or @p outputs is not global.
///  - ::arbb_error_out_of_bounds if an attempt to access a container out of bounds was made during execution.
///  - ::arbb_error_arithmetic if an arithmetic exception occurred during execution, such as overflow, underflow, or an integer division by zero.
///  - ::arbb_error_bad_alloc if memory allocation failed during execution.
///  - ::arbb_error_uninitialized_access if uninitialized variable was used during execution.
ARBB_VM_EXPORT
arbb_error_t arbb_execute(arbb_function_t function,
                          const arbb_variable_t* outputs,
                          const arbb_variable_t* inputs,
                          arbb_error_details_t* details);

/// Compiles the given function for a given set of arguments.
/// The provided arugments give information required for dynamic
/// recompilation. Passing to arbb_execute the same arguments or arguments that are
/// bound/non-bound in the same way results in no compilation
/// overhead.
///
/// This call is thread safe.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p function is a null object.
///  - ::arbb_error_invalid_argument if @p function has not been completely defined.
///  - ::arbb_error_invalid_argument if @p outputs is a null pointer and the function has at least one output.
///  - ::arbb_error_invalid_argument if @p inputs is a null pointer and the function has at least one input.
///  - ::arbb_error_invalid_argument if any variable in @p inputs or @p outputs does not match the function parameters.
///  - ::arbb_error_invalid_argument if any variable in @p inputs or @p outputs is not global.
ARBB_VM_EXPORT
arbb_error_t arbb_compile_for_args(arbb_function_t function,
                                   const arbb_variable_t* outputs,
                                   const arbb_variable_t* inputs,
                                   arbb_error_details_t* details);

/// Waits until all pending asynchronous Intel&reg; ArBB operations have completed.
/// Typically, this function is used when timing asynchronous operations.
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
ARBB_VM_EXPORT
arbb_error_t arbb_finish(arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_immediate_map Immediate Map Operation Support Functions
/// @{

/// Pushes a new coordinate on the map stack with values <tt>[x, y, z]</tt>
/// representing the coordinate along the first, second, and
/// third dimensions, respectively.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_push_map_coord(arbb_context_t context,
                                 uint64_t x,
                                 uint64_t y,
                                 uint64_t z,
                                 arbb_error_details_t* details);

/// Pops the current coordinate from the map coordinate stack.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_scoping if the map stack is currently empty.
ARBB_VM_EXPORT
arbb_error_t arbb_pop_map_coord(arbb_context_t context,
                                arbb_error_details_t* details);

/// Associates the global scalar variable @p scalar with the global container
/// @p source_container. Immediate invocations of ::arbb_op_get_neighbor operations
/// over the provided scalar fetch data from the provided container using the
/// current map coordinate.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p scalar is a null object.
///  - ::arbb_error_invalid_argument if @p source_container is a null object.
///  - ::arbb_error_invalid_argument if @p source_container is not a valid dense container.
ARBB_VM_EXPORT
arbb_error_t arbb_set_scalar_source_element(arbb_context_t context,
                                            arbb_variable_t scalar,
                                            arbb_variable_t source_container,
                                            arbb_error_details_t* details);

/// Sets a flag to copy the source container of @p scalar for subsequent use
/// as an input to arbb_op_copy in emulation mode.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p scalar is a null object.
///  - ::arbb_error_invalid_argument if @p scalar is not a valid scalar.
ARBB_VM_EXPORT
arbb_error_t
arbb_set_copy_scalar_source_element(arbb_context_t context,
                                    arbb_variable_t scalar,
                                    arbb_error_details_t* out_error_details);

/// @}

/// @defgroup arbb_virtual_machine_function_stack Auxiliary Operations - Function Stack
/// @{

/// Pushes the given function onto the function stack. The @p function
/// parameter may be a null object.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
ARBB_VM_EXPORT
arbb_error_t arbb_push_function(arbb_context_t context,
                                arbb_function_t function,
                                arbb_error_details_t* details);

/// Pops the top function from the function stack.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_scoping if the function stack is empty.
ARBB_VM_EXPORT
arbb_error_t arbb_pop_function(arbb_context_t context,
                               arbb_error_details_t* details);

/// Retrieves the top function from the function stack, placing it in
/// @p *out_function. If the function stack is empty, @p *out_function
/// is set to a null object.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_function is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_top_function(arbb_context_t context,
                               arbb_function_t* out_function,
                               arbb_error_details_t* details);

/// @}

/// @defgroup arbb_virtual_machine_stack_trace C/C++ Stack Traces
/// @{

/// The set of frame properties that can be queried.
/// @see arbb_cxx_get_frame_property
typedef enum {
  arbb_function_name, ///< The name of the function in the frame. C++ identifiers are left in mangled form. The result of arbb_cxx_get_frame_property is of type <tt>const char*</tt>.
  arbb_line_number, ///< The line number corresponding to the next function call in the stack frame. The result of arbb_cxx_get_frame_property is of type <tt>const unsigned int*</tt>.
  arbb_file_name ///< The name of the file in which the function was defined. The result arbb_cxx_get_frame_property is of type <tt>const char*</tt>.
} arbb_cxx_frame_property_t;

/// Defines a pointer to a callback function that the VM calls to request file name
///   and line number information from the front end. The VM calls this function 
///   when gathering debug information for an arbb_op with the <tt>debug_info</tt> attribute set by the front end.
///   The @p debug_info parameter passed to this call is
///   the pointer stored in that attribute by the front end. The front end should use 
///   the @p debug_info to determine the file name and line number in the user code to
///   be associated with the operation in question.
///
/// @return An integer value depending on the result of the operation:
///  - 1 if the front end provided line number information
///  - 0 if the front end could not provide line number information
typedef int (*arbb_source_info_provider_t)(arbb_context_t context,
                                            void*          attribute_value,
                                            char**         out_function_name,
                                            char**         out_file_name,
                                            unsigned int*  out_line_number);

/// Returns an arbb_cxx_stack_trace_t object that contains debug information about
/// all frames in the C/C++ stack trace, where the first frame is the frame
/// that called arbb_cxx_store_stack_trace. If a stack trace could not be
/// constructed, for example, due to missing or incomplete debug information, 
/// @p out_stack_trace is a null object.
///
/// @see arbb_cxx_stack_trace_t
/// @see arbb_cxx_is_stack_trace_null
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out_stack_trace is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_cxx_store_stack_trace(arbb_context_t context,
                                          arbb_cxx_stack_trace_t* out_stack_trace,
                                          arbb_error_details_t* details);

/// Releases all resources belonging to @p stack_trace. If @p stack_trace is
/// a null object, this function has no effect.
///
ARBB_VM_EXPORT
void arbb_cxx_release_stack_trace(arbb_cxx_stack_trace_t stack_trace);

/// Returns the number of stack frames in @p stack_trace.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p stack_trace is a null object.
///  - ::arbb_error_invalid_argument if @p out_num_frames is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_cxx_get_frame_count(arbb_context_t context,
                                      arbb_cxx_stack_trace_t stack_trace,
                                      unsigned int* out_num_frames,
                                      arbb_error_details_t* details);

/// Returns the stack frame at position @p frame_num in @p stack_trace.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p stack_trace is a null object.
///  - ::arbb_error_invalid_argument if @p frame_num is larger than or equal to the number of stack frames in @p stack_trace.
///  - ::arbb_error_invalid_argument if @p out_frame is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_cxx_get_frame(arbb_context_t context,
                                arbb_cxx_stack_trace_t stack_trace,
                                unsigned int frame_num,
                                arbb_cxx_frame_t* out_frame,
                                arbb_error_details_t* details);

/// Sets the pointer @p out to the frame property specified by @p type. After this function
/// is executed, the data that @p out points to can be
/// safely cast to the data type that corresponds to the @p type parameter.
/// See @ref arbb_cxx_frame_property_t for the supported query types.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p out is a null pointer.
ARBB_VM_EXPORT
arbb_error_t arbb_cxx_get_frame_property(arbb_context_t context,
                                         arbb_cxx_frame_t frame,
                                         arbb_cxx_frame_property_t type,
                                         void** out,
                                         arbb_error_details_t* details);

/// Sets @p out_name to the demangled name of the C/C++ function
/// pointed to by @p function. When using this function, you must call
/// @ref arbb_free_string on the returned string object
/// once it is no longer required.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p context is a null object.
///  - ::arbb_error_invalid_argument if @p function is null, or @p out_name is null
ARBB_VM_EXPORT
arbb_error_t arbb_cxx_resolve_function_name(arbb_context_t context,
                                            void* function,
                                            arbb_string_t* out_name,
                                            arbb_error_details_t* details);

/// Registers a callback function that the VM should use to permit the front
/// end to extract the file name and line number information from the attribute
/// associated with <tt>arbb_op</tt>. If a callback function was previously
/// registered with the same key, the new callback function replaces the
/// previous callback function.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_invalid_argument if @p callback is a null pointer.  
ARBB_VM_EXPORT
arbb_error_t arbb_register_source_info_provider(arbb_context_t context, 
                                                arbb_attribute_key_t key,
                                                arbb_source_info_provider_t callback,
                                                arbb_error_details_t* details);

/// @}

/// @defgroup arbb_properties Run-time Properties
/// @{

/// Enables or disables the creation of threaded code.
/// This operation is only valid before the first call to arbb_begin_function.
/// @param context The context in which to perform the operation.
/// @param value An integer value defining the code generation mode.
/// If @p value is non-zero, the VM generates threaded code during function compilation.
/// If @p value is zero, the VM generates serial code.
/// @param details If non-null, this parameter contains error details
/// in case of an error.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_scoping if the function has already been compiled.
/// @see @ref arbb_virtual_machine_functions  
ARBB_VM_EXPORT
arbb_error_t arbb_set_generate_threaded_code(arbb_context_t context,
                                             int value,
                                             arbb_error_details_t* details);

/// Sets the number of threads used when executing kernels while threaded code
/// generation is enabled. By default, the number of threads equals the number of
/// available physical cores used for the kernel execution.
/// 
/// @param context The context in which to perform the operation.
/// @param num_cores The number of threads to be used. 
/// Setting @p num_cores to zero restores the default number of threads.
/// @param details If non-null, this parameter contains error details
/// in case of an error.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
ARBB_VM_EXPORT
arbb_error_t arbb_set_num_threads(arbb_context_t context,
                                  unsigned int num_cores,
                                  arbb_error_details_t* details);

/// Sets the number of tasks spawned per core during parallel execution. 
/// By default, the decomposition degree equals the number of available
/// software threads. If hyper-threading is enabled, the decomposition degree
/// is multiplied by two internally.
/// A larger number can improve dynamic load balancing across threads, but may
/// eventually result in a noticeable overhead.
/// 
/// @note Changing the decomposition degree impacts concurrent task execution. Optimal
/// settings are target and workload dependent.
///
/// @param context The context in which to perform the operation.
/// @param decomp_degree_multiplier The decomposition degree multiplier defining the number of tasks to be executed per core.
/// Setting @p decomp_degree_multiplier to 0 on the host restores the default number of tasks.
/// @param details If non-null, this parameter contains error details
/// in case of an error.

///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
ARBB_VM_EXPORT
arbb_error_t arbb_set_decomp_degree(arbb_context_t context,
                                    unsigned int decomp_degree_multiplier,
                                    arbb_error_details_t* details);

/// Sets the initial and the maximum heap size for heap memory management.
/// Use this function to overwrite the default heap size setting.
/// You may only invoke this function before instantiating any VM 
/// variables or compiling a VM function. That is, you need to call this API 
/// before any VM API that requires an arbb_context_t variable.
///
/// @return An error code depending on the result of the operation:
///  - ::arbb_error_none if the operation succeeded.
///  - ::arbb_error_scoping if this function is invoked after heap setup.
ARBB_VM_EXPORT
arbb_error_t arbb_set_heap_size(uint64_t init_heap_size,
                                uint64_t max_heap_size,
                                arbb_error_details_t* details);

/// @}

/// @}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // ARBB_VM_API_H
