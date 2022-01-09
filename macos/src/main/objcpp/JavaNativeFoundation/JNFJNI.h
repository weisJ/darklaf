/*
 * Copyright (c) 2008-2020 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright notice,
 *      this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the copyright holder nor the names of its
 *      contributors may be used to endorse or promote products derived from
 *      this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * --
 *
 * The basic building blocks of writing Java JNI code that interacts with Objective-C.
 *
 *  All JNI functions should call Darklaf_JNF_COCOA_ENTER()/Darklaf_JNF_COCOA_EXIT() to properly
 *  catch thrown NSExceptions and periodically flush the autorelease pool for the
 *  current thread. Darklaf_JNF_COCOA_DURING()/Darklaf_JNF_COCOA_HANDLE() should only be used when
 *  AppKit is known to not be initialized yet.
 *
 *  Darklaf_JNF_CLASS_CACHE()/Darklaf_JNF_MEMBER_CACHE()/Darklaf_JNF_STATIC_MEMBER_CACHE()/Darklaf_JNF_CTOR_CACHE()
 *  all cache references to Java classes, methods, and variables for use by the
 *  GET/SET/CALL functions. These functions check for Java exceptions, immediately
 *  re-throwing them as Darklaf_JNFExceptions, and are simpler than their pure JNI equivalents.
 */

#import <jni.h>
#import <os/availability.h>
#import <sys/cdefs.h>

#define Darklaf_JNF_EXPORT __attribute__ ((visibility ("default"))) API_UNAVAILABLE(ios)

#import "JNFException.h"
#import "JNFAutoreleasePool.h"

__BEGIN_DECLS

// from jlong.h
// All pointers in and out of JNI functions should be expressed as jlongs
// to accomodate for both 32-bit and 64-bit pointer sizes
#ifndef jlong_to_ptr
#define jlong_to_ptr(a) ((void *)(uintptr_t)(a))
#endif

#ifndef ptr_to_jlong
#define ptr_to_jlong(a) ((jlong)(uintptr_t)(a))
#endif

// Darklaf_JNF_COCOA_DURING - Outermost exception scope for a JNI native method
//
// Use this macro only if you don't want any autorelease pool set or
// other Darklaf_JNFThreadContext setup (ie, if the AppKit isn't running
// yet).  Usually, you want to use Darklaf_JNF_COCOA_ENTER & Darklaf_JNF_COCOA_EXIT
#define Darklaf_JNF_COCOA_DURING(env)									\
@try {


// Darklaf_JNF_COCOA_HANDLE - Close of Darklaf_JNF_COCOA_DURING
//
// Use this macro to match an Darklaf_JNF_COCOA_DURING
// This macro ensures that no NSException is thrown into
// the VM.  It turns NSExceptions into Java exceptions.
#define Darklaf_JNF_COCOA_HANDLE(env)									\
} @catch(NSException *localException) {							\
    [Darklaf_JNFException throwToJava:env exception:localException];	\
}																\


// Darklaf_JNF_COCOA_ENTER - Place at the beginning of every JNI method
//
// Sets up an exception handler and an autorelease pool if one is
// not already setup.
//
// Note: if the native method executes before AppKit is
// initialized, use Darklaf_JNF_COCOA_DURING.
#define Darklaf_JNF_COCOA_ENTER(env)									\
{																\
    Darklaf_JNFAutoreleasePoolToken* _token = Darklaf_JNFNativeMethodEnter();	\
    Darklaf_JNF_COCOA_DURING(env)


// Darklaf_JNF_COCOA_EXIT - Place at the end of every JNI method
//
// Catches NSExceptions and re-throws them as Java exceptions.
// Use this macro to match Darklaf_JNF_COCOA_ENTER.
#define Darklaf_JNF_COCOA_EXIT(env)										\
    Darklaf_JNF_COCOA_HANDLE(env)										\
    @finally {													\
        if (_token) Darklaf_JNFNativeMethodExit(_token);				\
    }															\
}

// Darklaf_JNF_CHECK_AND_RETHROW_EXCEPTION - rethrows exceptions from Java
//
// Takes an exception thrown from Java, and transforms it into an
// NSException. The NSException should bubble up to the upper-most
// Darklaf_JNF_COCOA_ENTER/Darklaf_JNF_COCOA_EXIT pair, and then be re-thrown as
// a Java exception when returning from JNI. This check should be
// done after raw JNI operations which could cause a Java exception
// to be be thrown. The Darklaf_JNF{Get/Set/Call}  macros below do this
// check automatically.
#define Darklaf_JNF_CHECK_AND_RETHROW_EXCEPTION(env)							\
{																	\
    jthrowable _exception = (*env)->ExceptionOccurred(env);			\
    if (_exception) [Darklaf_JNFException raise:env throwable:_exception];	\
}


// Use Darklaf_JNF_CLASS_CACHE, Darklaf_JNF_MEMBER_CACHE, Darklaf_JNF_STATIC_MEMBER_CACHE
// and Darklaf_JNF_CTOR_CACHE as convenient ways to create
// Darklaf_JNFClassInfo and Darklaf_JNFMemberInfo records that can
// be passed to the utility functions that follow.

// Darklaf_JNF_CLASS_CACHE - Create a Darklaf_JNFClassInfo struct
//
// Use this macro to define a Darklaf_JNFClassInfo struct.
// For example:
// Darklaf_JNF_CLASS_CACHE(jc_java_awt_Font, "java/awt/Font");
// defines the symbol jc_java_awt_Font to point to the
// appropriately initialized Darklaf_JNFClassInfo struct.
// The "jc_" prefix is short for "java class."
#define Darklaf_JNF_CLASS_CACHE(cache_symbol, name) \
    Darklaf_JNFClassInfo cache_symbol = {name, NULL}

// Darklaf_JNF_MEMBER_CACHE - Create a Darklaf_JNFMemberInfo struct
//
// This macro creates and initializes a Darklaf_JNFMemberInfo
// struct, and defines a pointer to it.  Example:
// Darklaf_JNF_MEMBER_CACHE(jm_Font_isBold, jc_java_awt_Font, "isBold", "Z");
// This defines the symbol jm_Font_isBold to point to a
// Darklaf_JNFMemberInfo struct that represents the isBold method
// of the class java.awt.Font.  Use this macro for both
// fields and methods.
#define Darklaf_JNF_MEMBER_CACHE(cache_symbol, class_cache_symbol, name, sig) \
    Darklaf_JNFMemberInfo _ ## cache_symbol = {name, sig, NO, &class_cache_symbol, {NULL}}, *cache_symbol=&_ ## cache_symbol

// Darklaf_JNF_STATIC_MEMBER_CACHE - Create a Darklaf_JNFMemberInfo struct for static members
//
// Same as Darklaf_JNF_MEMBER_CACHE, but used for static fields and mehods.
#define Darklaf_JNF_STATIC_MEMBER_CACHE(cache_symbol, class_cache_symbol, name, sig) \
    Darklaf_JNFMemberInfo _ ## cache_symbol = {name, sig, YES, &class_cache_symbol, {NULL}}, *cache_symbol=&_ ## cache_symbol

// Darklaf_JNF_CTOR_CACHE - Create a Darklaf_JNFMemberInfo struct for a constructor
//
// Same as Darklaf_JNF_MEMBER_CACHE, but for constructors
#define Darklaf_JNF_CTOR_CACHE(cache_symbol, class_cache_symbol, sig) \
    Darklaf_JNFMemberInfo _ ## cache_symbol = {"<init>", sig, NO, &class_cache_symbol, {NULL}}, *cache_symbol=&_ ## cache_symbol


// Darklaf_JNFClassInfo - struct for caching a java class reference
//
// Create one of these by using the Darklaf_JNF_CLASS_CACHE macro (below).
// The class ref is resolved lazily.
typedef struct _Darklaf_JNFClassInfo {
    const char *name;	// fully/qualified/ClassName
    jclass cls;			// The JNI global class reference.
} Darklaf_JNFClassInfo;

// Darklaf_JNFMemberInfo - struct for caching a field or method ID
//
// Create these by using the Darklaf_JNF_MEMBER_CACHE macro (below).
// The member ID is resolved lazily.
typedef struct _Darklaf_JNFMemberInfo {
    const char *name;			// The name of the member
    const char *sig;			// The signature of the member
    BOOL isStatic;				// Is this member declared static?
    Darklaf_JNFClassInfo *classInfo;	// points to the Darklaf_JNFClassInfo struct of
    //   which this field/method is a member.
    union _j {
        jfieldID fieldID;		// If field, the JNI field ID
        jmethodID methodID;		// If method, the JNI method ID
    } j;
} Darklaf_JNFMemberInfo;


/*
 * JNI Utility Functions
 *
 * These functions make use of class and method ID caching, so they
 * are more efficient than simply calling their JNI equivalents directly.
 * They also detect Java exceptions and throw a corresponding
 * NSException when JNI returns with a Java exception.
 * Therefore, you should be prepared to handle exceptions
 * before they propagate either back to the VM or up
 * to the run loop.
 */

// Darklaf_JNFIsInstanceOf - returns whether obj is an instance of clazz
Darklaf_JNF_EXPORT extern BOOL Darklaf_JNFIsInstanceOf(JNIEnv *env, jobject obj, Darklaf_JNFClassInfo *clazz);

// Creating instances
Darklaf_JNF_EXPORT extern jobject Darklaf_JNFNewObject(JNIEnv *env, Darklaf_JNFMemberInfo *constructor, ...);

// Creating arrays
Darklaf_JNF_EXPORT extern jobjectArray		Darklaf_JNFNewObjectArray	(JNIEnv *env, Darklaf_JNFClassInfo *clazz, jsize length);
Darklaf_JNF_EXPORT extern jbooleanArray	Darklaf_JNFNewBooleanArray	(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jbyteArray		Darklaf_JNFNewByteArray		(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jcharArray		Darklaf_JNFNewCharArray		(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jshortArray		Darklaf_JNFNewShortArray	(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jintArray		Darklaf_JNFNewIntArray		(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jlongArray		Darklaf_JNFNewLongArray		(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jfloatArray		Darklaf_JNFNewFloatArray	(JNIEnv *env, jsize length);
Darklaf_JNF_EXPORT extern jdoubleArray		Darklaf_JNFNewDoubleArray	(JNIEnv *env, jsize length);

// Non-static getters
Darklaf_JNF_EXPORT extern jobject  Darklaf_JNFGetObjectField (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jboolean Darklaf_JNFGetBooleanField(JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jbyte    Darklaf_JNFGetByteField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jchar    Darklaf_JNFGetCharField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jshort   Darklaf_JNFGetShortField  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jint     Darklaf_JNFGetIntField    (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jlong    Darklaf_JNFGetLongField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jfloat   Darklaf_JNFGetFloatField  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jdouble  Darklaf_JNFGetDoubleField (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field);

// Static getters
Darklaf_JNF_EXPORT extern jobject  Darklaf_JNFGetStaticObjectField (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jboolean Darklaf_JNFGetStaticBooleanField(JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jbyte    Darklaf_JNFGetStaticByteField   (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jchar    Darklaf_JNFGetStaticCharField   (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jshort   Darklaf_JNFGetStaticShortField  (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jint     Darklaf_JNFGetStaticIntField    (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jlong    Darklaf_JNFGetStaticLongField   (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jfloat   Darklaf_JNFGetStaticFloatField  (JNIEnv *env, Darklaf_JNFMemberInfo *field);
Darklaf_JNF_EXPORT extern jdouble  Darklaf_JNFGetStaticDoubleField (JNIEnv *env, Darklaf_JNFMemberInfo *field);

// Non-static setters
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetObjectField (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jobject val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetBooleanField(JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jboolean val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetByteField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jbyte val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetCharField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jchar val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetShortField  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jshort val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetIntField    (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jint val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetLongField   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jlong val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetFloatField  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jfloat val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetDoubleField (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *field, jdouble val);

// Static setters
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticObjectField (JNIEnv *env, Darklaf_JNFMemberInfo *field, jobject val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticBooleanField(JNIEnv *env, Darklaf_JNFMemberInfo *field, jboolean val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticByteField   (JNIEnv *env, Darklaf_JNFMemberInfo *field, jbyte val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticCharField   (JNIEnv *env, Darklaf_JNFMemberInfo *field, jchar val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticShortField  (JNIEnv *env, Darklaf_JNFMemberInfo *field, jshort val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticIntField    (JNIEnv *env, Darklaf_JNFMemberInfo *field, jint val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticLongField   (JNIEnv *env, Darklaf_JNFMemberInfo *field, jlong val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticFloatField  (JNIEnv *env, Darklaf_JNFMemberInfo *field, jfloat val);
Darklaf_JNF_EXPORT extern void Darklaf_JNFSetStaticDoubleField (JNIEnv *env, Darklaf_JNFMemberInfo *field, jdouble val);

// Calling instance methods
Darklaf_JNF_EXPORT extern void     Darklaf_JNFCallVoidMethod   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jobject  Darklaf_JNFCallObjectMethod (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jboolean Darklaf_JNFCallBooleanMethod(JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jbyte    Darklaf_JNFCallByteMethod   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jchar    Darklaf_JNFCallCharMethod   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jshort   Darklaf_JNFCallShortMethod  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jint     Darklaf_JNFCallIntMethod    (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jlong    Darklaf_JNFCallLongMethod   (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jfloat   Darklaf_JNFCallFloatMethod  (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jdouble  Darklaf_JNFCallDoubleMethod (JNIEnv *env, jobject obj, Darklaf_JNFMemberInfo *method, ...);

// Calling static methods
Darklaf_JNF_EXPORT extern void     Darklaf_JNFCallStaticVoidMethod   (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jobject  Darklaf_JNFCallStaticObjectMethod (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jboolean Darklaf_JNFCallStaticBooleanMethod(JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jbyte    Darklaf_JNFCallStaticByteMethod   (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jchar    Darklaf_JNFCallStaticCharMethod   (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jshort   Darklaf_JNFCallStaticShortMethod  (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jint     Darklaf_JNFCallStaticIntMethod    (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jlong    Darklaf_JNFCallStaticLongMethod   (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jfloat   Darklaf_JNFCallStaticFloatMethod  (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);
Darklaf_JNF_EXPORT extern jdouble  Darklaf_JNFCallStaticDoubleMethod (JNIEnv *env, Darklaf_JNFMemberInfo *method, ...);

// Global references
Darklaf_JNF_EXPORT extern jobject Darklaf_JNFNewGlobalRef(JNIEnv *env, jobject obj);
Darklaf_JNF_EXPORT extern void Darklaf_JNFDeleteGlobalRef(JNIEnv *env, jobject globalRef);
Darklaf_JNF_EXPORT extern jobject Darklaf_JNFNewWeakGlobalRef(JNIEnv *env, jobject obj);
Darklaf_JNF_EXPORT extern void Darklaf_JNFDeleteWeakGlobalRef(JNIEnv *env, jobject globalRef);

__END_DECLS
