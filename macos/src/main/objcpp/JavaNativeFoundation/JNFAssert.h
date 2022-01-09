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
 * Assertions used by the Darklaf_JNF_COCOA_ENTER()/Darklaf_JNF_COCOA_EXIT() and class
 * caching macros. When building debug builds, improper use of the caching
 * macros will trigger warnings output to the console.
 */

#import <JavaNativeFoundation/Darklaf_JNFJNI.h>

#ifdef DEBUG
#define JAVA_ASSERTIONS_ON
#endif /* DEBUG */

// Use the WARN macro to send a message to stderr in the
// debug build.  It gets removed from the optimized build
// during preprocessing.
#ifdef DEBUG
#define Darklaf_JNF_WARN Darklaf_JNFDebugWarning
#else
#define Darklaf_JNF_WARN if (0) Darklaf_JNFDebugWarning
#endif /* DEBUG */

__BEGIN_DECLS

Darklaf_JNF_EXPORT extern void Darklaf_JNFDebugWarning(const char *fmt, ...);

Darklaf_JNF_EXPORT extern void Darklaf_JNFAssertionFailure(const char *file, int line, const char *condition, const char *msg);

#ifdef JAVA_ASSERTIONS_ON

#define Darklaf_JNF_ASSERT_FAILURE(condition, msg)					\
    Darklaf_JNFAssertionFailure(__FILE__, __LINE__, condition, msg)	\


#define Darklaf_JNF_ASSERT_MSG(condition, msg)						\
do {														\
    if (!(condition)) {										\
        Darklaf_JNF_ASSERT_FAILURE(#condition, msg);				\
    }														\
} while(0)													\


#define Darklaf_JNF_ASSERT_COND(condition)							\
    Darklaf_JNF_ASSERT_MSG(condition, NULL)							\


#define Darklaf_JNF_EXCEPTION_WARN(env, msg)						\
do {														\
    (*(env))->ExceptionDescribe(env);						\
    Darklaf_JNF_ASSERT_FAILURE("Java exception thrown", msg);		\
} while (0)													\


#define Darklaf_JNF_ASSERT_NO_EXCEPTION_MSG(env, msg)				\
if ((*(env))->ExceptionOccurred(env)) {						\
    Darklaf_JNF_EXCEPTION_WARN(env, msg);							\
}															\


#define Darklaf_JNF_ASSERT_NO_EXCEPTION(env)						\
    Darklaf_JNF_ASSERT_NO_EXCEPTION_MSG(env, NULL)					\

#else

#define Darklaf_JNF_ASSERT_COND(condition)
#define Darklaf_JNF_ASSERT_MSG(condition, msg)
#define Darklaf_JNF_EXCEPTION_WARN(env, msg)
#define Darklaf_JNF_ASSERT_NO_EXCEPTION(env)
#define Darklaf_JNF_ASSERT_NO_EXCEPTION_MSG(env, msg)

#endif /* JAVA_ASSERTIONS_ON */

Darklaf_JNF_EXPORT extern void Darklaf_JNFDumpJavaStack(JNIEnv *env);

__END_DECLS
