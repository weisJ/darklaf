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
 */
#import <Foundation/Foundation.h>
#import <jni.h>

__BEGIN_DECLS

#ifndef jlong_to_ptr
#define jlong_to_ptr(a) ((void *)(uintptr_t)(a))
#endif

#define OBJC(jl) ((id)jlong_to_ptr(jl))

#define JNF_COCOA_DURING(env)                                   \
@try {

#define JNF_COCOA_HANDLE(env)									\
} @catch(NSException *localException) {							\
    [JNF_Exception throwToJava:env exception:localException];	\
}

#define JNF_COCOA_ENTER(env)									\
@autoreleasepool {                                              \
    @try {                                                      \
        @autoreleasepool {

#define JNF_COCOA_EXIT(env)										\
        } /*@autoreleasepool*/                                  \
    JNF_COCOA_HANDLE(env)                                       \
} /*@autoreleasepool*/


@interface JNF_RunLoop : NSObject { }
+ (void)performOnMainThread:(SEL)aSelector on:(id)target withObject:(id)arg waitUntilDone:(BOOL)waitUntilDone;
+ (void)performOnMainThreadWaiting:(BOOL)waitUntilDone withBlock:(void (^)(void))block;
@end

@interface JNF_Exception : NSException
+ (void)throwToJava:(JNIEnv *)env exception:(NSException *)exception;
@end


__END_DECLS
