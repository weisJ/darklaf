/*
 * Copyright (c) 2009-2020 Apple Inc. All rights reserved.
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

#import "JNFRunnable.h"
#import "JNFThread.h"
#import "JNFJObjectWrapper.h"


static Darklaf_JNF_CLASS_CACHE(jc_Runnable, "java/lang/Runnable");
static Darklaf_JNF_MEMBER_CACHE(jm_run, jc_Runnable, "run", "()V");

@interface Darklaf_JNFRunnableWrapper : Darklaf_JNFJObjectWrapper { }
- (void) invokeRunnable;
@end

@implementation Darklaf_JNFRunnableWrapper

- (void) invokeRunnable {
    Darklaf_JNFThreadContext ctx = Darklaf_JNFThreadDetachOnThreadDeath | Darklaf_JNFThreadSetSystemClassLoaderOnAttach | Darklaf_JNFThreadAttachAsDaemon;
    JNIEnv *env = Darklaf_JNFObtainEnv(&ctx);
    Darklaf_JNFCallVoidMethod(env, [self jObjectWithEnv:env], jm_run);
    Darklaf_JNFReleaseEnv(env, &ctx);
}

@end


@implementation Darklaf_JNFRunnable

+ (NSInvocation *) invocationWithRunnable:(jobject)runnable withEnv:(JNIEnv *)env {
    SEL sel = @selector(invokeRunnable);
    NSMethodSignature *sig = [Darklaf_JNFRunnableWrapper instanceMethodSignatureForSelector:sel];
    NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:sig];
    [invocation retainArguments];
    [invocation setSelector:sel];

    Darklaf_JNFRunnableWrapper *runnableWrapper = [[Darklaf_JNFRunnableWrapper alloc] initWithJObject:runnable withEnv:env];
    [invocation setTarget:runnableWrapper];
    [runnableWrapper release];

    return invocation;
}

#if __BLOCKS__
+ (void(^)(void)) blockWithRunnable:(jobject)runnable withEnv:(JNIEnv *)env {
    Darklaf_JNFJObjectWrapper *runnableWrapper = [Darklaf_JNFJObjectWrapper wrapperWithJObject:runnable withEnv:env];

    return [[^() {
        Darklaf_JNFThreadContext ctx = Darklaf_JNFThreadDetachOnThreadDeath | Darklaf_JNFThreadSetSystemClassLoaderOnAttach | Darklaf_JNFThreadAttachAsDaemon;
        JNIEnv *_block_local_env = Darklaf_JNFObtainEnv(&ctx);
        Darklaf_JNFCallVoidMethod(env, [runnableWrapper jObjectWithEnv:_block_local_env], jm_run);
        Darklaf_JNFReleaseEnv(_block_local_env, &ctx);
    } copy] autorelease];
}
#endif

@end
