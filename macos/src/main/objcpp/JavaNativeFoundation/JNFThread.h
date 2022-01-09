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
 * Functions to help obtain a JNIEnv pointer in places where one cannot be passed
 * though (callbacks, catagory functions, etc). Use sparingly.
 */

#import <os/availability.h>
#import "JNFJNI.h"

__BEGIN_DECLS

// Options only apply if thread was not already attached to the JVM.
enum {
    Darklaf_JNFThreadDetachImmediately = (1 << 1),
    Darklaf_JNFThreadDetachOnThreadDeath = (1 << 2),
    Darklaf_JNFThreadSetSystemClassLoaderOnAttach = (1 << 3),
    Darklaf_JNFThreadAttachAsDaemon = (1 << 4)
};

typedef jlong Darklaf_JNFThreadContext;


/*
 * Attaches the current thread to the Java VM if needed, and obtains a JNI environment
 * to interact with the VM. Use a provided JNIEnv pointer for your current thread
 * whenever possible, since this method is particularly expensive to the Java VM if
 * used repeatedly.
 *
 * Provide a pointer to a Darklaf_JNFThreadContext to pass to Darklaf_JNFReleaseEnv().
 */
Darklaf_JNF_EXPORT extern JNIEnv *Darklaf_JNFObtainEnv(Darklaf_JNFThreadContext *context) API_DEPRECATED("This functionality is no longer supported and may stop working in a future version of macOS.", macos(10.10, 10.16));

/*
 * Release the JNIEnv for this thread, and detaches the current thread from the VM if
 * it was not already attached.
 */
Darklaf_JNF_EXPORT extern void Darklaf_JNFReleaseEnv(JNIEnv *env, Darklaf_JNFThreadContext *context) API_DEPRECATED("This functionality is no longer supported and may stop working in a future version of macOS.", macos(10.10, 10.16));


#if __BLOCKS__

/*
 * Performs the same attach/detach as Darklaf_JNFObtainEnv() and Darklaf_JNFReleaseEnv(), but executes a
 * block that accepts the obtained JNIEnv.
 */
typedef void (^JNIEnvBlock)(JNIEnv *);
Darklaf_JNF_EXPORT extern void Darklaf_JNFPerformEnvBlock(Darklaf_JNFThreadContext context, JNIEnvBlock block) API_DEPRECATED("This functionality is no longer supported and may stop working in a future version of macOS.", macos(10.10, 10.16));

#endif

__END_DECLS
