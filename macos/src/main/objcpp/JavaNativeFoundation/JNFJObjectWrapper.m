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

#import "JNFJObjectWrapper.h"

#import "JNFJNI.h"
#import "JNFThread.h"

@interface Darklaf_JNFJObjectWrapper ()
@property (readwrite, nonatomic, assign) jobject jObject;
@end

@implementation Darklaf_JNFJObjectWrapper

- (jobject) _getWithEnv:(__unused JNIEnv *)env {
    return self.jObject;
}

- (jobject) _createObj:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    return Darklaf_JNFNewGlobalRef(env, jObjectIn);
}

- (void) _destroyObj:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    Darklaf_JNFDeleteGlobalRef(env, jObjectIn);
}

+ (Darklaf_JNFJObjectWrapper *) wrapperWithJObject:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    return [[[Darklaf_JNFJObjectWrapper alloc] initWithJObject:jObjectIn withEnv:env] autorelease];
}

- (id) initWithJObject:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    self = [super init];
    if (!self) return self;

    if (jObjectIn) {
        self.jObject = [self _createObj:jObjectIn withEnv:env];
    }

    return self;
}

- (jobject) jObjectWithEnv:(JNIEnv *)env {
    jobject validObj = [self _getWithEnv:env];
    if (!validObj) return NULL;

    return env->NewLocalRef(validObj);
}

- (void) setJObject:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    jobject const jobj = self.jObject;
    if (jobj == jObjectIn) return;

    if (jobj) {
        [self _destroyObj:jobj withEnv:env];
    }

    if (jObjectIn) {
        self.jObject = [self _createObj:jObjectIn withEnv:env];
    } else {
        self.jObject = NULL;
    }
}

- (void) clearJObjectReference {
    jobject const jobj = self.jObject;
    if (!jobj) return;

    Darklaf_JNFThreadContext threadContext = Darklaf_JNFThreadDetachImmediately;
    JNIEnv *env = Darklaf_JNFObtainEnv(&threadContext);
    if (env == NULL) return; // leak?

    [self _destroyObj:jobj withEnv:env];
    self.jObject = NULL;

    Darklaf_JNFReleaseEnv(env, &threadContext);
}

- (void) dealloc {
    [self clearJObjectReference];
    [super dealloc];
}

@end


@implementation Darklaf_JNFWeakJObjectWrapper

+ (Darklaf_JNFWeakJObjectWrapper *) wrapperWithJObject:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    return [[[Darklaf_JNFWeakJObjectWrapper alloc] initWithJObject:jObjectIn withEnv:env] autorelease];
}

- (jobject) _getWithEnv:(JNIEnv *)env {
    jobject const jobj = self.jObject;

    if (env->IsSameObject(jobj, NULL) == JNI_TRUE) {
        self.jObject = NULL; // object went invalid
        return NULL;
    }
    return jobj;
}

- (jobject) _createObj:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    return Darklaf_JNFNewWeakGlobalRef(env, jObjectIn);
}

- (void) _destroyObj:(jobject)jObjectIn withEnv:(JNIEnv *)env {
    Darklaf_JNFDeleteWeakGlobalRef(env, jObjectIn);
}

@end
