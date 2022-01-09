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

#import "JNFObject.h"

#import "JNFJNI.h"
#import "JNFString.h"

static Darklaf_JNF_CLASS_CACHE(sjc_Object, "java/lang/Object");

BOOL Darklaf_JNFObjectEquals(JNIEnv* env, jobject a, jobject b)
{
    if ((a == NULL) && (b == NULL)) return YES;
    if ((a == NULL) || (b == NULL)) return NO;

    static Darklaf_JNF_MEMBER_CACHE(jm_equals, sjc_Object, "equals", "(Ljava/lang/Object;)Z");
    return (BOOL)Darklaf_JNFCallBooleanMethod(env, a, jm_equals, b); // AWT_THREADING Safe (!appKit)
}

NSString *Darklaf_JNFObjectToString(JNIEnv *env, jobject obj)
{
    static Darklaf_JNF_MEMBER_CACHE(jm_toString, sjc_Object, "toString", "()Ljava/lang/String;");
    jobject name = Darklaf_JNFCallObjectMethod(env, obj, jm_toString); // AWT_THREADING Safe (known object)

    id result = Darklaf_JNFJavaToNSString(env, name);
    (*env)->DeleteLocalRef(env, name);
    return result;
}

NSString *Darklaf_JNFObjectClassName(JNIEnv* env, jobject obj)
{
    static Darklaf_JNF_MEMBER_CACHE(jm_getClass, sjc_Object, "getClass", "()Ljava/lang/Class;");

    jobject clz = Darklaf_JNFCallObjectMethod(env, obj, jm_getClass); // AWT_THREADING Safe (known object)
    NSString *result = Darklaf_JNFObjectToString(env, clz);
    (*env)->DeleteLocalRef(env, clz);
    return result;
}
