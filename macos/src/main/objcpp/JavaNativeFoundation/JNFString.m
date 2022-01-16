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

#import "JNFString.h"

#import "JNFJNI.h"
#import "JNFAssert.h"
#import "debug.h"

#define STACK_BUFFER_SIZE 64

/*
 * Utility function to convert java String to NSString. We don't go through intermediate cString
 * representation, since we are trying to preserve unicode characters from Java to NSString.
 */
NSString *Darklaf_JNFJavaToNSString(JNIEnv *env, jstring javaString)
{
    // We try very hard to only allocate and memcopy once.
    if (javaString == NULL) return nil;

    jsize length = env->GetStringLength(javaString);
    unichar *buffer = (unichar *)calloc((size_t)length, sizeof(unichar));
    env->GetStringRegion(javaString, 0, length, buffer);
    NSString *str = (NSString *)CFStringCreateWithCharactersNoCopy(NULL, buffer, length, kCFAllocatorMalloc);
    //	NSLog(@"%@", str);
    return [(NSString *)CFMakeCollectable(str) autorelease];
}

/*
 * Utility function to convert NSString to Java string. We don't go through intermediate cString
 * representation, since we are trying to preserve unicode characters in translation.
 */
jstring Darklaf_JNFNSToJavaString(JNIEnv *env, NSString *nsString)
{
    jstring res = nil;
    if (nsString == nil) return NULL;

    unsigned long length = [nsString length];
    unichar *buffer;
    unichar stackBuffer[STACK_BUFFER_SIZE];
    if (length > STACK_BUFFER_SIZE) {
        buffer = (unichar *)calloc(length, sizeof(unichar));
    } else {
        buffer = stackBuffer;
    }

    Darklaf_JNF_ASSERT_COND(buffer != NULL);
    [nsString getCharacters:buffer];
    res = env->NewString(buffer, (jsize)length);
    if (buffer != stackBuffer) free(buffer);
    return res;
}

const unichar *Darklaf_JNFGetStringUTF16UniChars(JNIEnv *env, jstring javaString)
{
    const jchar *unichars = NULL;
    Darklaf_JNF_ASSERT_COND(javaString != NULL);
    unichars = env->GetStringChars(javaString, NULL);
    if (unichars == NULL) [Darklaf_JNFException raise:env as:kNullPointerException reason:"unable to obtain characters from GetStringChars"];
    return (const unichar *)unichars;
}

void Darklaf_JNFReleaseStringUTF16UniChars(JNIEnv *env, jstring javaString, const unichar *unichars)
{
    Darklaf_JNF_ASSERT_COND(unichars != NULL);
    env->ReleaseStringChars(javaString, (const jchar *)unichars);
}

const char *Darklaf_JNFGetStringUTF8Chars(JNIEnv *env, jstring javaString)
{
    const char *chars = NULL;
    Darklaf_JNF_ASSERT_COND(javaString != NULL);
    chars = env->GetStringUTFChars(javaString, NULL);
    if (chars == NULL) [Darklaf_JNFException raise:env as:kNullPointerException reason:"unable to obtain characters from GetStringUTFChars"];
    return chars;
}

void Darklaf_JNFReleaseStringUTF8Chars(JNIEnv *env, jstring javaString, const char *chars)
{
    Darklaf_JNF_ASSERT_COND(chars != NULL);
    env->ReleaseStringUTFChars(javaString, (const char *)chars);
}
