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
 * This umbrella header should be included by all JNI/Cocoa source files
 * for easy building. Use this file instead of importing individual Darklaf_JNF
 * headers.
 */

#import <JavaNativeFoundation/Darklaf_JNFJNI.h>
#import <JavaNativeFoundation/Darklaf_JNFObject.h>
#import <JavaNativeFoundation/Darklaf_JNFJObjectWrapper.h>
#import <JavaNativeFoundation/Darklaf_JNFString.h>
#import <JavaNativeFoundation/Darklaf_JNFNumber.h>
#import <JavaNativeFoundation/Darklaf_JNFDate.h>
#import <JavaNativeFoundation/Darklaf_JNFPath.h>
#import <JavaNativeFoundation/Darklaf_JNFTypeCoercion.h>
#import <JavaNativeFoundation/Darklaf_JNFThread.h>
#import <JavaNativeFoundation/Darklaf_JNFRunnable.h>
#import <JavaNativeFoundation/Darklaf_JNFRunLoop.h>
#import <JavaNativeFoundation/Darklaf_JNFException.h>
#import <JavaNativeFoundation/Darklaf_JNFAssert.h>
