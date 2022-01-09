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

#import "JNFDate.h"
#import "JNFJNI.h"


static Darklaf_JNF_CLASS_CACHE(sjc_Calendar, "java/util/Calendar");
static Darklaf_JNF_CLASS_CACHE(sjc_Date, "java/util/Date");

Darklaf_JNF_EXPORT extern NSTimeInterval Darklaf_JNFJavaMillisToNSTimeInterval(jlong javaMillisSince1970)
{
    return (NSTimeInterval)(((double)javaMillisSince1970 / 1000.0) - NSTimeIntervalSince1970);
}

Darklaf_JNF_EXPORT extern jlong Darklaf_JNFNSTimeIntervalToJavaMillis(NSTimeInterval intervalSince2001)
{
    return (jlong)((intervalSince2001 + NSTimeIntervalSince1970) * 1000.0);
}

Darklaf_JNF_EXPORT extern NSDate *Darklaf_JNFJavaToNSDate(JNIEnv *env, jobject date)
{
    if (date == NULL) return nil;

    jlong millis = 0;
    if (Darklaf_JNFIsInstanceOf(env, date, &sjc_Calendar)) {
        static Darklaf_JNF_MEMBER_CACHE(jm_getTimeInMillis, sjc_Calendar, "getTimeInMillis", "()J");
        millis = Darklaf_JNFCallLongMethod(env, date, jm_getTimeInMillis);
    } else if (Darklaf_JNFIsInstanceOf(env, date, &sjc_Date)) {
        static Darklaf_JNF_MEMBER_CACHE(jm_getTime, sjc_Date, "getTime", "()J");
        millis = Darklaf_JNFCallLongMethod(env, date, jm_getTime);
    }

    if (millis == 0) {
        return nil;
    }

    return [NSDate dateWithTimeIntervalSince1970:((double)millis / 1000.0)];
}

Darklaf_JNF_EXPORT extern jobject Darklaf_JNFNSToJavaCalendar(JNIEnv *env, NSDate *date)
{
    if (date == nil) return NULL;

    const jlong millis = (jlong)([date timeIntervalSince1970] * 1000.0);

    static Darklaf_JNF_STATIC_MEMBER_CACHE(jsm_getInstance, sjc_Calendar, "getInstance", "()Ljava/util/Calendar;");
    jobject calendar = Darklaf_JNFCallStaticObjectMethod(env, jsm_getInstance);

    static Darklaf_JNF_MEMBER_CACHE(jm_setTimeInMillis, sjc_Calendar, "setTimeInMillis", "(J)V");
    Darklaf_JNFCallVoidMethod(env, calendar, jm_setTimeInMillis, millis);

    return calendar;
}
