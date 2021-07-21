/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
#ifdef _MSC_VER
#define alloca _alloca
#pragma warning( disable : 4152 ) /* function/data conversion */
#pragma warning( disable : 4054 ) /* cast function pointer to data pointer */
#pragma warning( disable : 4055 ) /* cast data pointer to function pointer */
#pragma warning( disable : 4204 ) /* structure initializer */
#pragma warning( disable : 4710 ) /* swprintf not inlined */
#pragma warning( disable : 4201 ) /* nameless struct/union (jni_md.h) */
#else
#include <malloc.h>
#endif /* _MSC_VER */

#define MSG_SIZE 1024

#define GET_LAST_ERROR() GetLastError()

#define EIllegalArgument "java/lang/IllegalArgumentException"
#define EOutOfMemory "java/lang/OutOfMemoryError"
#define EUnsatisfiedLink "java/lang/UnsatisfiedLinkError"
#define EIllegalState "java/lang/IllegalStateException"
#define EUnsupportedOperation "java/lang/UnsupportedOperationException"
#define ERuntime "java/lang/RuntimeException"
#define EError "java/lang/Error"
#define ELastError "com/sun/jna/LastErrorException"

#define JAWT_NAME "jawt.dll"

#define LOAD_LIBRARY(NAME,OPTS) (NAME ? LoadLibraryExW(NAME, NULL, OPTS) : GetModuleHandleW(NULL))
#define LOAD_ERROR(BUF,LEN) w32_format_error(GetLastError(), BUF, LEN)
#define FIND_ENTRY(HANDLE, NAME) w32_find_entry(env, HANDLE, NAME)

#if defined(_WIN64)
    #define A2L(X) ((jlong)(X))
#else
#define A2L(X) ((jlong)(unsigned long)(X))
#endif

#if defined(_WIN64)
    #define METHOD_NAME "JAWT_GetAWT"
#else
#define METHOD_NAME "_JAWT_GetAWT@8"
#endif
