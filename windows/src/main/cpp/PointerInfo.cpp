/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include "com_github_weisj_darklaf_platform_windows_JNIDecorationsWindows.h"
#include <jawt.h>
#include <jawt_md.h>
#include <string>
#include <windows.h>
#include <psapi.h>
#include "PointerInfo.h"

static HMODULE jawt_handle = NULL;
typedef jboolean (JNICALL *PJAWT_GETAWT_T)(JNIEnv*, JAWT*);
static PJAWT_GETAWT_T pJAWT_GetAWT;
#define JAWT_GetAWT (*pJAWT_GetAWT)

/** Throw an exception by name */
static void throwByName(JNIEnv *env, char const *name, char const *msg) {
    jclass cls;

    env->ExceptionClear();

    cls = env->FindClass(name);

    if (cls != NULL) { /* Otherwise an exception has already been thrown */
        env->ThrowNew(cls, msg);

        /* It's a good practice to clean up the local references. */
        env->DeleteLocalRef(cls);
    }
}

static char* w32_format_error(int err, char *buf, int len) {
    wchar_t *wbuf = NULL;
    int wlen = FormatMessageW(
            FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_ALLOCATE_BUFFER, NULL, err, 0,
            (LPWSTR) & wbuf, 0, NULL);
    if (wlen > 0) {
        int result = WideCharToMultiByte(CP_UTF8, 0, wbuf, -1, buf, len, NULL, NULL);
        if (result == 0) {
            fprintf(stderr, "JNA: error converting error message: %d\n", (int) GET_LAST_ERROR());
            *buf = 0;
        } else {
            buf[len - 1] = 0;
        }
    } else {
        // Error retrieving message
        *buf = 0;
    }
    if (wbuf) {
        LocalFree(wbuf);
    }

    return buf;
}

static FARPROC w32_find_entry(JNIEnv *env, HMODULE handle, char const *funname) {
    FARPROC func = NULL;
    if (handle != GetModuleHandle(NULL)) {
        func = GetProcAddress(handle, funname);
    } else {
        HANDLE cur_proc = GetCurrentProcess();
        HMODULE *modules;
        DWORD needed, i;
        if (!EnumProcessModules(cur_proc, NULL, 0, &needed)) {
            fail: throwByName(env, EError, "Unexpected error enumerating modules");
            return 0;
        }
        modules = (HMODULE*) alloca(needed);
        if (!EnumProcessModules(cur_proc, modules, needed, &needed)) {
            goto fail;
        }
        for (i = 0; i < needed / sizeof(HMODULE); i++) {
            func = GetProcAddress(modules[i], funname);
            if (func) {
                break;
            }
        }
    }
    return func;
}

static std::wstring Java_To_WStr(JNIEnv *env, jstring string) {
    std::wstring value;

    jchar const *raw = env->GetStringChars(string, 0);
    jsize len = env->GetStringLength(string);

    value.assign(raw, raw + len);

    env->ReleaseStringChars(string, raw);

    return value;
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIDecorationsWindows_getWindowHWND(JNIEnv *env, jclass, jobject window, jstring java_lib_path) {
    jlong handle = 0;
    JAWT_DrawingSurface* ds;
    JAWT_DrawingSurfaceInfo* dsi;
    jint lock;
    JAWT awt;

    awt.version = JAWT_VERSION_1_4;

#ifdef _WIN32
    std::wstring path_str = Java_To_WStr(env, java_lib_path);
    wchar_t const* path = path_str.c_str();
#undef JAWT_NAME
#define JAWT_NAME path
#endif
    if (!jawt_handle) {
        jawt_handle = LOAD_LIBRARY(JAWT_NAME, LOAD_WITH_ALTERED_SEARCH_PATH);
        if (jawt_handle == NULL) {
            char msg[MSG_SIZE];
            throwByName(env, EUnsatisfiedLink, LOAD_ERROR(msg, sizeof(msg)));
            return -1;
        }
    }
    if (!pJAWT_GetAWT) {
        pJAWT_GetAWT = (PJAWT_GETAWT_T)(FIND_ENTRY(jawt_handle, METHOD_NAME));
        if (pJAWT_GetAWT == NULL) {
            char msg[MSG_SIZE], buf[MSG_SIZE - 31 /* literal characters */- sizeof(METHOD_NAME)];
            snprintf(msg, sizeof(msg), "Error looking up JAWT method %s: %s",
                    METHOD_NAME, LOAD_ERROR(buf, sizeof(buf)));
            throwByName(env, EUnsatisfiedLink, msg);
            return -1;
        }
    }
    if (!JAWT_GetAWT(env, &awt)) {
        throwByName(env, EUnsatisfiedLink, "Can't load JAWT");
        return 0;
    }
    ds = awt.GetDrawingSurface(env, window);
    if (ds == NULL) {
        throwByName(env, EError, "Can't get drawing surface");
        return -1;
    } else {
        lock = ds->Lock(ds);
        if ((lock & JAWT_LOCK_ERROR) != 0) {
            awt.FreeDrawingSurface(ds);
            throwByName(env, EError, "Can't get drawing surface lock");
            return 0;
        }
        dsi = ds->GetDrawingSurfaceInfo(ds);
        if (dsi == NULL) {
            throwByName(env, EError, "Can't get drawing surface info");
        }
        else {
            JAWT_Win32DrawingSurfaceInfo* wdsi = (JAWT_Win32DrawingSurfaceInfo*)dsi->platformInfo;
            if (wdsi != NULL) {
                handle = A2L(wdsi->hwnd);
                if (!handle) {
                    throwByName(env, EIllegalState, "Can't get HWND");
                }
            } else {
                throwByName(env, EError, "Can't get w32 platform info");
            }
            ds->FreeDrawingSurfaceInfo(dsi);
        }
        ds->Unlock(ds);
        awt.FreeDrawingSurface(ds);
        return handle;
    }
}
