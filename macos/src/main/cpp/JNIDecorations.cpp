/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 */
#import "com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS.h"
#import <Cocoa/Cocoa.h>

#define OBJC(jl) ((id)((void*)(jl)))
#define FULL_WINDOW_CONTENT (1 << 15)
#define TRANSPARENT_TITLE_BAR (1 << 18)

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_installDecorations(JNIEnv *env, jclass obj, jlong hwnd)
{
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask |= FULL_WINDOW_CONTENT;
        nsWindow.styleMask |= TRANSPARENT_TITLE_BAR;
    });
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_uninstallDecorations(JNIEnv *env, jclass obj, jlong hwnd)
{
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask &= ~FULL_WINDOW_CONTENT;
        nsWindow.styleMask &= ~TRANSPARENT_TITLE_BAR;
    });
}