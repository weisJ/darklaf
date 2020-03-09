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

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getComponentPointer(JNIEnv *env, jclass cls, jobject window) {
    if (!env || !window) return 0;
    jclass class_window = env->GetObjectClass(window);
    jfieldID fid_peer = env->GetFieldID(class_window, "peer", "Ljava/awt/peer/ComponentPeer;");

    if (!fid_peer) return 0;
    jobject peer = env->GetObjectField(window, fid_peer);

    if (!peer) return 0;
    jclass class_peer = env->GetObjectClass(peer);
    jfieldID fid_platformWindow = env->GetFieldID(class_peer, "platformWindow", "Lsun/lwawt/PlatformWindow;");

    if (!fid_platformWindow) return 0;
    jobject platformWindow = env->GetObjectField(peer, fid_platformWindow);

    if (!platformWindow) return 0;
    jclass class_platformWindow = env->GetObjectClass(platformWindow);
    jfieldID fid_ptr = env->GetFieldID(class_platformWindow, "ptr", "J");
    jlong ptr = env->GetLongField(platformWindow, fid_ptr);
    return ptr;
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_retainWindow(JNIEnv *env, jclass obj, jlong hwnd) {
     NSWindow *nsWindow = OBJC(hwnd);
       [nsWindow retain];
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_releaseWindow(JNIEnv *env, jclass obj, jlong hwnd) {
       NSWindow *nsWindow = OBJC(hwnd);
       //Ensure any queued operation on nsWindow is finished first.
       dispatch_async(dispatch_get_main_queue(), ^{
            [nsWindow release];
       });
}

JNIEXPORT jdouble JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getTitleBarHeight(JNIEnv *env, jclass obj, jlong hwnd) {
     NSWindow *nsWindow = OBJC(hwnd);
     CGFloat windowFrameHeight = nsWindow.contentView.frame.size.height;
     CGFloat contentLayoutRectHeight  = nsWindow.contentLayoutRect.size.height;
     return (jdouble)(windowFrameHeight - contentLayoutRectHeight);
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_isFullscreen(JNIEnv *env, jclass obj, jlong hwnd) {
    NSWindow *nsWindow = OBJC(hwnd);
    return (jboolean)(([nsWindow styleMask] & NSWindowStyleMaskFullScreen) != 0);
}

JNIEXPORT jdouble JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getTitleFontSize(JNIEnv *env, jclass obj, jlong hwnd) {
    return (jdouble)[[NSFont titleBarFontOfSize:0] pointSize];
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_setTitleEnabled(JNIEnv *env, jclass obj, jlong hwnd,
                                                                                 jboolean enabled) {
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        if (enabled) {
            nsWindow.titleVisibility = NSWindowTitleVisible;
        } else {
            nsWindow.titleVisibility = NSWindowTitleHidden;
        }
        [nsWindow contentView].needsDisplay = true;
    });
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_setDarkTheme(JNIEnv *env, jclass obj, jlong hwnd,
                                                                              jboolean darkEnabled) {
     NSWindow *nsWindow = OBJC(hwnd);
     dispatch_async(dispatch_get_main_queue(), ^{
          if (darkEnabled) {
            nsWindow.appearance = [NSAppearance appearanceNamed:NSAppearanceNameDarkAqua];
          } else {
            nsWindow.appearance = [NSAppearance appearanceNamed:NSAppearanceNameAqua];
          }
          [nsWindow contentView].needsDisplay = true;
     });
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_installDecorations(JNIEnv *env, jclass obj, jlong hwnd) {
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask |= NSWindowStyleMaskFullSizeContentView;
        nsWindow.titlebarAppearsTransparent = true;
        [nsWindow contentView].needsDisplay = true;
    });
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_uninstallDecorations(JNIEnv *env, jclass obj, jlong hwnd) {
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask &= ~NSWindowStyleMaskFullSizeContentView;
        nsWindow.titlebarAppearsTransparent = false;
        [nsWindow contentView].needsDisplay = true;
    });
}
