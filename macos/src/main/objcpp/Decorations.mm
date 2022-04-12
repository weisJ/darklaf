/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
#import "com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS.h"
#import "JNFUtils.h"
#import <Cocoa/Cocoa.h>

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getComponentPointer(JNIEnv *env, jclass cls, jobject window) {
JNF_COCOA_ENTER(env);
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
JNF_COCOA_EXIT(env);
    return 0;
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_retainWindow(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    [nsWindow retain];
JNF_COCOA_EXIT(env);
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_releaseWindow(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    [nsWindow release];
JNF_COCOA_EXIT(env);
}

JNIEXPORT jdouble JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getTitleBarHeight(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    NSRect frame = NSMakeRect(0.0, 0.0, 100.0, 100.0);
    NSUInteger windowStyle = nsWindow.styleMask & ~NSWindowStyleMaskFullSizeContentView;
    NSRect rect = [NSWindow contentRectForFrameRect:frame styleMask:windowStyle];
    return (jdouble)(frame.size.height - rect.size.height);
JNF_COCOA_EXIT(env);
    return 0;
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_isFullscreen(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    return (jboolean)(([nsWindow styleMask] & NSWindowStyleMaskFullScreen) != 0);
JNF_COCOA_EXIT(env);
    return NO;
}

JNIEXPORT jdouble JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_getTitleFontSize(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    return (jdouble)[[NSFont titleBarFontOfSize:0] pointSize];
JNF_COCOA_EXIT(env);
    return 0;
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_setTitleEnabled(JNIEnv *env, jclass obj, jlong hwnd,
                                                                                 jboolean enabled) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    [JNF_RunLoop performOnMainThreadWaiting:YES withBlock:^{
        if (enabled) {
            nsWindow.titleVisibility = NSWindowTitleVisible;
        } else {
            nsWindow.titleVisibility = NSWindowTitleHidden;
        }
        [nsWindow contentView].needsDisplay = TRUE;
    }];
JNF_COCOA_EXIT(env);
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_setDarkTheme(JNIEnv *env, jclass obj, jlong hwnd,
                                                                              jboolean darkEnabled) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    if(@available(macOS 10.14, *)) {
        [JNF_RunLoop performOnMainThreadWaiting:YES withBlock:^{
            if (darkEnabled) {
                nsWindow.appearance = [NSAppearance appearanceNamed:@"NSAppearanceNameDarkAqua"];
            } else {
                nsWindow.appearance = [NSAppearance appearanceNamed:NSAppearanceNameAqua];
            }
            [nsWindow contentView].needsDisplay = YES;
        }];
    }
JNF_COCOA_EXIT(env);
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_installDecorations(JNIEnv *env, jclass obj, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    [JNF_RunLoop performOnMainThreadWaiting:YES withBlock:^{
        nsWindow.styleMask |= NSWindowStyleMaskFullSizeContentView;
        nsWindow.titlebarAppearsTransparent = YES;
        [nsWindow contentView].needsDisplay = YES;
    }];
JNF_COCOA_EXIT(env);
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_uninstallDecorations(JNIEnv *env, jclass obj, jlong hwnd,
    jboolean fullSizeContent, jboolean transparentTitleBar) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    [JNF_RunLoop performOnMainThreadWaiting:YES withBlock:^{
        if (fullSizeContent) {
            nsWindow.styleMask |= NSWindowStyleMaskFullSizeContentView;
        } else {
            nsWindow.styleMask &= ~NSWindowStyleMaskFullSizeContentView;
        }
        nsWindow.titlebarAppearsTransparent = transparentTitleBar ? YES : NO;
        [nsWindow contentView].needsDisplay = YES;
    }];
JNF_COCOA_EXIT(env);
}

NSRect join(NSRect r, NSButton* button) {
    NSRect buttonRect = [button convertRect: [button bounds] toView: nil];
    return NSUnionRect(r, buttonRect);
}

JNIEXPORT jfloatArray JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_windowButtonRect(JNIEnv *env, jclass, jlong hwnd) {
JNF_COCOA_ENTER(env);
    NSWindow *nsWindow = OBJC(hwnd);
    NSButton *closeButton = [nsWindow standardWindowButton: NSWindowCloseButton];
    NSButton *minimizeButton = [nsWindow standardWindowButton: NSWindowMiniaturizeButton];
    NSButton *zoomButton = [nsWindow standardWindowButton: NSWindowZoomButton];

    NSRect rect = NSMakeRect(0,0, -1, -1);
    if (closeButton) rect = join(rect, closeButton);
    if (minimizeButton) rect = join(rect, minimizeButton);
    if (zoomButton) rect = join(rect, zoomButton);

    jfloatArray bounds = env->NewFloatArray(4);
    jfloat rawBounds[4] = {rect.origin.x, rect.origin.y, rect.size.width, rect.size.height};
    env->SetFloatArrayRegion(bounds, 0, 0, rawBounds);

    return bounds;
JNF_COCOA_EXIT(env);
}
