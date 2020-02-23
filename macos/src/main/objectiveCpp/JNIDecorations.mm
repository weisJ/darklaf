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

NSTextField* findView(NSArray<__kindof NSView *> *subviews) {
    for (NSView *view in subviews) {
        if ([view isKindOfClass:[NSTextField class]]) {
            return (NSTextField *)view;
        } else {
            return findView([view subviews]);
        }
    }
    return nil;
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_setTitleColor(JNIEnv *env, jclass obj, jlong hwnd,
                                                                               jint r, jint g, jint b) {
    NSWindow *nsWindow = OBJC(hwnd);
    NSView *contentView = [nsWindow contentView];
    NSTextField *textField = findView([[contentView superview] subviews]);
    if (textField == nil) {
        printf("TextField not found");
        return;
    }
    NSColor *color = [NSColor colorWithCalibratedRed:r / 255.0 green:g / 255.0 blue:b / 255.0 alpha:1.0f];
    dispatch_async(dispatch_get_main_queue(), ^{
        textField.textColor = color;
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
     });
}


JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_installDecorations(JNIEnv *env, jclass obj, jlong hwnd) {
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask |= NSWindowStyleMaskFullSizeContentView;
        nsWindow.titlebarAppearsTransparent = true;
    });
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIDecorationsMacOS_uninstallDecorations(JNIEnv *env, jclass obj, jlong hwnd) {
    NSWindow *nsWindow = OBJC(hwnd);
    dispatch_async(dispatch_get_main_queue(), ^{
        nsWindow.styleMask &= ~NSWindowStyleMaskFullSizeContentView;
        nsWindow.titlebarAppearsTransparent = false;
    });
}
