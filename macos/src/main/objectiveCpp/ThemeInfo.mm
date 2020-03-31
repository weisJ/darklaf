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
#import "com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS.h"
#import <AppKit/AppKit.h>

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_isDarkThemeEnabled(JNIEnv *env, jclass obj) {
    if(@available(macOS 10.14, *)) {
        return (jboolean)false;
    } else {
        return (jboolean)false;
    }
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_isHighContrastEnabled(JNIEnv *env, jclass obj) {
    return NSWorkspace.shared.accessibilityDisplayShouldIncreaseContrast;
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_getFontScaleFactor(JNIEnv *env, jclass obj) {
    return 100;
}
