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
#import <JavaNativeFoundation/JavaNativeFoundation.h>
#import <AppKit/AppKit.h>

#define OBJC(jl) ((id)jlong_to_ptr(jl))

#define KEY_ACCENT_COLOR @"AppleAccentColor"
#define KEY_SELECTION_COLOR @"selectedTextBackgroundColor"
#define KEY_SYSTEM_COLOR_LIST @"System"

#define EVENT_ACCENT_COLOR @"AppleColorPreferencesChangedNotification"
#define EVENT_AQUA_CHANGE @"AppleAquaColorVariantChanged"
#define EVENT_THEME_CHANGE @"AppleInterfaceThemeChangedNotification"
#define EVENT_HIGH_CONTRAST @"AXInterfaceIncreaseContrastStatusDidChange"
#define EVENT_COLOR_CHANGE NSSystemColorsDidChangeNotification

#define VALUE_DEFAULT_ACCENT_COLOR (-2)
#define VALUE_NO_ACCENT_COLOR (-100)
#define VALUE_NO_SELECTION_COLOR (-1)

@interface PreferenceChangeListener:NSObject {
    @public JavaVM *jvm;
    @public jobject callback;
}
@end

@implementation PreferenceChangeListener
- (id)initWithJVM:(JavaVM *)jvm_ andCallBack:(jobject)callback_ {
    self = [super init];
    self->jvm = jvm_;
    self->callback = callback_;

    NSDistributedNotificationCenter *center = [NSDistributedNotificationCenter defaultCenter];
    if (@available(macOS 10.14, *)) {
        [self listenToKey:EVENT_ACCENT_COLOR onCenter:center];
        [self listenToKey:EVENT_AQUA_CHANGE onCenter:center];
        [self listenToKey:EVENT_THEME_CHANGE onCenter:center];
        [self listenToKey:EVENT_COLOR_CHANGE onCenter:center];
    }
    [self listenToKey:EVENT_HIGH_CONTRAST onCenter:center];
    return self;
}

- (void)dealloc {
    NSDistributedNotificationCenter *center = [NSDistributedNotificationCenter defaultCenter];
    [center removeObserver:self]; // Removes all registered notifications.
    [super dealloc];
}

- (void)listenToKey:(NSString *)key onCenter:(NSDistributedNotificationCenter *)center {
     [center addObserver:self
                selector:@selector(notificationEvent:)
                    name:key
                  object:nil];
}

- (void)runCallback {
    if (!jvm) return;
    JNIEnv *env;
    BOOL detach = NO;
    int getEnvStat = jvm->GetEnv((void **)&env, JNI_VERSION_1_6);
    if (getEnvStat == JNI_EDETACHED) {
        detach = YES;
        if (jvm->AttachCurrentThread((void **) &env, NULL) != 0) return;
    } else if (getEnvStat == JNI_EVERSION) {
        return;
    }
    jclass runnableClass = env->GetObjectClass(callback);
    jmethodID runMethodId = env->GetMethodID(runnableClass, "run", "()V");
    if (runMethodId) {
        env->CallVoidMethod(callback, runMethodId);
    }
    if (env->ExceptionCheck()) {
        env->ExceptionDescribe();
    }
    if (detach) jvm->DetachCurrentThread();
}

- (void)notificationEvent:(NSNotification *)notification {
    [self runCallback];
}

@end

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_isHighContrastEnabled(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    return (jboolean) NSWorkspace.sharedWorkspace.accessibilityDisplayShouldIncreaseContrast;
JNF_COCOA_EXIT(env);
    return (jboolean) NO;
}

JNIEXPORT jint JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_nativeGetAccentColor(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    if (@available(macOS 10.14, *)) {
        BOOL hasAccentSet = ([[NSUserDefaults standardUserDefaults] objectForKey:KEY_ACCENT_COLOR] != nil);
        if (hasAccentSet) {
            return (jint) ([[NSUserDefaults standardUserDefaults] integerForKey:KEY_ACCENT_COLOR]);
        }
        return (jint) VALUE_DEFAULT_ACCENT_COLOR;
    }
    return (jint) VALUE_NO_ACCENT_COLOR;
JNF_COCOA_EXIT(env);
    return (jint) VALUE_NO_ACCENT_COLOR;
}

JNIEXPORT jint JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_nativeGetSelectionColor(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    if (@available(macOS 10.14, *)) {
        NSColorSpace *rgbSpace = [NSColorSpace genericRGBColorSpace];
        NSColor *accentColor = [[[NSColorList colorListNamed:KEY_SYSTEM_COLOR_LIST]
                                                colorWithKey:KEY_SELECTION_COLOR]
                                        colorUsingColorSpace:rgbSpace];
        NSInteger r = (NSInteger) (255 * [accentColor redComponent]);
        NSInteger g = (NSInteger) (255 * [accentColor greenComponent]);
        NSInteger b = (NSInteger) (255 * [accentColor blueComponent]);
        return (jint) (0xff000000 | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | ((b & 0xFF) << 0));
    }
    return (jint) VALUE_NO_SELECTION_COLOR;
JNF_COCOA_EXIT(env);
    return (jint) VALUE_NO_SELECTION_COLOR;
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_createPreferenceChangeListener(JNIEnv *env, jclass obj, jobject callback) {
JNF_COCOA_DURING(env); // We dont want an auto release pool.
    JavaVM *jvm;
    if (env->GetJavaVM(&jvm) == 0) {
        jobject callbackRef = env->NewGlobalRef(callback);
        PreferenceChangeListener *listener = [[PreferenceChangeListener alloc] initWithJVM:jvm andCallBack: callbackRef];
        [listener retain];
        return reinterpret_cast<jlong>(listener);
    }
    return (jlong) 0;
JNF_COCOA_HANDLE(env);
    return (jlong) 0;
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_deletePreferenceChangeListener(JNIEnv *env, jclass obj, jlong listenerPtr) {
JNF_COCOA_ENTER(env);
    PreferenceChangeListener *listener = OBJC(listenerPtr);
    if (listener) {
        env->DeleteGlobalRef(listener->callback);
        [listener release];
        [listener dealloc];
    }
JNF_COCOA_EXIT(env);
}
