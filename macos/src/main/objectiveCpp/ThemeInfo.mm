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

#define KEY_APPLE_INTERFACE_STYLE @"AppleInterfaceStyle"
#define KEY_SWITCHES_AUTOMATICALLY @"AppleInterfaceStyleSwitchesAutomatically"
#define KEY_ACCENT_COLOR @"AppleAccentColor"

#define EVENT_ACCENT_COLOR @"AppleColorPreferencesChangedNotification"
#define EVENT_THEME_CHANGE @"AppleInterfaceThemeChangedNotification"
#define EVENT_HIGH_CONTRAS @"AXInterfaceIncreaseContrastStatusDidChange"
#define EVENT_COLOR_CHANGE NSSystemColorsDidChangeNotification

#define VALUE_DARK @"Dark"
#define VALUE_NO_ACCENT_COLOR (0)
#define VALUE_NO_SELECTION_COLOR (-1)

@interface PreferenceChangeListener:NSObject

@property NSCondition *condition;
@property BOOL notified;

- (void)notifyLock;

@end

@implementation PreferenceChangeListener
- (id)init {
    _condition = [[NSCondition alloc] init];
    NSDistributedNotificationCenter *center = [NSDistributedNotificationCenter defaultCenter];
    [self listenToKey:EVENT_ACCENT_COLOR onCenter:center];
    [self listenToKey:EVENT_THEME_CHANGE onCenter:center];
    [self listenToKey:EVENT_HIGH_CONTRAS onCenter:center];
    [self listenToKey:EVENT_COLOR_CHANGE onCenter:center];
    return self;
}

- (void)dealloc {
    NSDistributedNotificationCenter *center = [NSDistributedNotificationCenter defaultCenter];
    [center removeObserver:self]; // Removes all registered notifications.
    [self notifyLock];
    [_condition release];
    [super dealloc];
}

- (void)listenToKey:(NSString *)key onCenter:(NSDistributedNotificationCenter *)center {
     [center addObserver:self
                selector:@selector(notificationEvent:)
                    name:key
                  object:nil];
}

- (void)notificationEvent:(NSNotification *)notification {
    [self notifyLock];
}

- (void)notifyLock {
    [_condition lock];
    self.notified = true;
    [_condition signal];
    [_condition unlock];
}
@end

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_isDarkThemeEnabled(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    if(@available(macOS 10.14, *)) {
        NSString *interfaceStyle = [[NSUserDefaults standardUserDefaults] stringForKey:KEY_APPLE_INTERFACE_STYLE];
        // interfaceStyle can be nil (light mode) or "Dark" (dark mode).
        BOOL isDark = [VALUE_DARK caseInsensitiveCompare:interfaceStyle] == NSOrderedSame;
        if (@available(macOS 10.15, *)) {
            // Catalina
            BOOL switchesAutomatically = [[NSUserDefaults standardUserDefaults] boolForKey:KEY_SWITCHES_AUTOMATICALLY];
            if (switchesAutomatically) {
                // If switchesAutomatically == true the roles of "Dark" and nil are changed.
                return (jboolean) !isDark;
            }
        }
        // Mojave or switchesAutomatically == false.
        return (jboolean) isDark;
    } else {
        return (jboolean) false;
    }
JNF_COCOA_EXIT(env);
    return false;
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_isHighContrastEnabled(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    return (jboolean) NSWorkspace.sharedWorkspace.accessibilityDisplayShouldIncreaseContrast;
JNF_COCOA_EXIT(env);
    return (jboolean) false;
}

JNIEXPORT jint JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_nativeGetAccentColor(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    if (@available(macOS 10.14, *)) {
        BOOL hasAccentSet = ([[NSUserDefaults standardUserDefaults] objectForKey:KEY_ACCENT_COLOR] != nil);
        if (hasAccentSet) {
            return (jint) ([[NSUserDefaults standardUserDefaults] integerForKey:KEY_ACCENT_COLOR]);
        }
    }
    return (jint) VALUE_NO_ACCENT_COLOR;
JNF_COCOA_EXIT(env);
    return (jint) VALUE_NO_ACCENT_COLOR;
}

JNIEXPORT jint JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_nativeGetSelectionColor(JNIEnv *env, jclass obj) {
JNF_COCOA_ENTER(env);
    NSColorSpace *rgbSpace = [NSColorSpace genericRGBColorSpace];
    NSColor *accentColor = [[NSColor selectedControlColor] colorUsingColorSpace:rgbSpace];
    NSInteger r = (NSInteger) (255 * [accentColor redComponent]);
    NSInteger g = (NSInteger) (255 * [accentColor greenComponent]);
    NSInteger b = (NSInteger) (255 * [accentColor blueComponent]);
    return (jint) (0xff000000 | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | ((b & 0xFF) << 0));
JNF_COCOA_EXIT(env);
    return (jint) VALUE_NO_SELECTION_COLOR;
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_createPreferenceChangeListener(JNIEnv *env, jclass obj) {
JNF_COCOA_DURING(env); // We dont want an auto release pool.
    PreferenceChangeListener *listener = [[PreferenceChangeListener alloc] init];
    [listener retain];
    return (jlong) listener;
JNF_COCOA_HANDLE(env);
    return (jlong) 0;
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_awaitPreferenceChange(JNIEnv *env, jclass obj, jlong listenerPtr) {
JNF_COCOA_ENTER(env);
    PreferenceChangeListener *listener = OBJC(listenerPtr);
    [listener.condition lock];
    listener.notified = false;
    while(!listener.notified) {
        [listener.condition wait];
    }
    [listener.condition unlock];
    return true;
JNF_COCOA_EXIT(env);
    return false;
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_macos_JNIThemeInfoMacOS_deletePreferenceChangeListener(JNIEnv *env, jclass obj, jlong listenerPtr) {
JNF_COCOA_ENTER(env);
    PreferenceChangeListener *listener = OBJC(listenerPtr);
    if (listener) {
        [listener release];
        [listener dealloc];
    }
JNF_COCOA_EXIT(env);
}
