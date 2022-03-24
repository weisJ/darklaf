/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.nativelaf;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.platform.decorations.NativeDecorationsManager;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DecorationsHandler extends NativeDecorationsManager {

    public static final String DECORATIONS_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "decorations";
    private static DecorationsHandler sharedInstance;

    public static DecorationsHandler getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new DecorationsHandler());
        return sharedInstance;
    }

    public static void setSharedInstance(final DecorationsHandler handler) {
        DecorationsHandler.sharedInstance = handler;
    }

    protected DecorationsHandler() {
        super(isNativeDecorationsEnabled());
    }

    private static boolean isNativeDecorationsEnabled() {
        return PropertyUtil.getSystemFlag(DECORATIONS_FLAG)
                && PropertyUtil.getSystemFlag(DarkLaf.ALLOW_NATIVE_CODE_FLAG);
    }
}
