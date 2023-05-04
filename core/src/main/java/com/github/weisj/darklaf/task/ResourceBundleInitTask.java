/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.task;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.defaults.UIDefaultsWithResourceBundleCache;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.bundles.ResourceUtil;

public class ResourceBundleInitTask implements DefaultsInitTask {

    /*
     * The resource bundle containing localization for FileChooser, ColorChooser etc.
     */
    private static final String DARKLAF_BUNDLE_NAME = ResourceUtil.getBundleName("darklaf");
    private static final String SETTING_BUNDLE_NAME = ResourceUtil.getBundleName("theme_settings");
    private static final String TAB_FRAME_BUNDLE_NAME = ResourceUtil.getBundleName("tabFrame");
    // Note: These two bundles are pulled from the jdk.
    private static final String BASIC_BUNDLE_NAME = ResourceUtil.getJdkBundleName("basic");
    private static final String METAL_BUNDLE_NAME = ResourceUtil.getJdkBundleName("metal");

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        if (!(defaults instanceof UIDefaultsWithResourceBundleCache)) {
            throw new IllegalStateException();
        }
        UIDefaultsWithResourceBundleCache defs = (UIDefaultsWithResourceBundleCache) defaults;
        defs.addResourceBundle(DarkLaf.class, BASIC_BUNDLE_NAME);
        defs.addResourceBundle(DarkLaf.class, METAL_BUNDLE_NAME);
        defs.addResourceBundle(DarkLaf.class, DARKLAF_BUNDLE_NAME);
        defs.addResourceBundle(DarkLaf.class, SETTING_BUNDLE_NAME);
        defs.addResourceBundle(DarkLaf.class, TAB_FRAME_BUNDLE_NAME);
    }
}
