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
 *
 */
package com.github.weisj.darklaf.task;

import javax.swing.*;

import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.ResourceUtil;

public class ResourceBundleInitTask implements DefaultsInitTask {

    /*
     * The resource bundle containing localization for FileChooser, ColorChooser etc.
     */
    private static final String PLAF_BUNDLE_NAME = "com.sun.swing.internal.plaf.basic.resources.basic";
    private static final String METAL_BUNDLE_NAME = "com.sun.swing.internal.plaf.metal.resources.metal";
    private static final String DARKLAF_BUNDLE_NAME = ResourceUtil.getBundleName("darklaf");
    private static final String SETTING_BUNDLE_NAME = ResourceUtil.getBundleName("theme_settings");
    private static final String TAB_FRAME_BUNDLE_NAME = ResourceUtil.getBundleName("tabFrame");

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        defaults.addResourceBundle(PLAF_BUNDLE_NAME);
        defaults.addResourceBundle(METAL_BUNDLE_NAME);
        defaults.addResourceBundle(DARKLAF_BUNDLE_NAME);
        defaults.addResourceBundle(SETTING_BUNDLE_NAME);
        defaults.addResourceBundle(TAB_FRAME_BUNDLE_NAME);
    }
}
