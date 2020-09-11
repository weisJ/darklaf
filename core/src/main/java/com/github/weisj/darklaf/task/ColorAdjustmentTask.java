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

import java.awt.*;
import java.util.List;
import java.util.Properties;
import java.util.function.Consumer;

import javax.swing.*;

import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.DarkUIUtil;

public abstract class ColorAdjustmentTask implements DefaultsAdjustmentTask {

    protected static final UIDefaults DEFAULTS = new UIDefaults();

    @Override
    public void run(final Theme currentTheme, final Properties properties) {
        beforeTask(currentTheme, properties);
        runTask(currentTheme, properties);
        afterTask(currentTheme, properties);
    }

    protected void beforeTask(final Theme currentTheme, final Properties properties) {}

    protected abstract void runTask(final Theme currentTheme, final Properties properties);

    protected void afterTask(final Theme currentTheme, final Properties properties) {
        DEFAULTS.clear();
    }

    protected void adjust(final String listKey, final Properties listProperties, final Consumer<List<?>> action) {
        Object obj = PropertyLoader
            .parseValue(listKey, listProperties.getProperty(listKey), listProperties, DEFAULTS, DarkUIUtil.ICON_LOADER);
        if (obj instanceof List<?>) {
            action.accept((List<?>) obj);
        }
    }
}
