/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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

import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.theme.Theme;

public class SystemDefaultsInitTask implements DefaultsInitTask {

    private static final String OVERWRITES_PATH = "properties/";
    private static final String OVERWRITES_NAME = "overwrites";

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        loadSystemOverwrites(defaults);
    }

    private void loadSystemOverwrites(final UIDefaults defaults) {
        Properties overwrites = PropertyLoader.loadProperties(DarkLaf.class, OVERWRITES_NAME, OVERWRITES_PATH);
        overwrites.values().removeIf(v -> System.getProperty(DarkLaf.SYSTEM_PROPERTY_PREFIX + v.toString()) == null);
        overwrites.entrySet()
                .forEach(e -> e.setValue(System.getProperty(DarkLaf.SYSTEM_PROPERTY_PREFIX + e.getValue().toString())));
        PropertyLoader.putProperties(overwrites, defaults);
    }
}
