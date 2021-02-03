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

import java.util.Map;
import java.util.Properties;
import java.util.function.Consumer;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.parser.ParseResult;
import com.github.weisj.darklaf.parser.Parser;
import com.github.weisj.darklaf.parser.ParserContext;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LogUtil;

public abstract class ColorAdjustmentTask implements DefaultsAdjustmentTask {

    private static final Logger LOGGER = LogUtil.getLogger(ColorAdjustmentTask.class);
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

    protected void adjust(final String listKey, final Properties listProperties, final Consumer<Map<?, ?>> action) {
        ParseResult p = Parser.parse(Parser.createParseResult(listKey, listProperties.getProperty(listKey)),
                new ParserContext(listProperties, DEFAULTS, DarkUIUtil.ICON_LOADER));
        Object obj = p.result;
        if (obj instanceof Map<?, ?>) {
            action.accept((Map<?, ?>) obj);
        } else if (listProperties.contains(listKey)) {
            LOGGER.severe("Expected map object but got " + obj + "[" + obj.getClass() + "]."
                    + " Declared as " + listProperties.getProperty(listKey) + " with key " + listKey);
        }
    }
}
