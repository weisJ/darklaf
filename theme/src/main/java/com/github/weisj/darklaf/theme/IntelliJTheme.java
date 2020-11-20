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
package com.github.weisj.darklaf.theme;

import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.annotations.SynthesiseLaf;
import com.github.weisj.darklaf.theme.info.ColorToneRule;
import com.github.weisj.darklaf.theme.info.PresetIconRule;
import com.google.auto.service.AutoService;

/** @author Jannis Weis */
@AutoService(Theme.class)
@SynthesiseLaf
public class IntelliJTheme extends Theme {

    @Override
    protected String getResourcePath() {
        return "intellij/";
    }

    @Override
    protected PresetIconRule getPresetIconRule() {
        return PresetIconRule.LIGHT;
    }

    @Override
    public String getPrefix() {
        return "intellij";
    }

    @Override
    public String getName() {
        return "IntelliJ";
    }

    @Override
    protected Class<? extends Theme> getLoaderClass() {
        return IntelliJTheme.class;
    }

    @Override
    public ColorToneRule getColorToneRule() {
        return ColorToneRule.LIGHT;
    }

    @Override
    public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults) {
        super.customizeUIProperties(properties, currentDefaults);
        loadCustomProperties("ui", properties, currentDefaults);
    }

    @Override
    public void customizeIconTheme(final Properties properties, final UIDefaults currentDefaults) {
        super.customizeIconTheme(properties, currentDefaults);
        loadCustomProperties("icons", properties, currentDefaults);
    }

    @Override
    public boolean supportsCustomAccentColor() {
        return true;
    }

    @Override
    public boolean supportsCustomSelectionColor() {
        return true;
    }
}
