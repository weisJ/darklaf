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
 *
 */
package com.github.weisj.darklaf.task;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.icons.AwareIconStyle;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.cell.CellUtil;

public class UtilityDefaultsInitTask implements DefaultsInitTask {
    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        setupUtils(currentTheme, defaults);
    }

    /*
     * Update the values for all classes using theme defaults and notify them that the theme has
     * changed.
     */
    private void setupUtils(final Theme currentTheme, final UIDefaults defaults) {
        PaintUtil.setDropOpacity(getOpacity(defaults, "dropOpacity"));
        PaintUtil.setGlowOpacity(getOpacity(defaults, "glowOpacity"));
        PaintUtil.setShadowOpacity(getOpacity(defaults, "shadowOpacityLight"));

        PaintUtil.setErrorGlow(defaults.getColor("glowError"));
        PaintUtil.setErrorFocusGlow(defaults.getColor("glowFocusError"));
        PaintUtil.setFocusGlow(defaults.getColor("glowFocus"));
        PaintUtil.setFocusInactiveGlow(defaults.getColor("glowFocusInactive"));
        PaintUtil.setWarningGlow(defaults.getColor("glowWarning"));

        IconLoader.updateAwareStyle(Theme.isDark(currentTheme) ? AwareIconStyle.DARK : AwareIconStyle.LIGHT);
        IconLoader.updateThemeStatus(currentTheme);

        CellUtil.updateColors(defaults);
    }

    private float getOpacity(final UIDefaults defaults, final String key) {
        Object obj = defaults.get(key);
        int val = (obj instanceof Integer) ? (int) obj : 100;
        return val / 100f;
    }

    @Override
    public boolean onlyDuringInstallation() {
        return true;
    }
}
