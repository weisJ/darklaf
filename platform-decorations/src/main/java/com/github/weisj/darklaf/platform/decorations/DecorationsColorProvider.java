/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.decorations;

import java.awt.*;

import com.github.weisj.darklaf.properties.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.ColorUtil;

public interface DecorationsColorProvider {

    default void onLafChanged() {}

    default Color borderColor() {
        return inactiveForegroundColor();
    }

    boolean isDark();

    Color backgroundColor();

    default Color hoverBackgroundColor() {
        Color background = backgroundColor();
        if (background == null) return null;
        int brightness = (int) ColorUtil.getPerceivedBrightness(background);
        if (brightness >= 125) {
            // Darken
            return new DarkColorUIResource(
                    Math.max((int) (background.getRed() * 0.9f), 0),
                    Math.max((int) (background.getGreen() * 0.9f), 0),
                    Math.max((int) (background.getBlue() * 0.9f), 0));
        } else {
            return background.brighter();
        }
    }

    default Color clickBackgroundColor() {
        return hoverBackgroundColor();
    }

    Color activeForegroundColor();

    Color inactiveForegroundColor();

    default Color windowButtonColor() {
        return activeForegroundColor();
    }

    default Color inactiveWindowButtonColor() {
        return inactiveForegroundColor();
    }
}
