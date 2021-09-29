/*
 * MIT License
 *
 * Copyright (c) 2021-2022 Jannis Weis
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
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.jsvg.parser.DomProcessor;
import com.github.weisj.jsvg.parser.ParsedElement;

public class DarkSVGIconDomProcessor<T extends DarkSVGIcon> implements DomProcessor {
    protected final @NotNull T icon;

    public DarkSVGIconDomProcessor(@NotNull T icon) {
        this.icon = icon;
    }

    @Override
    public void process(@NotNull ParsedElement root) {
        float[] visualPaddings = root.attributeNode().getFloatList("visualPadding");
        if (visualPaddings.length == 4) {
            icon.setVisualPadding(new Insets(
                    (int) visualPaddings[0],
                    (int) visualPaddings[1],
                    (int) visualPaddings[2],
                    (int) visualPaddings[3]));
        }
    }
}
