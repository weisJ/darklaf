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
 */
package com.github.weisj.darklaf.icons;

import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.app.beans.SVGIcon;

import javax.swing.*;
import java.net.URI;
import java.util.Map;

public class CustomThemedIcon extends ThemedSVGIcon {

    private final UIDefaults defaults;

    public CustomThemedIcon(final URI uri, final int displayWidth, final int displayHeight,
                            final Map<Object, Object> colors) {
        super(uri, displayWidth, displayHeight);
        defaults = new UIDefaults(colors.size(), 1f);
        for (Map.Entry<Object, Object> entry : colors.entrySet()) {
            defaults.put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    protected SVGIcon createSVGIcon() {
        SVGIcon icon = new SVGIcon();
        icon.setSvgUniverse(new SVGUniverse());
        return icon;
    }

    @Override
    protected void patchColors() {
        IconColorMapper.patchColors(getSVGIcon(), defaults);
    }
}
