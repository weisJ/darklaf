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
package com.github.weisj.darklaf.icons;

import java.net.URI;
import java.util.Map;
import java.util.function.Supplier;

import javax.swing.*;

import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.app.beans.SVGIcon;

public class CustomThemedIcon extends ThemedSVGIcon {

    private final Map<Object, Object> defaults;

    public CustomThemedIcon(final Supplier<URI> uriSupplier, final int displayWidth, final int displayHeight,
                            final Map<Object, Object> colors) {
        super(uriSupplier, displayWidth, displayHeight);
        defaults = colors;
    }

    public CustomThemedIcon(final URI uri, final int displayWidth, final int displayHeight,
                            final Map<Object, Object> colors) {
        super(uri, displayWidth, displayHeight);
        defaults = colors;
    }

    protected CustomThemedIcon(final int width, final int height, final CustomThemedIcon icon) {
        super(width, height, icon);
        this.defaults = icon.defaults;
    }

    @Override
    public CustomThemedIcon derive(final int width, final int height) {
        return new CustomThemedIcon(width, height, this);
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
