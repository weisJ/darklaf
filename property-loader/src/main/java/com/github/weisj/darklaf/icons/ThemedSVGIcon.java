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
package com.github.weisj.darklaf.icons;

import java.net.URI;
import java.util.function.Supplier;

import javax.swing.*;


/** @author Jannis Weis */
public class ThemedSVGIcon extends DarkSVGIcon implements ThemedIcon {

    private Object currentTheme;
    private boolean updatedNotDuringPaint;

    public ThemedSVGIcon(final Supplier<URI> uriSupplier, final int displayWidth, final int displayHeight) {
        super(uriSupplier, displayWidth, displayHeight);
        currentTheme = new Object();
    }

    public ThemedSVGIcon(final URI uri, final int displayWidth, final int displayHeight) {
        super(uri, displayWidth, displayHeight);
        currentTheme = new Object();
    }

    protected ThemedSVGIcon(final int width, final int height, final ThemedSVGIcon icon) {
        super(width, height, icon);
        this.currentTheme = icon.currentTheme;
        this.updatedNotDuringPaint = icon.updatedNotDuringPaint;
    }

    @Override
    public ThemedSVGIcon derive(final int width, final int height) {
        if (width == getIconWidth() && height == getIconHeight()) {
            // Even though checking the size may cause the icon to be loaded, we
            // do this optimization as painting to different off-screen images is more
            // expensive than loading the icon.
            return this;
        }
        return new ThemedSVGIcon(width, height, this);
    }

    @Override
    protected boolean ensureLoaded(final boolean painting) {
        /*
         * Use non-short-circuiting operand here to ensure the colors are actually patched.
         */
        return super.ensureLoaded(painting) | ensureTheme(painting);
    }

    protected boolean ensureTheme(final boolean painting) {
        Object theme = IconLoader.getThemeStatus();
        if (currentTheme != theme) {
            patchColors();
            currentTheme = theme;
            updatedNotDuringPaint = !painting;
            return true;
        }
        if (updatedNotDuringPaint) {
            updatedNotDuringPaint = false;
            // Update didn't happen during painting call.
            // Image might not be up to date.
            return true;
        }
        return false;
    }

    protected void invalidate() {
        currentTheme = new Object();
    }

    protected UIDefaults getContextDefaults() {
        return UIManager.getDefaults();
    }

    protected void patchColors() {
        IconColorMapper.patchColors(getSVGIcon(), getContextDefaults());
    }
}
