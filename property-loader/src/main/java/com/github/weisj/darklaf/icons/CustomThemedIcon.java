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
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.app.beans.SVGIcon;

public class CustomThemedIcon extends ThemedSVGIcon {

    private Map<Object, Object> defaults;
    private boolean derived;

    public CustomThemedIcon(final Supplier<URI> uriSupplier, final int displayWidth, final int displayHeight) {
        this(uriSupplier, displayWidth, displayHeight, null);
    }

    public CustomThemedIcon(
            final Supplier<URI> uriSupplier, final int displayWidth, final int displayHeight,
            final Map<Object, Object> colors
    ) {
        super(uriSupplier, displayWidth, displayHeight);
        defaults = colors;
    }

    public CustomThemedIcon(
            final URI uri, final int displayWidth, final int displayHeight, final Map<Object, Object> colors
    ) {
        super(uri, displayWidth, displayHeight);
        defaults = colors;
    }

    protected CustomThemedIcon(final int width, final int height, final CustomThemedIcon icon) {
        super(width, height, icon);
        this.defaults = icon.defaults;
        this.derived = true;
    }

    /**
     * Set a property if the underlying property map supports mutation.
     *
     * @param  key                           the property key.
     * @param  value                         the property value.
     * @throws UnsupportedOperationException if the underlying property map doesnt support mutation.
     */
    public void setProperty(final Object key, final Object value) throws UnsupportedOperationException {
        getProperties().put(key, value);
    }

    /**
     * Get a property.
     *
     * @param  key the property key.
     * @return     the property value.
     */
    public Object getProperty(final Object key) {
        return getProperties().get(key);
    }

    /**
     * Get a property of a given type.
     *
     * @param  key  the property key.
     * @param  type the type.
     * @param  <T>  the types type parameter.
     * @return      the property value if the type matches or null otherwise.
     */
    public <T> T getPropertyOfType(final Object key, final Class<T> type) {
        Object obj = getProperty(key);
        if (type != null && type.isInstance(obj)) return type.cast(obj);
        return null;
    }

    private Map<Object, Object> getProperties() {
        if (defaults == null) {
            defaults = new HashMap<>();
            return defaults;
        }
        if (derived) {
            defaults = new HashMap<>(defaults);
            derived = false;
        }
        return defaults;
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
        IconColorMapper.patchColors(getSVGIcon(), getProperties());
    }
}
