/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.weisj.darklaf.properties.PropertyLoader;

public class CustomThemedIcon extends ThemedSVGIcon implements MutableThemedIcon {

    public static final Map<Object, Object> LAF_CONTEXT = null;
    private Map<Object, Object> defaults;
    private Map<Object, Object> contextDefaults;
    private boolean derived;

    public CustomThemedIcon(final URI uri, final int displayWidth, final int displayHeight,
            final Map<Object, Object> colors) {
        super(uri, displayWidth, displayHeight);
        defaults = colors;
    }

    public CustomThemedIcon(final DarkSVGIcon icon) {
        this(icon, LAF_CONTEXT, MergeMode.KEEP_REFERENCES);
    }

    public CustomThemedIcon(final DarkSVGIcon icon, final Map<Object, Object> contextDefaults,
            final MergeMode mergeMode) {
        super(icon.getURI(), icon.getIconWidth(), icon.getIconHeight());
        List<ThemedSVGIconParserProvider.ThemedSolidColorPaint> customPaints;
        if (icon instanceof ThemedSVGIcon) {
            icon.ensureLoaded(false);
            customPaints = ((ThemedSVGIcon) icon).paints();
        } else {
            ensureLoaded(false);
            customPaints = paints();
        }
        defaults = ThemedSVGIconParserProvider.getProperties(customPaints);
        setContextProperties(contextDefaults);
        mergeProperties(mergeMode, icon);
    }

    public void mergeProperties(final MergeMode mergeMode, final DarkSVGIcon originalIcon) {
        if (mergeMode != MergeMode.KEEP_REFERENCES && contextDefaults != null) {
            // Any property that isn't explicitly defined needs to be converted to an implicit property
            // for proper inter-theme behaviour.
            String referencePrefix = PropertyLoader.getReferencePrefix();
            if (!(originalIcon instanceof CustomThemedIcon)) {
                if (mergeMode == MergeMode.REMOVE_REFERENCES) {
                    defaults.clear();
                } else {
                    defaults.entrySet().forEach(e -> e.setValue(referencePrefix + e.getKey()));
                }
            } else {
                Map<Object, Object> originalProperties = ((CustomThemedIcon) originalIcon).getProperties();
                defaults.keySet().forEach(k -> {
                    if (!originalProperties.containsKey(k)
                            && contextDefaults.containsKey(k)) {
                        if (mergeMode == MergeMode.REMOVE_REFERENCES) {
                            defaults.remove(k);
                        } else {
                            defaults.put(referencePrefix + k, defaults.remove(k));
                        }
                    }
                });
            }
            invalidate();
        }
    }

    protected CustomThemedIcon(final int width, final int height, final CustomThemedIcon icon) {
        super(width, height, icon);
        this.defaults = icon.defaults;
        this.derived = true;
    }

    @Override
    public Map<Object, Object> getProperties() {
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
    public void setProperties(final Map<Object, Object> props) {
        this.defaults = props;
        invalidate();
    }

    @Override
    public CustomThemedIcon derive(final int width, final int height) {
        return new CustomThemedIcon(width, height, this);
    }

    @Override
    public Map<Object, Object> getContextProperties() {
        return contextDefaults != LAF_CONTEXT ? contextDefaults : getContextDefaults();
    }

    @Override
    public void setContextProperties(final Map<Object, Object> props) {
        this.contextDefaults = props;
        invalidate();
    }

    @Override
    protected void patchColors() {
        super.patchColors();
        ThemedSVGIconParserProvider.patchColors(paints(), getProperties(), getContextProperties());
    }

    public enum MergeMode {
        REMOVE_REFERENCES,
        REPLACE_REFERENCES,
        KEEP_REFERENCES
    }
}
