/*
 * MIT License
 *
 * Copyright (c) 2025 Jannis Weis
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.jsvg.paint.SimplePaintSVGPaint;
import com.github.weisj.jsvg.parser.DomElement;
import com.github.weisj.jsvg.parser.PaintParser;

public class ThemedSVGIconDomProcessor extends DarkSVGIconDomProcessor<ThemedSVGIcon> {

    public ThemedSVGIconDomProcessor(@NotNull ThemedSVGIcon icon) {
        super(icon);
    }

    @Override
    public void process(final @NotNull DomElement root) {
        super.process(root);
        for (var child : root.children()) {
            if ("colors".equals(child.id()) && "defs".equalsIgnoreCase(child.tagName())) {
                replaceColorDefinitions(child);
                return;
            }
        }
    }

    private void replaceColorDefinitions(final DomElement element) {
        for (var child : element.children()) {
            if ("lineargradient".equalsIgnoreCase(child.tagName())) {
                replaceGradientColor(child, "opacity");
            } else if ("solidcolor".equalsIgnoreCase(child.tagName())) {
                replaceGradientColor(child, "solid-opacity");
            }
        }
    }

    private float parseOpacity(final @Nullable String attribute, float fallback) {
        if (attribute == null) return fallback;
        try {
            return Float.parseFloat(attribute.trim());
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    private void replaceGradientColor(final DomElement colorElement, final String opacityTag) {
        String id = colorElement.id();
        if (id == null) return;

        String[] fallbacks = parseStringList(colorElement.attribute("fallback"));
        String opacityKey = colorElement.attribute(opacityTag);
        String[] opacityFallback = parseStringList(colorElement.attribute("opacity-fallback"));

        var stops = colorElement.children();
        float originalOpacity = 1;
        if (!stops.isEmpty()) {
            String rawOpacity = stops.get(0).attribute("stop-opacity");
            originalOpacity = parseOpacity(rawOpacity, originalOpacity);
        }

        var themedPaint = new ThemedSVGIconDomProcessor.ThemedSolidColorPaint(
                id, fallbacks, opacityKey, opacityFallback, originalOpacity);
        colorElement.document().registerNamedElement(id, themedPaint);
        icon.registerPaint(themedPaint);
    }

    public static void patchColors(final List<ThemedSolidColorPaint> paints, final Map<Object, Object> propertyMap,
            final Map<Object, Object> contextDefaults) {
        for (ThemedSolidColorPaint paint : paints) {
            paint.color = IconColorMapper.resolveColor(
                    paint.colorKey, paint.colorFallbacks, propertyMap, contextDefaults);
            float opacity = IconColorMapper.getOpacity(
                    paint.opacityKey, paint.opacityFallbacks, propertyMap, contextDefaults);
            if (opacity < 0) opacity = paint.originalOpacity;
            paint.color = ColorUtil.toAlpha(paint.color, opacity);
        }
    }

    public static Map<Object, Object> getProperties(List<ThemedSolidColorPaint> paints) {
        Map<Object, Object> values = new HashMap<>(paints.size() * 2, 0.75f);
        for (ThemedSolidColorPaint paint : paints) {
            values.put(paint.colorKey, ColorUtil.removeAlpha(paint.color));
            if (paint.opacityKey != null && !paint.opacityKey.isEmpty()) {
                values.put(paint.opacityKey, (int) (paint.color.getAlpha() / 255f));
            }
        }
        return values;
    }

    public static Map<String, Color> getNamedColors(final ThemedSVGIcon icon) {
        icon.ensureLoaded(false);
        return icon.paints().stream().collect(Collectors.toMap(
                p -> p.colorKey,
                p -> p.color));
    }

    static class ThemedSolidColorPaint implements SimplePaintSVGPaint {

        private final String colorKey;
        private final String[] colorFallbacks;
        private final String opacityKey;
        private final String[] opacityFallbacks;

        private final float originalOpacity;

        private Color color = PaintParser.DEFAULT_COLOR;

        ThemedSolidColorPaint(final String colorKey, final String[] colorFallbacks,
                final String opacityKey, final String[] opacityFallbacks,
                float originalOpacity) {
            this.colorKey = colorKey;
            this.colorFallbacks = colorFallbacks;
            this.opacityKey = opacityKey;
            this.opacityFallbacks = opacityFallbacks;
            this.originalOpacity = originalOpacity;
        }

        @Override
        public @NotNull Paint paint() {
            return color;
        }
    }
}
