/*
 * MIT License
 *
 * Copyright (c) 2021-2023 Jannis Weis
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
import com.github.weisj.jsvg.attributes.paint.DefaultPaintParser;
import com.github.weisj.jsvg.attributes.paint.SimplePaintSVGPaint;
import com.github.weisj.jsvg.nodes.Defs;
import com.github.weisj.jsvg.nodes.LinearGradient;
import com.github.weisj.jsvg.nodes.SolidColor;
import com.github.weisj.jsvg.parser.AttributeNode;
import com.github.weisj.jsvg.parser.DefaultParserProvider;
import com.github.weisj.jsvg.parser.DomProcessor;
import com.github.weisj.jsvg.parser.ParsedElement;

public class ThemedSVGIconParserProvider extends DefaultParserProvider {
    private final DomProcessor preProcessor;

    public ThemedSVGIconParserProvider(final @NotNull ThemedSVGIcon icon) {
        preProcessor = new ThemedSVGIconDomProcessor(icon);
    }

    @Override
    public @Nullable DomProcessor createPreProcessor() {
        return preProcessor;
    }

    private static class ThemedSVGIconDomProcessor extends DarkSVGIconDomProcessor<ThemedSVGIcon> {

        public ThemedSVGIconDomProcessor(@NotNull ThemedSVGIcon icon) {
            super(icon);
        }

        @Override
        public void process(final @NotNull ParsedElement root) {
            super.process(root);
            for (ParsedElement child : root.children()) {
                if ("colors".equals(child.id()) && child.node() instanceof Defs) {
                    replaceColorDefinitions(child);
                    return;
                }
            }
        }

        private void replaceColorDefinitions(final ParsedElement element) {
            for (ParsedElement child : element.children()) {
                if (child.node() instanceof LinearGradient) {
                    replaceGradientColor(child, "opacity");
                } else if (child.node() instanceof SolidColor) {
                    replaceGradientColor(child, "solid-opacity");
                }
            }
        }

        private void replaceGradientColor(final ParsedElement colorElement, final String opacityTag) {
            String id = colorElement.id();
            if (id == null) return;

            AttributeNode attributeNode = colorElement.attributeNode();
            String[] fallbacks = attributeNode.getStringList("fallback");
            String opacityKey = attributeNode.getValue(opacityTag);
            String[] opacityFallback = attributeNode.getStringList("opacity-fallback");

            List<ParsedElement> stops = colorElement.children();
            float originalOpacity = 1;
            if (!stops.isEmpty()) {
                originalOpacity = stops.get(0).attributeNode().getPercentage("stop-opacity", originalOpacity);
            }

            ThemedSVGIconParserProvider.ThemedSolidColorPaint themedPaint =
                    new ThemedSVGIconParserProvider.ThemedSolidColorPaint(
                            id, fallbacks, opacityKey, opacityFallback, originalOpacity);
            colorElement.registerNamedElement(id, themedPaint);
            icon.registerPaint(themedPaint);
        }
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

    public static Map<Object, Object> getProperties(List<ThemedSVGIconParserProvider.ThemedSolidColorPaint> paints) {
        Map<Object, Object> values = new HashMap<>(paints.size() * 2, 0.75f);
        for (ThemedSVGIconParserProvider.ThemedSolidColorPaint paint : paints) {
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

        private Color color = DefaultPaintParser.DEFAULT_COLOR;

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
