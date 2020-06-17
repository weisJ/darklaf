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

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Pair;
import com.kitfox.svg.*;
import com.kitfox.svg.animation.AnimationElement;
import com.kitfox.svg.app.beans.SVGIcon;
import com.kitfox.svg.xml.StyleAttribute;

/**
 * @author Jannis Weis
 */
public final class IconColorMapper {
    private static final Logger LOGGER = LogUtil.getLogger(IconLoader.class);
    private static final Color FALLBACK_COLOR = Color.RED;

    public static void patchColors(final SVGIcon svgIcon) {
        patchColors(svgIcon, UIManager.getDefaults());
    }

    public static void patchColors(final SVGIcon svgIcon, final UIDefaults defaults) {
        SVGUniverse universe = svgIcon.getSvgUniverse();
        SVGDiagram diagram = universe.getDiagram(svgIcon.getSvgURI());
        LOGGER.fine(() -> "Patching colors of icon " + svgIcon.getSvgURI());
        try {
            loadColors(diagram, defaults);
        } catch (SVGElementException e) {
            LOGGER.log(Level.SEVERE, "Failed patching colors. " + e.getMessage(), e.getStackTrace());
        }
    }

    private static void loadColors(final SVGDiagram diagram, final UIDefaults defaults) throws SVGElementException {
        SVGRoot root = diagram.getRoot();
        SVGElement defs = diagram.getElement("colors");
        if (defs == null) {
            LOGGER.warning(() -> {
                String uri = diagram.getXMLBase().toASCIIString();
                String name = uri.substring(Math.min(uri.lastIndexOf('/') + 1, uri.length() - 1));
                return "Themed icon '" + name
                       + "' has no color definitions. Consider loading it as a standard icon or add missing definitions";
            });
            return;
        }
        List<?> children = defs.getChildren(null);
        root.removeChild(defs);

        Defs themedDefs = new Defs();
        themedDefs.addAttribute("id", AnimationElement.AT_XML, "colors");
        root.loaderAddChild(null, themedDefs);

        for (Object child : children) {
            if (child instanceof LinearGradient) {
                String id = ((LinearGradient) child).getId();
                StyleAttribute fallbacks = getFallbacks((LinearGradient) child);
                String opacityKey = getOpacityKey((LinearGradient) child);
                float opacity = getOpacity(opacityKey);
                Color c = resolveColor(id, getFallbacks(fallbacks), FALLBACK_COLOR, defaults);
                Pair<LinearGradient, Runnable> result = createColor(c, id, opacityKey, fallbacks, opacity);
                LinearGradient gradient = result.getFirst();
                Runnable finalizer = result.getSecond();
                themedDefs.loaderAddChild(null, gradient);
                finalizer.run();
            }
        }
    }

    private static Color resolveColor(final String key, final String[] fallbacks,
                                      final Color fallbackColor, final UIDefaults defaults) {
        Color color = defaults.getColor(key);
        for (int i = 0; i < fallbacks.length && color == null; i++) {
            color = defaults.getColor(fallbacks[i]);
        }
        if (color == null) {
            color = fallbackColor;
            LOGGER.warning("Could not load color with id '" + key + "' fallbacks" + Arrays.toString(fallbacks)
                           + ". Using color '" + fallbackColor + "' instead.");
        }
        return color;
    }

    private static StyleAttribute getFallbacks(final LinearGradient child) {
        StyleAttribute attribute = new StyleAttribute();
        attribute.setName("fallback");
        try {
            child.getStyle(attribute);
        } catch (SVGException e) {
            return null;
        }
        return attribute;
    }

    private static String[] getFallbacks(final StyleAttribute fallbacks) {
        if (fallbacks == null) return new String[0];
        return fallbacks.getStringList();
    }

    private static float getOpacity(final String key) {
        // UIManager defaults to 0, if the values isn't an integer (or null).
        Object obj = UIManager.get(key);
        if (obj instanceof Integer) {
            return ((Integer) obj) / 100.0f;
        }
        if (key != null && !key.isEmpty()) {
            LOGGER.warning(obj + " is an invalid opacity value. Key = '" + key + "'");
        }
        // In this case we default to 1.
        return 1;
    }

    private static String getOpacityKey(final LinearGradient child) {
        StyleAttribute attribute = new StyleAttribute();
        attribute.setName("opacity");
        try {
            child.getStyle(attribute);
        } catch (SVGException e) {
            e.printStackTrace();
            return null;
        }
        return attribute.getStringValue();
    }

    private static Pair<LinearGradient, Runnable> createColor(final Color c,
                                                              final String name, final String opacityKey,
                                                              final StyleAttribute fallbacks,
                                                              final float opacity) throws SVGElementException {
        LinearGradient grad = new LinearGradient();
        grad.addAttribute("id", AnimationElement.AT_XML, name);
        if (opacityKey != null) {
            grad.addAttribute("opacity", AnimationElement.AT_XML, opacityKey);
        }
        if (fallbacks != null) {
            grad.addAttribute(fallbacks.getName(), AnimationElement.AT_XML, fallbacks.getStringValue());
        }
        return new Pair<>(grad, () -> {
            Stop stop1 = new Stop();
            Stop stop2 = new Stop();
            String color = toHexString(c);
            try {
                stop1.addAttribute("stop-color", AnimationElement.AT_XML, color);
                stop1.addAttribute("offset", AnimationElement.AT_XML, "0");
                stop2.addAttribute("stop-color", AnimationElement.AT_XML, color);
                stop2.addAttribute("offset", AnimationElement.AT_XML, "1");
                if (opacity != 1) {
                    stop1.addAttribute("stop-opacity", AnimationElement.AT_XML, String.valueOf(opacity));
                    stop2.addAttribute("stop-opacity", AnimationElement.AT_XML, String.valueOf(opacity));
                }
                grad.loaderAddChild(null, stop1);
                grad.loaderAddChild(null, stop2);
            } catch (SVGElementException e) {
                e.printStackTrace();
            }
        });
    }

    private static String toHexString(final Color color) {
        return "#" + ColorUtil.toHex(color);
    }
}
