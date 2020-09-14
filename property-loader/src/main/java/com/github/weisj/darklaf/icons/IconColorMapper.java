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

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
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

/** @author Jannis Weis */
public final class IconColorMapper {
    private static final Logger LOGGER = LogUtil.getLogger(IconLoader.class);
    private static final Color FALLBACK_COLOR = Color.RED;

    public static void patchColors(final SVGIcon svgIcon) {
        patchColors(svgIcon, UIManager.getDefaults());
    }

    public static void patchColors(final SVGIcon svgIcon, final Map<Object, Object> defaults) {
        SVGUniverse universe = svgIcon.getSvgUniverse();
        SVGDiagram diagram = universe.getDiagram(svgIcon.getSvgURI());
        LOGGER.finer(() -> "Patching colors of icon " + svgIcon.getSvgURI());
        try {
            loadColors(diagram, defaults);
        } catch (SVGElementException e) {
            LOGGER.log(Level.SEVERE, "Failed patching colors. " + e.getMessage(), e.getStackTrace());
        }
    }

    private static void loadColors(final SVGDiagram diagram, final Map<Object, Object> defaults)
            throws SVGElementException {
        SVGRoot root = diagram.getRoot();
        SVGElement defs = diagram.getElement("colors");
        if (defs == null) {
            LOGGER.info(() -> {
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
                LinearGradient grad = (LinearGradient) child;
                String id = grad.getId();
                StyleAttribute colorFallbacks = getAttribute("fallback", grad);
                StyleAttribute opacityFallbacks = getAttribute("opacity-fallback", grad);
                String opacityKey = getOpacityKey(grad);

                float opacity = getOpacity(opacityKey, getFallbacks(opacityFallbacks), defaults);
                float opacity1 = opacity;
                float opacity2 = opacity;
                if (opacity < 0) {
                    opacity = 1;
                    int childCount = grad.getNumChildren();
                    if (childCount > 0) {
                        SVGElement elem = grad.getChild(0);
                        if (elem instanceof Stop) {
                            opacity1 = getStopOpacity((Stop) elem);
                        }
                    }
                    if (childCount > 1) {
                        SVGElement elem = grad.getChild(1);
                        if (elem instanceof Stop) {
                            opacity2 = getStopOpacity((Stop) elem);
                        }
                    }

                    if (opacity1 < 0) opacity1 = opacity;
                    if (opacity2 < 0) opacity2 = opacity;
                }

                Color c = resolveColor(id, getFallbacks(colorFallbacks), FALLBACK_COLOR, defaults);
                Pair<LinearGradient, Runnable> result =
                        createColor(c, id, opacityKey, colorFallbacks, opacity1, opacity2);
                LinearGradient gradient = result.getFirst();
                Runnable finalizer = result.getSecond();
                themedDefs.loaderAddChild(null, gradient);
                finalizer.run();
            }
        }
    }

    public static float getOpacity(final LinearGradient gradient, final Map<Object, Object> propertyMap) {
        String opacityKey = getOpacityKey(gradient);
        return getOpacity(opacityKey, null, propertyMap);
    }

    public static Color getColor(final LinearGradient gradient, final Map<Object, Object> propertyMap) {
        String id = (gradient).getId();
        StyleAttribute fallbacks = getAttribute("fallback", gradient);
        return resolveColor(id, getFallbacks(fallbacks), FALLBACK_COLOR, propertyMap);
    }

    private static Color resolveColor(final String key, final String[] fallbacks, final Color fallbackColor,
            final Map<Object, Object> propertyMap) {
        Color color = get(propertyMap, key, fallbacks, Color.class);

        if (color == null) {
            color = fallbackColor;
            LOGGER.warning("Could not load color with id '" + key + "' fallbacks" + Arrays.toString(fallbacks)
                    + ". Using color '" + fallbackColor + "' instead.");
        }
        return color;
    }

    private static StyleAttribute getAttribute(final String key, final LinearGradient child) {
        StyleAttribute attribute = new StyleAttribute();
        attribute.setName(key);
        try {
            child.getStyle(attribute);
        } catch (SVGException e) {
            return null;
        }
        return attribute;
    }

    private static float getStopOpacity(final Stop stop) {
        StyleAttribute attribute = new StyleAttribute();
        attribute.setName("stop-opacity");
        try {
            stop.getStyle(attribute);
        } catch (SVGException e) {
            return -1;
        }
        return !attribute.getStringValue().isEmpty() ? attribute.getFloatValue() : -1;
    }

    private static String[] getFallbacks(final StyleAttribute fallbacks) {
        if (fallbacks == null) return new String[0];
        return fallbacks.getStringList();
    }

    private static float getOpacity(final String key, final String[] fallbacks, final Map<Object, Object> propertyMap) {
        if ((key == null || key.isEmpty()) && (fallbacks == null || fallbacks.length == 0)) return -1;
        // UIManager defaults to 0, if the value isn't an integer (or null).
        Number obj = get(propertyMap, key, fallbacks, Number.class);
        if (obj instanceof Integer) {
            return obj.intValue() / 100.0f;
        } else if (obj instanceof Long) {
            return obj.intValue() / 100.0f;
        } else if (obj instanceof Float) {
            return obj.floatValue();
        } else if (obj instanceof Double) {
            return obj.floatValue();
        }
        LOGGER.warning(obj + " is an invalid opacity value. Key = '" + key + "'");
        // In this case we default to -1.
        return -1;
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

    private static Pair<LinearGradient, Runnable> createColor(final Color c, final String name, final String opacityKey,
            final StyleAttribute fallbacks, final float opacity1, final float opacity2) throws SVGElementException {
        LinearGradient grad = new LinearGradient();
        grad.addAttribute("id", AnimationElement.AT_XML, name);
        if (opacityKey != null && !opacityKey.isEmpty()) {
            grad.addAttribute("opacity", AnimationElement.AT_XML, opacityKey);
        }
        if (fallbacks != null && !fallbacks.getStringValue().isEmpty()) {
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
                if (opacity1 != 1) {
                    stop1.addAttribute("stop-opacity", AnimationElement.AT_XML, String.valueOf(opacity1));
                }
                if (opacity2 != 1) {
                    stop2.addAttribute("stop-opacity", AnimationElement.AT_XML, String.valueOf(opacity2));
                }
                grad.loaderAddChild(null, stop1);
                grad.loaderAddChild(null, stop2);
            } catch (SVGException e) {
                e.printStackTrace();
            }
        });
    }

    private static <T> T get(final Map<Object, Object> map, final Object key, final Class<T> type) {
        return getFromMap(map, key, null, type);
    }

    private static <T> T get(final Map<Object, Object> map, final Object key, final Object[] fallbacks,
            final Class<T> type) {
        T obj = getFromMap(map, key, fallbacks, type);
        if (obj == null) return getFromMap(UIManager.getDefaults(), key, fallbacks, type);
        return obj;
    }

    private static <T> T getFromMap(final Map<Object, Object> map, final Object key, final Object[] fallbacks,
            final Class<T> type) {
        Object obj = map.get(key);
        if (fallbacks != null) {
            for (int i = 0; i < fallbacks.length && !type.isInstance(obj); i++) {
                obj = map.get(fallbacks[i]);
            }
        }
        if (type.isInstance(obj)) return type.cast(obj);
        return null;
    }

    private static String toHexString(final Color color) {
        return "#" + ColorUtil.toHex(color);
    }
}
