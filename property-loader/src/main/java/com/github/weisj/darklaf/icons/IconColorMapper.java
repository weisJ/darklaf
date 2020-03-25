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

import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.Pair;
import com.kitfox.svg.*;
import com.kitfox.svg.animation.AnimationElement;
import com.kitfox.svg.app.beans.SVGIcon;
import com.kitfox.svg.xml.StyleAttribute;

import javax.swing.*;
import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public final class IconColorMapper {
    private static final Logger LOGGER = Logger.getLogger(IconLoader.class.getName());
    private static final Color FALLBACK_COLOR = Color.RED;

    public static void patchColors(final SVGIcon svgIcon) {
        SVGUniverse universe = svgIcon.getSvgUniverse();
        SVGDiagram diagram = universe.getDiagram(svgIcon.getSvgURI());
        try {
            loadColors(diagram);
        } catch (SVGElementException e) {
            LOGGER.log(Level.SEVERE, "Failed patching colors. " + e.getMessage(), e.getStackTrace());
        }
    }

    private static void loadColors(final SVGDiagram diagram) throws SVGElementException {
        SVGRoot root = diagram.getRoot();
        SVGElement defs = diagram.getElement("colors");
        if (defs == null) return;
        List<?> children = defs.getChildren(null);
        root.removeChild(defs);

        Defs themedDefs = new Defs();
        themedDefs.addAttribute("id", AnimationElement.AT_XML, "colors");
        root.loaderAddChild(null, themedDefs);

        for (Object child : children) {
            if (child instanceof LinearGradient) {
                String id = ((LinearGradient) child).getId();
                String[] fallbacks = getFallbacks((LinearGradient) child);
                Color c = resolveColor(id, fallbacks, FALLBACK_COLOR);
                Pair<LinearGradient, Runnable> result = createColor(c, id);
                LinearGradient gradient = result.getFirst();
                Runnable finalizer = result.getSecond();
                themedDefs.loaderAddChild(null, gradient);
                finalizer.run();
            }
        }
    }

    private static Color resolveColor(final String key, final String[] fallbacks, final Color fallbackColor) {
        Color color = UIManager.getColor(key);
        for (int i = 0; i < fallbacks.length && color == null; i++) {
            color = UIManager.getColor(fallbacks[i]);
        }
        if (color == null) {
            color = fallbackColor;
            LOGGER.warning("Could not load color with id'" + key + "' fallbacks" + Arrays.toString(fallbacks)
                           + " Using color " + fallbackColor + " instead.");
        }
        return color;
    }

    private static String[] getFallbacks(final LinearGradient child) {
        StyleAttribute attribute = new StyleAttribute();
        attribute.setName("fallback");
        try {
            child.getStyle(attribute);
        } catch (SVGException e) {
            return new String[0];
        }
        return attribute.getStringList();
    }

    private static Pair<LinearGradient, Runnable> createColor(final Color c, final String name)
        throws SVGElementException {
        LinearGradient grad = new LinearGradient();
        grad.addAttribute("id", AnimationElement.AT_XML, name);
        return new Pair<>(grad, () -> {
            Stop stop1 = new Stop();
            Stop stop2 = new Stop();
            String color = toHexString(c);
            try {
                stop1.addAttribute("stop-color", AnimationElement.AT_XML, color);
                stop1.addAttribute("offset", AnimationElement.AT_XML, "0");
                stop2.addAttribute("stop-color", AnimationElement.AT_XML, color);
                stop2.addAttribute("offset", AnimationElement.AT_XML, "1");
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
