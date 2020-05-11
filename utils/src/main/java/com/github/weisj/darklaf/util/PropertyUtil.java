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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.util.Objects;

import javax.swing.*;
import javax.swing.plaf.UIResource;

public class PropertyUtil {

    public static void installBackground(final JComponent component, final Color color) {
        Color c = component.getBackground();
        if (c == null || c instanceof UIResource) {
            component.setBackground(color);
        }
    }

    public static void installForeground(final JComponent component, final Color color) {
        Color c = component.getForeground();
        if (c == null || c instanceof UIResource) {
            component.setForeground(color);
        }
    }

    public static void installProperty(final JComponent c, final String key, final Object value) {
        if (c.getClientProperty(key) == null) {
            c.putClientProperty(key, value);
        }
    }

    public static void installBooleanProperty(final JComponent c, final String key, final String valueKey) {
        installProperty(c, key, UIManager.getBoolean(valueKey));
    }

    public static boolean getBooleanProperty(final Component c, final String property) {
        return getBooleanProperty(c, property, false);
    }

    public static boolean getBooleanProperty(final Component c, final String property, final boolean defaultValue) {
        return c instanceof JComponent && getBooleanProperty((JComponent) c, property, defaultValue);
    }

    public static boolean getBooleanProperty(final JComponent c, final String property) {
        return getBooleanProperty(c, property, false);
    }

    public static boolean getBooleanProperty(final JComponent c, final String property, final boolean defaultValue) {
        if (c == null) return defaultValue;
        Object obj = c.getClientProperty(property);
        if (!defaultValue) {
            return Boolean.TRUE.equals(obj);
        } else {
            return !Boolean.FALSE.equals(obj);
        }
    }

    public static <T> boolean isPropertyEqual(final Component c, final String property, final T checkValue) {
        return c instanceof JComponent && isPropertyEqual((JComponent) c, property, checkValue);
    }

    public static <T> boolean isPropertyEqual(final JComponent c, final String property, final T checkValue) {
        if (c == null) return false;
        Object obj = c.getClientProperty(property);
        return Objects.equals(checkValue, obj);
    }

    public static <T> T getObject(final Component c, final String key, final Class<T> type, final T defaultValue) {
        if (!(c instanceof JComponent)) return defaultValue;
        return getObject((JComponent) c, key, type, defaultValue);
    }

    public static <T> T getObject(final JComponent c, final String key, final Class<T> type, final T defaultValue) {
        Object obj = c.getClientProperty(key);
        if (type.isInstance(obj)) return (T) obj;
        return defaultValue;
    }

    public static Color getColor(final JComponent c, final String key, final Color defaultValue) {
        return getObject(c, key, Color.class, defaultValue);
    }

    public static Color getColor(final Component c, final String key, final Color defaultValue) {
        return getObject(c, key, Color.class, defaultValue);
    }

    public static String getString(final JComponent c, final String key, final String defaultValue) {
        return getObject(c, key, String.class, defaultValue);
    }

    public static String getString(final Component c, final String key, final String defaultValue) {
        return getObject(c, key, String.class, defaultValue);
    }

    public static <T> T getObject(final Component c, final String key, final Class<T> type) {
        return getObject(c, key, type, null);
    }

    public static <T> T getObject(final JComponent c, final String key, final Class<T> type) {
        return getObject(c, key, type, null);
    }

    public static Color getColor(final JComponent c, final String key) {
        return getColor(c, key, null);
    }

    public static Color getColor(final Component c, final String key) {
        return getColor(c, key, null);
    }

    public static String getString(final JComponent c, final String key) {
        return getString(c, key, null);
    }

    public static String getString(final Component c, final String key) {
        return getString(c, key, null);
    }

    public static Integer getInteger(final JComponent c, final String key, final int defaultValue) {
        return getObject(c, key, Integer.class, defaultValue);
    }

    public static Integer getInteger(final Component c, final String key, final int defaultValue) {
        return getObject(c, key, Integer.class, defaultValue);
    }

    public static Integer getInteger(final Component c, final String key) {
        return getInteger(c, key, 0);
    }

    public static Integer getInteger(final JComponent c, final String key) {
        return getInteger(c, key, 0);
    }
}
