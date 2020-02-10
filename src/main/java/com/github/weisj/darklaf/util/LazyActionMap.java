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
package com.github.weisj.darklaf.util;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ActionMapUIResource;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * An ActionMap that populates its contents as necessary. The contents are populated by invoking the
 * <code>loadActionMap</code> method on the passed in Object.
 *
 * @author Scott Violet
 */
public class LazyActionMap extends ActionMapUIResource {
    /**
     * Object to invoke <code>loadActionMap</code> on. This may be a Class object.
     */
    private transient Object _loader;

    public LazyActionMap(final Class<?> loader) {
        _loader = loader;
    }

    /**
     * Installs an ActionMap that will be populated by invoking the
     * <code>loadActionMap</code> method on the specified Class
     * when necessary.
     * <p>
     * This should be used if the ActionMap can be shared.
     *
     * @param c           JComponent to install the ActionMap on.
     * @param loaderClass Class object that gets loadActionMap invoked on.
     * @param defaultsKey Key to use to defaults table to check for existing map and what resulting Map will be
     *                    registered on.
     */
    public static void installLazyActionMap(final JComponent c, final Class<?> loaderClass,
                                            final String defaultsKey) {
        ActionMap map = (ActionMap) UIManager.get(defaultsKey);
        if (map == null) {
            map = new LazyActionMap(loaderClass);
            UIManager.getLookAndFeelDefaults().put(defaultsKey, map);
        }
        SwingUtilities.replaceUIActionMap(c, map);
    }

    /**
     * Returns an ActionMap that will be populated by invoking the
     * <code>loadActionMap</code> method on the specified Class
     * when necessary.
     * <p>
     * This should be used if the ActionMap can be shared.
     *
     * @param loaderClass Class object that gets loadActionMap invoked on.
     * @param defaultsKey Key to use to defaults table to check for existing map and what resulting Map will be
     *                    registered on.
     * @return the action map
     */
    @NotNull
    public static ActionMap getActionMap(final Class<?> loaderClass,
                                         final String defaultsKey) {
        ActionMap map = (ActionMap) UIManager.get(defaultsKey);
        if (map == null) {
            map = new LazyActionMap(loaderClass);
            UIManager.getLookAndFeelDefaults().put(defaultsKey, map);
        }
        return map;
    }

    public void put(final Action action) {
        put(action.getValue(Action.NAME), action);
    }

    private void loadIfNecessary() {
        if (_loader != null) {
            Object loader = _loader;

            _loader = null;
            Class<?> klass = (Class<?>) loader;
            try {
                Method method = klass.getDeclaredMethod("loadActionMap", LazyActionMap.class);
                method.invoke(klass, this);
            } catch (NoSuchMethodException nsme) {
                assert false : "LazyActionMap unable to load actions " +
                        klass;
            } catch (IllegalAccessException
                    | InvocationTargetException
                    | IllegalArgumentException iae) {
                assert false : "LazyActionMap unable to load actions " +
                        iae;
            }
        }
    }

    public void setParent(final ActionMap map) {
        loadIfNecessary();
        super.setParent(map);
    }

    public void put(final Object key, final Action action) {
        loadIfNecessary();
        super.put(key, action);
    }

    public Action get(final Object key) {
        loadIfNecessary();
        return super.get(key);
    }

    public void remove(final Object key) {
        loadIfNecessary();
        super.remove(key);
    }

    public void clear() {
        loadIfNecessary();
        super.clear();
    }

    public Object[] keys() {
        loadIfNecessary();
        return super.keys();
    }

    public int size() {
        loadIfNecessary();
        return super.size();
    }

    public Object[] allKeys() {
        loadIfNecessary();
        return super.allKeys();
    }
}