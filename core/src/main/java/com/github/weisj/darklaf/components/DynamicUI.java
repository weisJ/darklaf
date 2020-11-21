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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.function.Consumer;

import javax.swing.*;

public class DynamicUI {

    private static final Map<Component, List<Consumer<Component>>> listeners = new WeakHashMap<>();

    static {
        UIManager.addPropertyChangeListener(e -> {
            String key = e.getPropertyName();
            if ("lookAndFeel".equals(key)) {
                listeners.keySet().forEach(DynamicUI::updateComponent);
            }
        });
    }

    private DynamicUI() {
        throw new IllegalStateException("Utility class");
    }

    public static <T extends Component> T withDynamic(final T component, final Consumer<T> onUpdateUi) {
        // Explicit component update is required since the component already exists
        // and we can't want to wait for the next LaF change
        onUpdateUi.accept(component);
        synchronized (listeners) {
            listeners.compute(component, (k, v) -> {
                if (v == null) {
                    // noinspection unchecked
                    return Collections.singletonList((Consumer<Component>) onUpdateUi);
                }
                List<Consumer<Component>> res = v.size() == 1 ? new ArrayList<>(v) : v;
                // noinspection unchecked
                res.add((Consumer<Component>) onUpdateUi);
                return res;
            });
        }
        return component;
    }


    public static <T extends JComponent> T withLocalizedTooltip(final T comp, final String tipTextKey) {
        return withDynamic(comp, c -> c.setToolTipText(UIManager.getString(tipTextKey, c.getLocale())));
    }

    private static void updateComponent(final Component component) {
        synchronized (listeners) {
            List<Consumer<Component>> list = listeners.get(component);
            if (list == null) {
                return;
            }
            for (Consumer<Component> action : list) {
                action.accept(component);
            }
        }
    }
}
