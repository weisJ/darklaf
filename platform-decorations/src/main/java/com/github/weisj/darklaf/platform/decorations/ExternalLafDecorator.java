/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.decorations;

import java.awt.*;
import java.beans.PropertyChangeListener;
import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.properties.uiresource.DarkColorUIResource;

public final class ExternalLafDecorator {

    private static ExternalLafDecorator instance;

    private final NativeDecorationsManager decorationsManager;
    private final PropertyChangeListener propertyChangeListener;
    private boolean installed;

    private final DecorationsColorProvider fallbackColorProvider = new DefaultDecorationsColorProvider();
    private DecorationsColorProvider colorProvider = fallbackColorProvider;

    private ExternalLafDecorator() {
        decorationsManager = new NativeDecorationsManager();
        propertyChangeListener = evt -> {
            if ("lookAndFeel".equals(evt.getPropertyName())) {
                installExtraProperties();
            }
        };
    }

    public static ExternalLafDecorator instance() {
        if (instance == null) instance = new ExternalLafDecorator();
        return instance;
    }

    public NativeDecorationsManager decorationsManager() {
        return decorationsManager;
    }

    public void install() {
        if (installed) return;
        installed = true;
        decorationsManager.initialize();
        UIManager.addPropertyChangeListener(propertyChangeListener);
        installExtraProperties();
    }

    public void uninstall() {
        if (!installed) return;
        installed = false;
        UIManager.removePropertyChangeListener(propertyChangeListener);
    }

    public void setColorProvider(final DecorationsColorProvider colorProvider) {
        this.colorProvider = colorProvider;
        if (colorProvider == null) {
            this.colorProvider = fallbackColorProvider;
        }
    }

    public boolean isInstalled() {
        return installed;
    }

    private void putOrCopy(final String key, final Object value, final Properties props, final UIDefaults uiDefaults) {
        Object other = uiDefaults.get(key);
        if (other == null) other = value;
        props.put(key, other);
    }

    private void installExtraProperties() {
        IconLoader.updateThemeStatus(new Object());
        colorProvider.onLafChanged();

        Properties props = new Properties();
        UIDefaults defaults = UIManager.getDefaults();

        putOrCopy("borderSecondary", colorProvider.borderColor(), props, defaults);
        putOrCopy("hoverHighlight", colorProvider.hoverBackgroundColor(), props, defaults);
        putOrCopy("clickHighlight", colorProvider.clickBackgroundColor(), props, defaults);

        putOrCopy("background", colorProvider.backgroundColor(), props, defaults);
        putOrCopy("textForeground", colorProvider.activeForegroundColor(), props, defaults);
        putOrCopy("textForegroundInactive", colorProvider.inactiveForegroundColor(), props, defaults);
        putOrCopy("textForegroundSecondary", colorProvider.inactiveForegroundColor(), props, defaults);

        putOrCopy("windowButton", colorProvider.windowButtonColor(), props, defaults);
        putOrCopy("windowButtonDisabled", colorProvider.inactiveWindowButtonColor(), props, defaults);
        putOrCopy("windowCloseHovered", new DarkColorUIResource(Color.WHITE), props, defaults);

        decorationsManager.loadDecorationProperties(props, defaults);

        props.remove("borderSecondary");
        props.remove("hoverHighlight");
        props.remove("clickHighlight");
        props.remove("background");
        props.remove("textForeground");
        props.remove("textForegroundInactive");
        props.remove("textForegroundSecondary");
        props.remove("windowButton");
        props.remove("windowButtonDisabled");
        props.remove("windowCloseHovered");

        defaults.putAll(props);
        defaults.put("RootPaneUI", BasicNativeDecorationsRootPaneUI.class.getName());
    }

    private static class DefaultDecorationsColorProvider implements DecorationsColorProvider {

        private Color foreground;
        private Color background;
        private Color disabledForeground;

        @Override
        public void onLafChanged() {
            JLabel label = new JLabel();
            label.setOpaque(true);
            foreground = new DarkColorUIResource(label.getForeground());
            background = new DarkColorUIResource(label.getBackground());
            label.setEnabled(false);
            disabledForeground = new DarkColorUIResource(label.getForeground());
        }

        @Override
        public Color backgroundColor() {
            return background;
        }

        @Override
        public Color activeForegroundColor() {
            return foreground;
        }

        @Override
        public Color inactiveForegroundColor() {
            return disabledForeground;
        }
    }
}
