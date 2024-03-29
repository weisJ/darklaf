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
import java.util.function.Consumer;
import java.util.function.Function;

import javax.swing.*;

import com.github.weisj.darklaf.properties.color.DarkColorModelHSL;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.properties.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.ColorUtil;

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

    private void putOrCopy(final String key, final Function<DecorationsColorProvider, Color> getter,
            final Properties props, final UIDefaults uiDefaults) {
        Object other = uiDefaults.get(key);
        if (other == null) other = getter.apply(colorProvider);
        if (other == null) other = getter.apply(fallbackColorProvider);
        props.put(key, other);
    }

    private void installExtraProperties() {
        IconLoader.updateThemeStatus(new Object());
        colorProvider.onLafChanged();

        DecorationsColorProvider.TitleColor titleColor = colorProvider.windowTitleColor();

        Properties props = new Properties();
        UIDefaults defaults = UIManager.getDefaults();

        putOrCopy("borderSecondary", DecorationsColorProvider::borderColor, props, defaults);
        putOrCopy("hoverHighlight", DecorationsColorProvider::hoverBackgroundColor, props, defaults);
        putOrCopy("clickHighlight", DecorationsColorProvider::clickBackgroundColor, props, defaults);

        putOrCopy("background", DecorationsColorProvider::backgroundColor, props, defaults);
        putOrCopy("textForeground", DecorationsColorProvider::activeForegroundColor, props, defaults);
        putOrCopy("textForegroundInactive", DecorationsColorProvider::inactiveForegroundColor, props, defaults);
        putOrCopy("textForegroundSecondary", DecorationsColorProvider::inactiveForegroundColor, props, defaults);

        putOrCopy("windowButton", DecorationsColorProvider::windowButtonColor, props, defaults);
        putOrCopy("windowButtonDisabled", DecorationsColorProvider::inactiveWindowButtonColor, props, defaults);
        putOrCopy("windowCloseHovered", p -> new DarkColorUIResource(Color.WHITE), props, defaults);

        if (!decorationsManager.supportsNativeTitleText()) {
            Consumer<String> adjustColor = s -> props.put(s, adjustForegroundColor((Color) props.get(s), titleColor));
            adjustColor.accept("textForeground");
            adjustColor.accept("textForegroundSecondary");
            adjustColor.accept("textForegroundInactive");
        }

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
        defaults.put("Theme.dark", titleColor == DecorationsColorProvider.TitleColor.LIGHT);
        defaults.put("Theme.macos.useSwingTitleLabel", titleColor == DecorationsColorProvider.TitleColor.CUSTOM);
        defaults.put("RootPaneUI", BasicNativeDecorationsRootPaneUI.class.getName());
    }

    private Color adjustForegroundColor(final Color color, final DecorationsColorProvider.TitleColor titleColor) {
        if (titleColor == DecorationsColorProvider.TitleColor.CUSTOM) return color;
        final double[] hslFG = DarkColorModelHSL.RGBtoHSLValues(color.getRed(), color.getGreen(), color.getBlue());
        double brightness = titleColor == DecorationsColorProvider.TitleColor.LIGHT ? 0.9 : 0.1;
        return DarkColorModelHSL.getColorFromHSLValues(hslFG[0], hslFG[1], brightness);
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

        @Override
        public TitleColor windowTitleColor() {
            if (ColorUtil.getPerceivedBrightness(background) <= 125) return TitleColor.LIGHT;
            return TitleColor.DARK;
        }
    }
}
