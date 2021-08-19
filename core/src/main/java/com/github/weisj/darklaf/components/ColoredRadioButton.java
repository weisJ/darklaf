/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
import java.util.Objects;
import java.util.Properties;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.iconset.IconSet;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.properties.icons.StateIcon;
import com.github.weisj.darklaf.task.AccentColorAdjustmentTask;
import com.github.weisj.darklaf.task.ForegroundColorGenerationTask;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.togglebutton.radiobutton.DarkRadioButtonUI;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public class ColoredRadioButton extends JRadioButton {

    public static final Color DEFAULT_COLOR = new Color(0) {
        @Override
        public boolean equals(final Object obj) {
            return obj == this;
        }
    };
    private Color color;
    private Color focusColor;

    public ColoredRadioButton(final String text, final Color color) {
        super(text, null, false);
        setColors(color, color);
    }

    public ColoredRadioButton(final String text, final boolean selected, final Color color) {
        super(text, null, selected);
        setColors(color, color);
    }

    public ColoredRadioButton(final String text, final Color color, final Color focusColor) {
        super(text, null, false);
        setColors(color, focusColor);
    }

    public ColoredRadioButton(final String text, final boolean selected, final Color color, final Color focusColor) {
        super(text, null, selected);
        setColors(color, focusColor);
    }

    public void setColors(final Color color, final Color focusColor) {
        this.color = color;
        this.focusColor = focusColor != null ? focusColor : color;
        updateColorUI();
    }

    public void setColor(final Color color) {
        this.color = color;
        updateColorUI();
    }

    public void setFocusColor(final Color color) {
        this.focusColor = color;
        updateColorUI();
    }

    private void updateColorUI() {
        ColoredRadioButtonUI ui = DarkUIUtil.getUIOfType(getUI(), ColoredRadioButtonUI.class);
        if (ui != null) ui.setColors(color, focusColor);
        repaint();
    }

    public Color getColor() {
        return color;
    }

    @Override
    protected void setUI(final ComponentUI newUI) {
        if (!(newUI instanceof ColoredRadioButtonUI)) {
            throw new IllegalArgumentException("UI must be of type ColoredRadioButtonUI");
        }
        super.setUI(newUI);
    }

    @Override
    public void updateUI() {
        setUI(new ColoredRadioButtonUI(color, focusColor));
    }

    protected static class ColoredRadioButtonUI extends DarkRadioButtonUI {

        private static final String[] PROPERTIES = {
                "Icons.RadioButton.activeFillColor",
                "Icons.RadioButton.activeBorderColor",
                "Icons.RadioButtonDisabled.inactiveFillColor",
                "Icons.RadioButtonDisabled.inactiveBorderColor",
                "Icons.RadioButtonFocused.activeFillColor",
                "Icons.RadioButtonFocused.focusBorderColor",
                "Icons.RadioButtonFocused.glowFocus",
                "Icons.RadioButtonFocused.glowOpacity",
                "Icons.RadioButtonSelected.selectedFillColor",
                "Icons.RadioButtonSelected.selectedBorderColor",
                "Icons.RadioButtonSelected.selectionSelectedColor",
                "Icons.RadioButtonSelectedDisabled.inactiveFillColor",
                "Icons.RadioButtonSelectedDisabled.inactiveBorderColor",
                "Icons.RadioButtonSelectedDisabled.selectionDisabledColor",
                "Icons.RadioButtonSelectedFocused.selectedFillColor",
                "Icons.RadioButtonSelectedFocused.focusSelectedBorderColor",
                "Icons.RadioButtonSelectedFocused.selectionFocusSelectedColor",
                "Icons.RadioButtonSelectedFocused.glowFocus",
                "Icons.RadioButtonSelectedFocused.glowOpacity"};
        private static final String[] COLOR_PROPERTIES = {
                "Icons.RadioButton.activeFillColor",
                "Icons.RadioButton.activeBorderColor",
                "Icons.RadioButtonFocused.activeFillColor",
                "Icons.RadioButtonFocused.focusBorderColor",
                "Icons.RadioButtonSelected.selectedFillColor",
                "Icons.RadioButtonSelected.selectedBorderColor",
                "Icons.RadioButtonSelectedFocused.selectedFillColor",
                "Icons.RadioButtonSelectedFocused.focusSelectedBorderColor"};
        private static final String[] FOCUS_COLOR_PROPERTIES = {
                "Icons.RadioButtonFocused.glowFocus",
                "Icons.RadioButtonSelectedFocused.glowFocus"};
        private static final String[] FOREGROUND_PROPERTIES = {
                "Icons.RadioButtonSelected.selectionSelectedColor",
                "Icons.RadioButtonSelectedFocused.selectionFocusSelectedColor"};
        private static final double MIN_FG_CONTRAST = 0.6;
        private Properties propertyMap;

        private Icon stateIcon;
        private Color iconColor;
        private Color focusIconColor;

        private Color patchedColor;
        private Color patchedFocusColor;

        private boolean patched;
        private static final AccentColorAdjustmentTask adjustment = new AccentColorAdjustmentTask();

        public ColoredRadioButtonUI(final Color iconColor, final Color focusIconColor) {
            super();
            this.iconColor = iconColor;
            this.focusIconColor = focusIconColor;
        }

        @Override
        protected void installIcons() {
            super.installIcons();
            patchColors(iconColor, focusIconColor);
        }

        public void setColors(final Color color, final Color focusColor) {
            this.iconColor = color;
            this.focusIconColor = focusColor != null ? focusColor : color;
        }

        @Override
        public void update(final Graphics g, final JComponent c) {
            if (!Objects.equals(patchedColor, iconColor) || Objects.equals(patchedFocusColor, focusIconColor)) {
                patchColors(iconColor, focusIconColor);
            }
            super.update(g, c);
        }

        private void patchColors(final Color color, final Color focusColor) {
            if (color == null
                    || (patched && Objects.equals(patchedColor, color)
                            && Objects.equals(patchedFocusColor, focusColor))) {
                return;
            }
            this.patchedColor = color;
            this.patchedFocusColor = focusColor;
            IconLoader iconLoader = DarkUIUtil.radioButtonLoader();

            Theme theme = LafManager.getInstalledTheme();
            Properties props = new Properties();
            UIDefaults defaults = UIManager.getLookAndFeelDefaults();
            theme.loadDefaults(props, defaults, iconLoader);
            Color accentCol = DEFAULT_COLOR.equals(color) ? (Color) props.get("widgetFillDefault") : color;
            Color focusCol = DEFAULT_COLOR.equals(focusColor) ? accentCol : focusColor;
            adjustment.applyColors(theme, props, accentCol, null);
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "radioButton", "ui/"),
                    props, defaults, iconLoader);
            PropertyLoader.putProperties(PropertyLoader.loadProperties(IconSet.class, "radioButton", ""),
                    props, defaults, iconLoader);
            propertyMap = new Properties();
            for (String prop : PROPERTIES) {
                propertyMap.put(prop, props.get(prop));
            }
            for (String prop : COLOR_PROPERTIES) {
                propertyMap.put(prop, accentCol);
            }
            if (!DEFAULT_COLOR.equals(focusColor)) {
                for (String prop : FOCUS_COLOR_PROPERTIES) {
                    propertyMap.put(prop, focusCol);
                }
            }
            for (String prop : FOREGROUND_PROPERTIES) {
                Color fg = ForegroundColorGenerationTask.makeAdjustedForeground((Color) props.get(prop), accentCol,
                        ForegroundColorGenerationTask.Bias.BACKGROUND, MIN_FG_CONTRAST);
                propertyMap.put(prop, fg);
            }

            stateIcon = new StateIcon(new Icon[] {
                    load(iconLoader, "control/radio.svg"),
                    load(iconLoader, "control/radioDisabled.svg"),
                    load(iconLoader, "control/radioFocused.svg"),
                    load(iconLoader, "control/radioSelected.svg"),
                    load(iconLoader, "control/radioSelectedDisabled.svg"),
                    load(iconLoader, "control/radioSelectedFocused.svg")});
            patched = true;
        }

        private Icon load(final IconLoader loader, final String name) {
            return loader.loadSVGIcon(name, -1, -1, true, propertyMap);
        }

        @Override
        protected Icon getStateIcon(final AbstractButton b) {
            return patched ? stateIcon : super.getStateIcon(b);
        }
    }
}
