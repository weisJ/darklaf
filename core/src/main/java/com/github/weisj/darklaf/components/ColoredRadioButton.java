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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.util.Properties;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.StateIcon;
import com.github.weisj.darklaf.task.AccentColorAdjustmentTask;
import com.github.weisj.darklaf.task.ForegroundColorGenerationTask;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.togglebutton.radiobutton.DarkRadioButtonUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class ColoredRadioButton extends JRadioButton {

    public static final Color DEFAULT_FILLED = new Color(0);
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

        private static final String[] PROPERTIES = {"Icons.RadioButton.activeFillColor",
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
        private static final String[] COLOR_PROPERTIES = {"Icons.RadioButton.activeFillColor",
                                                          "Icons.RadioButton.activeBorderColor",
                                                          "Icons.RadioButtonFocused.activeFillColor",
                                                          "Icons.RadioButtonFocused.focusBorderColor",
                                                          "Icons.RadioButtonSelected.selectedFillColor",
                                                          "Icons.RadioButtonSelected.selectedBorderColor",
                                                          "Icons.RadioButtonSelectedFocused.selectedFillColor",
                                                          "Icons.RadioButtonSelectedFocused.focusSelectedBorderColor"};
        private static final String[] FOCUS_COLOR_PROPERTIES = {"Icons.RadioButtonFocused.glowFocus",
                                                                "Icons.RadioButtonSelectedFocused.glowFocus"};
        private static final String[] FOREGROUND_PROPERTIES = {"Icons.RadioButtonSelected.selectionSelectedColor",
                                                               "Icons.RadioButtonSelectedFocused.selectionFocusSelectedColor"};
        private static final double MIN_FG_CONTRAST = 0.6;
        private Properties propertyMap;

        private Icon stateIcon;
        private Color color;
        private Color focusColor;

        private Color patchedColor;
        private Color patchedFocusColor;

        private boolean patched;
        private static final AccentColorAdjustmentTask adjustment = new AccentColorAdjustmentTask();

        public ColoredRadioButtonUI(final Color color, final Color focusColor) {
            super();
            this.color = color;
            this.focusColor = focusColor;
        }

        @Override
        protected void installIcons() {
            super.installIcons();
            patchColors(color, focusColor);
        }

        public void setColors(final Color color, final Color focusColor) {
            this.color = color;
            this.focusColor = focusColor != null ? focusColor : color;
        }

        @Override
        public void update(final Graphics g, final JComponent c) {
            if (patchedColor != color || patchedFocusColor != focusColor) {
                patchColors(color, focusColor);
            }
            super.update(g, c);
        }

        private void patchColors(final Color color, final Color focusColor) {
            if (color == null || (patched && patchedColor == color && patchedFocusColor == focusColor)) {
                return;
            }
            this.patchedColor = color;
            this.patchedFocusColor = focusColor;
            IconLoader loader = DarkUIUtil.ICON_LOADER;
            Theme theme = LafManager.getTheme();
            Properties props = new Properties();
            UIDefaults defaults = UIManager.getLookAndFeelDefaults();
            theme.loadDefaults(props, defaults);
            Color accentCol = color == DEFAULT_FILLED ? (Color) props.get("widgetFillDefault") : color;
            Color focusCol = focusColor == DEFAULT_FILLED ? accentCol : focusColor;
            adjustment.applyColors(LafManager.getTheme(), props, accentCol, null);
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "radioButton",
                                                                       "properties/ui/"),
                                         props, defaults);
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "radioButton",
                                                                       "properties/icons/"),
                                         props, defaults);
            propertyMap = new Properties();
            for (String prop : PROPERTIES) {
                propertyMap.put(prop, props.get(prop));
            }
            for (String prop : COLOR_PROPERTIES) {
                propertyMap.put(prop, accentCol);
            }
            if (focusColor != DEFAULT_FILLED) {
                for (String prop : FOCUS_COLOR_PROPERTIES) {
                    propertyMap.put(prop, focusCol);
                }
            }
            for (String prop : FOREGROUND_PROPERTIES) {
                Color fg = ForegroundColorGenerationTask.makeAdjustedForeground((Color) props.get(prop),
                                                                                accentCol,
                                                                                MIN_FG_CONTRAST);
                propertyMap.put(prop, fg);
            }

            stateIcon = new StateIcon(new Icon[]{load(loader, "control/radio.svg"),
                                                 load(loader, "control/radioDisabled.svg"),
                                                 load(loader, "control/radioFocused.svg"),
                                                 load(loader, "control/radioSelected.svg"),
                                                 load(loader, "control/radioSelectedDisabled.svg"),
                                                 load(loader, "control/radioSelectedFocused.svg")});
            patched = true;
        }

        private Icon load(final IconLoader loader, final String name) {
            return loader.loadSVGIcon(name, 19, 19, true, propertyMap);
        }

        @Override
        protected Icon getStateIcon(final AbstractButton b) {
            return patched ? stateIcon : super.getStateIcon(b);
        }
    }
}
