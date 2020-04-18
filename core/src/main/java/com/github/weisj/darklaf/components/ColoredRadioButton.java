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
package com.github.weisj.darklaf.components;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.StateIcon;
import com.github.weisj.darklaf.task.AccentColorAdjustmentTask;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.togglebutton.radiobutton.DarkRadioButtonUI;
import com.github.weisj.darklaf.util.StringUtil;

import javax.swing.*;
import javax.swing.plaf.ButtonUI;
import java.awt.*;
import java.util.Properties;

public class ColoredRadioButton extends JRadioButton {

    public static final Color DEFAULT_FILLED = new Color(0);
    private Color color;

    public ColoredRadioButton(final String text, final Color color) {
        super(text, null, false);
        setColor(color);
    }

    public ColoredRadioButton(final String text, final boolean selected, final Color color) {
        super(text, null, selected);
        setColor(color);
    }

    public void setColor(final Color color) {
        this.color = color;
        ButtonUI ui = getUI();
        if (ui instanceof ColoredRadioButtonUI) {
            ((ColoredRadioButtonUI) ui).setColor(color);
        }
        SwingUtilities.invokeLater(this::updateUI);
    }

    public Color getColor() {
        return color;
    }

    @Override
    public void updateUI() {
        setUI(new ColoredRadioButtonUI(color));
    }

    protected static class ColoredRadioButtonUI extends DarkRadioButtonUI {

        private static final String[] PROPERTIES = {
            "Icons.RadioButton.activeFillColor", "Icons.RadioButtonSelected.selectedFillColor",
            "Icons.RadioButton.activeBorderColor", "Icons.RadioButtonSelected.selectedBorderColor",
            "Icons.RadioButtonDisabled.inactiveFillColor", "",
            "Icons.RadioButtonDisabled.inactiveBorderColor", "",
            "Icons.RadioButtonFocused.activeFillColor", "Icons.RadioButtonSelectedFocused.selectedFillColor",
            "Icons.RadioButtonFocused.focusBorderColor", "Icons.RadioButtonSelectedFocused.focusSelectedBorderColor",
            "Icons.RadioButtonFocused.glowFocus", "",
            "Icons.RadioButtonFocused.glowOpacity", "",
            "Icons.RadioButtonSelected.selectedFillColor", "",
            "Icons.RadioButtonSelected.selectedBorderColor", "",
            "Icons.RadioButtonSelected.selectionSelectedColor", "",
            "Icons.RadioButtonSelectedDisabled.inactiveFillColor", "",
            "Icons.RadioButtonSelectedDisabled.inactiveBorderColor", "",
            "Icons.RadioButtonSelectedDisabled.selectionDisabledColor", "",
            "Icons.RadioButtonSelectedFocused.selectedFillColor", "",
            "Icons.RadioButtonSelectedFocused.focusSelectedBorderColor", "",
            "Icons.RadioButtonSelectedFocused.selectionFocusSelectedColor", "",
            "Icons.RadioButtonSelectedFocused.glowFocus", "",
            "Icons.RadioButtonSelectedFocused.glowOpacity", ""
        };
        private static final String[] COLOR_PROPERTIES = {
            "Icons.RadioButton.activeFillColor",
            "Icons.RadioButton.activeBorderColor",
            "Icons.RadioButtonFocused.activeFillColor",
            "Icons.RadioButtonFocused.focusBorderColor",
            "Icons.RadioButtonSelected.selectedFillColor",
            "Icons.RadioButtonSelected.selectedBorderColor"
        };
        private Properties propertyMap;

        private Icon stateIcon;
        private final Color color;
        private static final AccentColorAdjustmentTask adjustment = new AccentColorAdjustmentTask();

        public ColoredRadioButtonUI(final Color color) {
            super();
            this.color = color;
        }

        @Override
        protected void installIcons() {
            super.installIcons();
            setColor(color);
        }

        public void setColor(final Color color) {
            if (color == null) return;
            IconLoader loader = IconLoader.get();
            Theme theme = LafManager.getTheme();
            Properties props = new Properties();
            UIDefaults defaults = UIManager.getLookAndFeelDefaults();
            theme.loadDefaults(props, defaults);
            Color c = color == DEFAULT_FILLED ? (Color) props.get("widgetFillDefault") : color;
            adjustment.applyColors(LafManager.getTheme(), props, color, null);
            PropertyLoader.putProperties(
                PropertyLoader.loadProperties(DarkLaf.class, "radioButton", "properties/ui/"),
                props, defaults);
            PropertyLoader.putProperties(
                PropertyLoader.loadProperties(DarkLaf.class, "radioButton", "properties/icons/"),
                props, defaults);
            propertyMap = new Properties();
            for (int i = 0; i < PROPERTIES.length; i += 2) {
                String prop = PROPERTIES[i];
                String key = PROPERTIES[i + 1];
                Object value = props.get(StringUtil.isBlank(key) ? prop : key);
                propertyMap.put(prop, value);
            }
            for (String prop : COLOR_PROPERTIES) {
                propertyMap.put(prop, c);
            }

            stateIcon = new StateIcon(new Icon[]{
                load(loader, "control/radio.svg"),
                load(loader, "control/radioDisabled.svg"),
                load(loader, "control/radioFocused.svg"),
                load(loader, "control/radioSelected.svg"),
                load(loader, "control/radioSelectedDisabled.svg"),
                load(loader, "control/radioSelectedFocused.svg")
            });
        }

        private Icon load(final IconLoader loader, final String name) {
            return loader.loadSVGIcon(name, 19, 19, true, propertyMap);
        }

        @Override
        protected Icon getStateIcon(final AbstractButton b) {
            return color != null ? stateIcon : super.getStateIcon(b);
        }
    }
}
