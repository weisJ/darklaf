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
package com.github.weisj.darklaf.settings;

import java.awt.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.ChangeListener;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.DynamicUI;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

public class ThemeSettingsPanel extends JPanel {

    private final ThemeSettingsUI settingsUI;

    public ThemeSettingsPanel() {
        this(GroupLayout.Alignment.TRAILING, new Insets(LayoutHelper.getDefaultSpacing(), 0, 0, 0));
    }

    public ThemeSettingsPanel(final GroupLayout.Alignment alignment, final Insets insets) {
        settingsUI = new ThemeSettingsUI();
        setLayout(new BorderLayout());
        setBorder(LayoutHelper.createEmptyContainerBorder());
        add(createGeneralSettings(alignment, insets), BorderLayout.CENTER);
        add(createMonitorSettings(alignment, insets), BorderLayout.SOUTH);
    }

    public void loadConfiguration(final SettingsConfiguration configuration) {
        settingsUI.loadConfiguration(configuration);
    }

    public void setThemeComboBoxRenderer(final ListCellRenderer<Theme> renderer) {
        settingsUI.getThemeComboBox().setRenderer(renderer != null ? renderer : LafManager.getThemeListCellRenderer());
    }

    /**
     * Add a listener which gets notified when a !potential! change is made to the settings.
     *
     * @param listener the listener to add.
     */
    public void addChangeListener(final ChangeListener listener) {
        settingsUI.addChangeListener(listener);
    }

    /**
     * Removes a change listener.
     *
     * @param listener the listener to remove.
     */
    public void removeChangeListener(final ChangeListener listener) {
        settingsUI.removeChangeListener(listener);
    }

    protected void updateConfiguration() {
        ThemeSettings.getInstance().setConfiguration(getSettingsConfiguration());
    }

    public SettingsConfiguration getSettingsConfiguration() {
        return settingsUI.getSettingsConfiguration();
    }

    private static JLabel createDynamicLabel(final String key) {
        return DynamicUI.withDynamic(new JLabel(), c -> c.setText(UIManager.getString(key, c.getLocale())));
    }

    private JComponent createGeneralSettings(final GroupLayout.Alignment alignment, final Insets insets) {
        Locale l = getLocale();

        JLabel themeLabel = createDynamicLabel(ThemeSettingsUI.THEME_LABEL_KEY);

        JComboBox<Theme> themeComboBox = settingsUI.getThemeComboBox();
        themeLabel.setLabelFor(themeComboBox);

        JComponent themeBox = new JPanel(new FlowLayout(FlowLayout.LEFT));
        themeBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        themeBox.add(themeLabel);
        themeBox.add(themeComboBox);
        add(themeBox);

        JComponent selectionBox = Box.createHorizontalBox();
        JLabel selectionColorLabel = createDynamicLabel(ThemeSettingsUI.SELECTION_COLOR_LABEL_KEY);
        selectionColorLabel.setLabelFor(selectionBox);
        settingsUI.getSelectionChooser().getRadioButtons().forEach(selectionBox::add);

        JComponent accentBox = Box.createHorizontalBox();
        JLabel accentColorLabel = createDynamicLabel(ThemeSettingsUI.ACCENT_COLOR_LABEL_KEY);
        accentColorLabel.setLabelFor(accentBox);
        settingsUI.getAccentChooser().getRadioButtons().forEach(accentBox::add);

        JSlider fontSlider = settingsUI.getFontSlider();
        JLabel fontSizeLabel = createDynamicLabel(ThemeSettingsUI.FONT_SIZE_LABEL_KEY);
        fontSizeLabel.setLabelFor(fontSlider);
        JPanel panel = DynamicUI.withDynamic(new JPanel(new BorderLayout()),
                c -> c.setBorder(BorderFactory
                        .createTitledBorder(UIManager.getString(ThemeSettingsUI.GENERAL_LABEL_KEY, getLocale()))));
        JComponent c = LayoutHelper.createTwoColumnPanel(
                new JLabel[] {themeLabel, accentColorLabel, selectionColorLabel, fontSizeLabel},
                new JComponent[] {themeComboBox, accentBox, selectionBox, fontSlider},
                alignment, GroupLayout.Alignment.LEADING);
        c.setBorder(LayoutHelper.createEmptyBorder(insets));
        panel.add(c);
        return panel;
    }

    private Component createMonitorSettings(final GroupLayout.Alignment alignment, final Insets insets) {
        TristateCheckBox enabledSystemPreferences = settingsUI.getSystemPreferencesTristateCheckBox();
        Insets ins = new Insets(insets.top, insets.left, insets.bottom, insets.right);
        if (alignment == GroupLayout.Alignment.LEADING) {
            Insets padding = ((VisualPaddingProvider) enabledSystemPreferences.getBorder())
                    .getVisualPaddings(enabledSystemPreferences);
            ins = DarkUIUtil.addInsets(ins, DarkUIUtil.invert(padding));
        }

        JPanel panel = DynamicUI.withDynamic(new JPanel(new BorderLayout()),
                c -> c.setBorder(BorderFactory
                        .createTitledBorder(UIManager.getString(ThemeSettingsUI.MONITORING_LABEL_KEY, getLocale()))));
        JComponent c = LayoutHelper.createTwoColumnPanel(
                new JComponent[] {enabledSystemPreferences, settingsUI.getThemeFollowsSystemCheckBox(),
                        settingsUI.getAccentColorFollowsSystemCheckBox()},
                new JComponent[] {new JLabel(), settingsUI.getFontSizeFollowsSystemCheckBox(),
                        settingsUI.getSelectionColorFollowsSystemCheckBox()},
                GroupLayout.Alignment.LEADING, GroupLayout.Alignment.LEADING);
        c.setBorder(LayoutHelper.createEmptyBorder(ins));
        panel.add(c);
        return panel;
    }
}
