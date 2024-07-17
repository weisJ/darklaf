/*
 * MIT License
 *
 * Copyright (c) 2020-2024 Jannis Weis
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
package com.github.weisj.darklaf.settings;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Dictionary;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SliderUI;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.ColoredRadioButton;
import com.github.weisj.darklaf.components.DynamicUI;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.delegate.ListCellRendererDelegate;
import com.github.weisj.darklaf.graphics.ThemedColor;
import com.github.weisj.darklaf.iconset.AllIcons;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.listener.UIUpdater;
import com.github.weisj.darklaf.nativelaf.ThemePreferencesHandler;
import com.github.weisj.darklaf.platform.macos.theme.MacOSColors;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.spec.AccentColorRule;
import com.github.weisj.darklaf.theme.spec.FontPrototype;
import com.github.weisj.darklaf.theme.spec.FontSizePreset;
import com.github.weisj.darklaf.theme.spec.FontSizeRule;
import com.github.weisj.darklaf.ui.combobox.ComboBoxConstants;
import com.github.weisj.darklaf.ui.slider.DarkSliderUI;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.StringUtil;

public class ThemeSettingsUI {

    public final static String THEME_LABEL_KEY = "settings.label_theme";
    public final static String SELECTION_COLOR_LABEL_KEY = "settings.label_selection_color";
    public final static String ACCENT_COLOR_LABEL_KEY = "settings.label_accent_color";
    public final static String FONT_SIZE_LABEL_KEY = "settings.label_font_size";
    public final static String GENERAL_LABEL_KEY = "settings.title_general";
    public final static String MONITORING_LABEL_KEY = "settings.title_monitoring";

    private final SettingsUIConfiguration settingsConfiguration;
    private final List<ChangeListener> listeners = new ArrayList<>();

    private final JComboBox<Theme> themeComboBox;

    private final RadioColorChooser selectionChooser;
    private final RadioColorChooser accentChooser;

    private final JSlider fontSlider;
    private final JCheckBox useCustomFontPrototype;
    private final JComboBox<FontEntry> fontPrototypeChooser;
    private final JComponent fontPrototypeChooserComponent;

    private final TristateCheckBox enabledSystemPreferences;
    private final JCheckBox fontSizeFollowsSystem;
    private final JCheckBox accentColorFollowsSystem;
    private final JCheckBox selectionColorFollowsSystem;
    private final JCheckBox themeFollowsSystem;

    public ThemeSettingsUI() {
        settingsConfiguration = new SettingsUIConfiguration();
        Color currentAccentColor = LafManager.getTheme().getAccentColorRule().getAccentColor();
        Color currentSelectionColor = LafManager.getTheme().getAccentColorRule().getSelectionColor();

        themeComboBox = createThemeComboBox();

        accentChooser = createAccentColorChooser(currentAccentColor);
        selectionChooser = createSelectionColorChooser(currentSelectionColor);

        List<ColoredRadioButton> accentButtons = accentChooser.getRadioButtons();
        List<ColoredRadioButton> selectionButtons = selectionChooser.getRadioButtons();
        for (int i = 0; i < accentButtons.size(); i++) {
            ColoredRadioButton button = accentButtons.get(i);
            ColoredRadioButton peer = selectionButtons.get(i);
            button.addActionListener(e -> {
                if (button.isSelected()) peer.setSelected(true);
            });
        }

        fontSlider = createFontSlider();
        useCustomFontPrototype = DynamicUI.withLocalizedText(new JCheckBox(), "settings.label_font_prototype");
        fontPrototypeChooser = new JComboBox<>(FontFamiliesCache.families);

        // Value is based on length of "Helvetica Neue"
        fontPrototypeChooser.setPrototypeDisplayValue(FontEntry.createDisplayValue(15));
        // noinspection unchecked
        fontPrototypeChooser.setRenderer(new ListCellRendererDelegate<>(
                (ListCellRenderer<FontEntry>) fontPrototypeChooser.getRenderer()) {
            @Override
            public Component getListCellRendererComponent(final JList<? extends FontEntry> list, final FontEntry value,
                    final int index, final boolean isSelected, final boolean cellHasFocus) {
                Component delegate = getDelegate()
                        .getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (index >= 0 && value != null) {
                    delegate.setFont(value.font);
                } else {
                    delegate.setFont(useCustomFontPrototype.getFont());
                }
                return delegate;
            }
        });

        fontPrototypeChooser.setEnabled(useCustomFontPrototype.isSelected());
        fontPrototypeChooserComponent = Box.createHorizontalBox();
        fontPrototypeChooserComponent.add(useCustomFontPrototype);
        fontPrototypeChooserComponent.add(Box.createHorizontalStrut(LayoutHelper.getDefaultSpacing()));
        fontPrototypeChooserComponent.add(fontPrototypeChooser);

        enabledSystemPreferences =
                DynamicUI.withLocalizedText(new TristateCheckBox(), "settings.check_system_preferences");
        accentColorFollowsSystem = createSystemSettingCheckBox("settings.check_system_accent_color",
                ThemePreferencesHandler::supportsNativeAccentColor);
        selectionColorFollowsSystem = createSystemSettingCheckBox("settings.check_system_selection_color",
                ThemePreferencesHandler::supportsNativeSelectionColor);
        fontSizeFollowsSystem = createSystemSettingCheckBox("settings.check_system_font",
                ThemePreferencesHandler::supportsNativeFontSize);
        themeFollowsSystem = createSystemSettingCheckBox("settings.check_system_theme",
                ThemePreferencesHandler::supportsNativeTheme);

        setupListeners();
    }

    private void setupListeners() {
        themeComboBox.addItemListener(e -> update());
        enabledSystemPreferences.addChangeListener(e -> {
            if (!enabledSystemPreferences.getTristateModel().isIndeterminate()) {
                boolean selected = enabledSystemPreferences.getTristateModel().isSelected();

                if (themeFollowsSystem.isEnabled()) themeFollowsSystem.setSelected(selected);
                if (accentColorFollowsSystem.isEnabled()) accentColorFollowsSystem.setSelected(selected);
                if (selectionColorFollowsSystem.isEnabled()) selectionColorFollowsSystem.setSelected(selected);
                if (fontSizeFollowsSystem.isEnabled()) fontSizeFollowsSystem.setSelected(selected);
            }
            update();
        });
        themeFollowsSystem.addActionListener(e -> update());

        useCustomFontPrototype.addItemListener(
                e -> fontPrototypeChooser.setEnabled(useCustomFontPrototype.isSelected()));

        accentColorFollowsSystem.addActionListener(e -> update());
        selectionColorFollowsSystem.addActionListener(e -> update());
        fontSizeFollowsSystem.addActionListener(e -> update());

        enabledSystemPreferences.setSelected(LafManager.isPreferenceChangeReportingEnabled());
    }

    private void update() {
        boolean enabled = !enabledSystemPreferences.getTristateModel().isDeselected();

        enabledSystemPreferences.setEnabled(settingsConfiguration.isSystemPreferencesSupported());

        themeFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemThemeSupported());
        accentColorFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemAccentColorSupported());
        selectionColorFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemSelectionColorSupported());
        fontSizeFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemFontSizeSupported());

        settingsConfiguration.setSystemPreferencesEnabled(settingsConfiguration.isSystemPreferencesEnabled());

        accentChooser.setEnabled(!settingsConfiguration.isAccentColorFollowsSystem()
                && settingsConfiguration.getSelectedTheme().supportsCustomAccentColor());
        selectionChooser.setEnabled(!settingsConfiguration.isSelectionColorFollowsSystem()
                && settingsConfiguration.getSelectedTheme().supportsCustomSelectionColor());

        themeComboBox.setEnabled(!settingsConfiguration.isThemeFollowsSystem());
        fontSlider.setEnabled(!settingsConfiguration.isFontSizeFollowsSystem());

        if (!listeners.isEmpty()) {
            ChangeEvent e = new ChangeEvent(this);
            for (ChangeListener listener : listeners) {
                listener.stateChanged(e);
            }
        }
    }

    protected void setAccentColor(final Color accentColor) {
        accentChooser.setColor(accentColor);
    }

    protected void setSelectionColor(final Color selectionColor) {
        selectionChooser.setColor(selectionColor);
    }

    public void loadConfiguration(final SettingsConfiguration configuration) {
        themeComboBox.setModel(LafManager.getThemeComboBoxModel());
        settingsConfiguration.load(configuration);
        settingsConfiguration.setAccentColorRule(settingsConfiguration.getAccentColorRule());
        update();
    }

    public SettingsConfiguration getSettingsConfiguration() {
        return settingsConfiguration;
    }

    /**
     * Add a listener which gets notified when a !potential! change is made to the settings.
     *
     * @param listener the listener to add.
     */
    public void addChangeListener(final ChangeListener listener) {
        listeners.add(listener);
    }

    /**
     * Removes a change listener.
     *
     * @param listener the listener to remove.
     */
    public void removeChangeListener(final ChangeListener listener) {
        listeners.remove(listener);
    }

    public JComboBox<Theme> getThemeComboBox() {
        return themeComboBox;
    }

    public RadioColorChooser getSelectionChooser() {
        return selectionChooser;
    }

    public RadioColorChooser getAccentChooser() {
        return accentChooser;
    }

    public JSlider getFontSlider() {
        return fontSlider;
    }

    public JComponent getFontPrototypeChooser() {
        return fontPrototypeChooserComponent;
    }

    public TristateCheckBox getSystemPreferencesTristateCheckBox() {
        return enabledSystemPreferences;
    }

    public JCheckBox getAccentColorFollowsSystemCheckBox() {
        return accentColorFollowsSystem;
    }

    public JCheckBox getSelectionColorFollowsSystemCheckBox() {
        return selectionColorFollowsSystem;
    }

    public JCheckBox getFontSizeFollowsSystemCheckBox() {
        return fontSizeFollowsSystem;
    }

    public JCheckBox getThemeFollowsSystemCheckBox() {
        return themeFollowsSystem;
    }

    private static JComboBox<Theme> createThemeComboBox() {
        JComboBox<Theme> comboBox = new JComboBox<>(LafManager.getThemeComboBoxModel());
        comboBox.setRenderer(LafManager.getThemeListCellRenderer());
        comboBox.putClientProperty(ComboBoxConstants.KEY_DO_NOT_UPDATE_WHEN_SCROLLED, true);
        return comboBox;
    }

    private static RadioColorChooser createAccentColorChooser(final Color currentAccentColor) {
        Color defaultAccentColor = new ThemedColor("themeAccentColor");
        return new RadioColorChooser(
                Arrays.asList(
                        new RadioColorChooser.ColorSpec(
                                ColoredRadioButton.DEFAULT_COLOR, defaultAccentColor, "settings.color_default"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_BLUE, null, "settings.color_blue"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_LILAC, null, "settings.color_lilac"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_ROSE, null, "settings.color_rose"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_RED, null, "settings.color_red"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_ORANGE, null, "settings.color_orange"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_YELLOW, null, "settings.color_yellow"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_GREEN, null, "settings.color_green"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.ACCENT_GRAPHITE, null, "settings.color_gray")),
                new RadioColorChooser.ColorSpec(
                        currentAccentColor, null, "settings.color_custom"),
                currentAccentColor, defaultAccentColor);
    }

    private static RadioColorChooser createSelectionColorChooser(final Color currentSelectionColor) {
        Color defaultSelectionColor = new ThemedColor("themeSelectionColor");
        return new RadioColorChooser(
                Arrays.asList(
                        new RadioColorChooser.ColorSpec(
                                defaultSelectionColor, ColoredRadioButton.DEFAULT_COLOR, "settings.color_default"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_BLUE, MacOSColors.ACCENT_BLUE, "settings.color_blue"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_PURPLE, MacOSColors.ACCENT_LILAC, "settings.color_purple"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_PINK, MacOSColors.ACCENT_ROSE, "settings.color_pink"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_RED, MacOSColors.ACCENT_RED, "settings.color_red"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_ORANGE, MacOSColors.ACCENT_ORANGE, "settings.color_orange"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_YELLOW, MacOSColors.ACCENT_YELLOW, "settings.color_yellow"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_GREEN, MacOSColors.ACCENT_GREEN, "settings.color_green"),
                        new RadioColorChooser.ColorSpec(
                                MacOSColors.SELECTION_GRAPHITE, MacOSColors.ACCENT_GRAPHITE, "settings.color_gray")),
                new RadioColorChooser.ColorSpec(
                        currentSelectionColor, defaultSelectionColor, "settings.color_custom"),
                currentSelectionColor, defaultSelectionColor);
    }

    private static JSlider createFontSlider() {
        JSlider fontSlider = new JSlider() {
            @Override
            public String getToolTipText(final MouseEvent event) {
                return getValue() + "%";
            }
        };
        ToolTipContext context = new ToolTipContext()
                .setAlignment(Alignment.CENTER)
                .setCenterAlignment(Alignment.NORTH)
                .setUseBestFit(true).setToolTipRectSupplier(e -> {
                    SliderUI ui = fontSlider.getUI();
                    if (ui instanceof DarkSliderUI) {
                        Rectangle r = ((DarkSliderUI) ui).getThumbRect();
                        r.x -= 1;
                        return r;
                    }
                    return new Rectangle(0, 0, fontSlider.getWidth(), fontSlider.getHeight());
                });
        fontSlider.putClientProperty(DarkSliderUI.KEY_INSTANT_SCROLL, true);
        fontSlider.putClientProperty(DarkSliderUI.KEY_USE_TRACK_AS_BASELINE, true);
        fontSlider.putClientProperty(ToolTipConstants.KEY_CONTEXT, context);
        fontSlider.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        fontSlider.setToolTipText(String.valueOf(fontSlider.getValue()));
        fontSlider.setOrientation(JSlider.HORIZONTAL);
        fontSlider.setPaintTicks(true);
        fontSlider.setMinimum(FontSizePreset.TINY.getPercentage());
        fontSlider.setMaximum(FontSizePreset.HUGE.getPercentage());
        int tickSpacing = 25;
        Dictionary<Integer, JComponent> dict = fontSlider.createStandardLabels(tickSpacing);
        JLabel min = (JLabel) dict.get(fontSlider.getMinimum());
        UIUpdater.registerComponent(min);
        min.setText("");
        min.setIcon(AllIcons.Action.DecreaseFontSize.get());
        min.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        min.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);

        JLabel mid = (JLabel) dict.get(fontSlider.getMinimum() + tickSpacing);
        UIUpdater.registerComponent(mid);
        dict.remove(fontSlider.getMinimum() + tickSpacing);
        dict.put(FontSizePreset.NORMAL.getPercentage(), mid);
        DynamicUI.withDynamic(mid,
                c -> c.setText(UIManager.getString("settings.label_font_default", fontSlider.getLocale())));

        mid.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        mid.setHorizontalTextPosition(JLabel.RIGHT);

        JLabel max = (JLabel) dict.get(fontSlider.getMaximum());
        max.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);
        max.setText("");
        max.setIcon(AllIcons.Action.IncreaseFontSize.get());
        max.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        max.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);
        UIUpdater.registerComponent(max);

        dict.remove(FontSizePreset.Small.getPercentage());
        dict.remove(FontSizePreset.SMALL.getPercentage());
        dict.remove(FontSizePreset.Large.getPercentage());
        dict.remove(FontSizePreset.LARGE.getPercentage());
        dict.remove(FontSizePreset.Huge.getPercentage());

        fontSlider.setLabelTable(dict);
        fontSlider.setMajorTickSpacing(tickSpacing);
        fontSlider.setMinorTickSpacing(tickSpacing);
        fontSlider.setPaintLabels(true);
        fontSlider.setSnapToTicks(true);
        return fontSlider;
    }

    private static JCheckBox createSystemSettingCheckBox(final String labelKey,
            final Function<ThemePreferencesHandler, Boolean> enabledCondition) {
        JCheckBox checkBox = DynamicUI.withLocalizedText(new JCheckBox() {
            @Override
            public void setEnabled(final boolean b) {
                boolean enabled = b && enabledCondition.apply(ThemePreferencesHandler.getSharedInstance());
                super.setEnabled(enabled);
            }
        }, labelKey);
        checkBox.setSelected(false);
        checkBox.setEnabled(false);
        return checkBox;
    }

    private static class FontEntry {
        private final String name;
        private Font font;

        private FontEntry(Font font) {
            this.font = font;
            this.name = font.getFontName();
        }

        private FontEntry(String family) {
            this.font = new Font(family, Font.PLAIN, 12);
            this.name = font.getFontName();
        }

        private static FontEntry createDisplayValue(int displayLength) {
            return new FontEntry(displayLength);
        }

        private FontEntry(int displayLength) {
            this.font = null;
            this.name = StringUtil.repeat("A", displayLength);
        }

        @Override
        public String toString() {
            return name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof FontEntry fontEntry)) return false;
            return Objects.equals(name, fontEntry.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    private static class FontFamiliesCache {
        private static final FontEntry[] families;

        static {
            families = getFontFamilies();
            DynamicUI.registerCallback(FontFamiliesCache.class, c -> new SwingWorker<Void, FontEntry>() {
                @Override
                protected Void doInBackground() {
                    float fontSize = new JLabel().getFont().getSize2D();
                    for (FontEntry family : families) {
                        family.font = family.font.deriveFont(fontSize);
                    }
                    return null;
                }
            }.execute(), false);
        }

        private static FontEntry[] getFontFamilies() {
            GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
            return Arrays.stream(env.getAvailableFontFamilyNames())
                    .map(FontEntry::new)
                    .filter(e -> e.font.canDisplayUpTo(e.toString()) == -1)
                    .toArray(FontEntry[]::new);
        }
    }

    private class SettingsUIConfiguration extends SettingsConfiguration {

        @Override
        public boolean isSystemPreferencesEnabled() {
            return !enabledSystemPreferences.getTristateModel().isDeselected() && isSystemPreferencesSupported();
        }

        @Override
        public boolean isAccentColorFollowsSystem() {
            return accentColorFollowsSystem.isSelected() && isSystemAccentColorSupported();
        }

        @Override
        public boolean isFontSizeFollowsSystem() {
            return fontSizeFollowsSystem.isSelected() && isSystemFontSizeSupported();
        }

        @Override
        public boolean isSelectionColorFollowsSystem() {
            return selectionColorFollowsSystem.isSelected() && isSystemSelectionColorSupported();
        }

        @Override
        public boolean isThemeFollowsSystem() {
            return themeFollowsSystem.isSelected() && isSystemThemeSupported();
        }

        @Override
        public boolean isSystemAccentColorSupported() {
            return super.isSystemAccentColorSupported() && getSelectedTheme().supportsCustomAccentColor();
        }

        @Override
        public boolean isSystemSelectionColorSupported() {
            return super.isSystemSelectionColorSupported() && getSelectedTheme().supportsCustomSelectionColor();
        }

        private Color getSelectedColor(final RadioColorChooser chooser) {
            Color c = chooser.getSelectedColor();
            return Objects.equals(c, chooser.getDefaultColor()) ? null : c;
        }

        @Override
        public AccentColorRule getAccentColorRule() {
            return AccentColorRule.fromColor(
                    getSelectedColor(ThemeSettingsUI.this.accentChooser),
                    getSelectedColor(ThemeSettingsUI.this.selectionChooser));
        }

        @Override
        public FontSizeRule getFontSizeRule() {
            return FontSizeRule.relativeAdjustment(fontSlider.getValue());
        }

        @Override
        public FontPrototype getFontPrototype() {
            FontEntry selected = ((FontEntry) fontPrototypeChooser.getSelectedItem());
            return useCustomFontPrototype.isSelected() && selected != null
                    ? FontPrototype.fromFont(selected.font)
                    : FontPrototype.getDefault();
        }

        @Override
        public Theme getTheme() {
            return getSelectedTheme();
        }

        private Theme getSelectedTheme() {
            Theme selected = (Theme) themeComboBox.getSelectedItem();
            if (selected == null) {
                selected = LafManager.getInstalledTheme();
                selected = selected != null ? selected : LafManager.getTheme();
                setTheme(selected);
            }
            return selected;
        }

        @Override
        public void setSystemPreferencesEnabled(final boolean enabled) {
            TristateState state = TristateState.DESELECTED;
            if (enabled && (isFontSizeFollowsSystem() || !fontSizeFollowsSystem.isEnabled())
                    && (isThemeFollowsSystem() || !themeFollowsSystem.isEnabled())
                    && (isAccentColorFollowsSystem() || !accentColorFollowsSystem.isEnabled())
                    && (isSelectionColorFollowsSystem() || !selectionColorFollowsSystem.isEnabled())) {
                state = TristateState.SELECTED;
            } else if (enabled) {
                enabledSystemPreferences.getTristateModel().setIndeterminate();
                return;
            }
            enabledSystemPreferences.getTristateModel().setState(state);
        }

        @Override
        public void setAccentColorFollowsSystem(final boolean accentColorFollowsSystem) {
            ThemeSettingsUI.this.accentColorFollowsSystem
                    .setSelected(accentColorFollowsSystem && isSystemAccentColorSupported(false));
        }

        @Override
        public void setFontSizeFollowsSystem(final boolean fontSizeFollowsSystem) {
            ThemeSettingsUI.this.fontSizeFollowsSystem
                    .setSelected(fontSizeFollowsSystem && isSystemFontSizeSupported(false));
        }

        @Override
        public void setSelectionColorFollowsSystem(final boolean selectionColorFollowsSystem) {
            ThemeSettingsUI.this.selectionColorFollowsSystem
                    .setSelected(selectionColorFollowsSystem && isSystemSelectionColorSupported(false));
        }

        @Override
        public void setThemeFollowsSystem(final boolean themeFollowsSystem) {
            ThemeSettingsUI.this.themeFollowsSystem
                    .setSelected(themeFollowsSystem && isSystemThemeSupported(false));
        }

        @Override
        public void setAccentColorRule(final AccentColorRule accentColorRule) {
            if (accentColorRule == null) {
                setAccentColorRule(null, null);
            } else {
                setAccentColorRule(accentColorRule.getAccentColor(), accentColorRule.getSelectionColor());
            }
        }

        protected void setAccentColorRule(final Color accentColor, final Color selectionColor) {
            setAccentColor(accentColor);
            setSelectionColor(selectionColor);
        }

        @Override
        public void setFontSizeRule(final FontSizeRule fontSizeRule) {
            if (fontSizeRule == null) {
                fontSlider.setValue(FontSizePreset.NORMAL.getPercentage());
            } else {
                fontSlider.setValue(fontSizeRule.getPercentage());
            }
        }

        @Override
        public void setFontPrototype(FontPrototype fontPrototype) {
            boolean validPrototype = fontPrototype != null && fontPrototype.family() != null;
            if (validPrototype) {
                fontPrototypeChooser.setSelectedItem(new FontEntry(fontPrototype.family()));
            }

            if (!validPrototype || fontPrototypeChooser.getSelectedItem() == null) {
                useCustomFontPrototype.setSelected(false);
                fontPrototypeChooser.setSelectedItem(new FontEntry(fontPrototypeChooser.getFont()));
            }
        }

        @Override
        public void setTheme(final Theme theme) {
            themeComboBox.setSelectedItem(LafManager.getClosestMatchForTheme(theme));
        }
    }
}
