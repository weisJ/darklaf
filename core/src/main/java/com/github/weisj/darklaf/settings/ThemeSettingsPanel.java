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
package com.github.weisj.darklaf.settings;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.function.Supplier;

import javax.swing.*;
import javax.swing.plaf.SliderUI;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.ColoredRadioButton;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.graphics.ThemedColor;
import com.github.weisj.darklaf.listener.UIUpdater;
import com.github.weisj.darklaf.platform.ThemePreferencesHandler;
import com.github.weisj.darklaf.platform.macos.theme.MacOSColors;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.theme.info.FontSizePreset;
import com.github.weisj.darklaf.theme.info.FontSizeRule;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import com.github.weisj.darklaf.ui.combobox.ComboBoxConstants;
import com.github.weisj.darklaf.ui.slider.DarkSliderUI;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Alignment;

public class ThemeSettingsPanel extends JPanel {

    private final ResourceBundle resourceBundle;

    private final SettingsPanelConfiguration settingsConfiguration;

    private JCheckBox fontSizeFollowsSystem;
    private JCheckBox accentColorFollowsSystem;
    private JCheckBox selectionColorFollowsSystem;
    private JComboBox<Theme> themeComboBox;
    private JCheckBox themeFollowsSystem;
    private JSlider fontSlider;
    private TristateCheckBox enabledSystemPreferences;

    private ButtonGroup bgSelection;
    private ButtonGroup bgAccent;

    private Color defaultAccentColor;
    private Color defaultSelectionColor;
    private ColoredRadioButton customAccent;
    private ColoredRadioButton defaultAccent;
    private ColoredRadioButton customSelection;
    private ColoredRadioButton defaultSelection;

    public ThemeSettingsPanel(final ResourceBundle resourceBundle) {
        this.resourceBundle = resourceBundle;
        this.settingsConfiguration = new SettingsPanelConfiguration();
        init();
    }

    protected void init() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        add(createGeneralSettings(), BorderLayout.CENTER);
        add(createMonitorSettings(), BorderLayout.SOUTH);
    }

    public void loadConfiguration(final SettingsConfiguration configuration) {
        themeComboBox.setModel(LafManager.getThemeComboBoxModel());
        settingsConfiguration.load(configuration);
        settingsConfiguration.setAccentColorRule(settingsConfiguration.getAccentColorRule());

        update();
    }

    private void update() {
        boolean enabled = !enabledSystemPreferences.getTristateModel().isDeselected();

        enabledSystemPreferences.setEnabled(settingsConfiguration.isSystemPreferencesSupported());

        themeFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemThemeSupported());
        accentColorFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemAccentColorSupported());
        selectionColorFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemSelectionColorSupported());
        fontSizeFollowsSystem.setEnabled(enabled && settingsConfiguration.isSystemFontSizeSupported());

        settingsConfiguration.setSystemPreferencesEnabled(settingsConfiguration.isSystemPreferencesEnabled());

        enableButtonGroup(bgAccent, !settingsConfiguration.isAccentColorFollowsSystem()
                && settingsConfiguration.getSelectedTheme().supportsCustomAccentColor());
        enableButtonGroup(bgSelection, !settingsConfiguration.isSelectionColorFollowsSystem()
                && settingsConfiguration.getSelectedTheme().supportsCustomSelectionColor());
        themeComboBox.setEnabled(!settingsConfiguration.isThemeFollowsSystem());
        fontSlider.setEnabled(!settingsConfiguration.isFontSizeFollowsSystem());
    }

    public void updateConfiguration() {
        ThemeSettings.getInstance().setConfiguration(settingsConfiguration);
    }

    private boolean updateButtonGroup(final ButtonGroup bg, final Color currentColor,
            final AbstractButton defaultButton, final Color defaultColor) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            ColoredRadioButton radioButton = ((ColoredRadioButton) buttons.nextElement());
            boolean selected = Objects.equals(radioButton.getColor(), currentColor)
                    || (radioButton == defaultButton && Objects.equals(defaultColor, currentColor));
            bg.setSelected(radioButton.getModel(), selected);
            if (selected)
                return true;
        }
        return false;
    }

    protected Color getSelectedColor(final ButtonGroup bg, final AbstractButton defaultButton) {
        Enumeration<AbstractButton> enumeration = bg.getElements();
        while (enumeration.hasMoreElements()) {
            ColoredRadioButton button = (ColoredRadioButton) enumeration.nextElement();
            if (button.isSelected()) {
                Color c = button.getColor();
                return button != defaultButton ? c : null;
            }
        }
        return null;
    }

    private Component createGeneralSettings() {
        JLabel themeLabel = new JLabel(resourceBundle.getString("label_theme"));
        themeComboBox = new JComboBox<>(LafManager.getThemeComboBoxModel());
        themeComboBox.setRenderer(LafManager.getThemeListCellRenderer());
        themeComboBox.setSelectedItem(LafManager.getTheme());

        themeComboBox.putClientProperty(ComboBoxConstants.KEY_DO_NOT_UPDATE_WHEN_SCROLLED, true);
        themeComboBox.addItemListener(e -> update());
        themeLabel.setLabelFor(themeComboBox);

        JComponent themeBox = new JPanel(new FlowLayout(FlowLayout.LEFT));
        themeBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        themeBox.add(themeLabel);
        themeBox.add(themeComboBox);
        add(themeBox);

        Color currentAccentColor = LafManager.getTheme().getAccentColorRule().getAccentColor();
        Color currentSelectionColor = LafManager.getTheme().getAccentColorRule().getSelectionColor();

        JComponent selectionBox = Box.createHorizontalBox();
        JLabel selectionColorLabel = new JLabel(resourceBundle.getString("label_selection_color"));
        selectionColorLabel.setLabelFor(selectionBox);

        bgSelection = new ButtonGroup();
        defaultSelectionColor = createDefaultColor("themeSelectionColor");
        defaultSelection = addColoredButton(bgSelection, selectionBox, defaultSelectionColor,
                ColoredRadioButton.DEFAULT_COLOR, resourceBundle.getString("color_default"));
        AbstractButton selectionBlue = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_BLUE,
                MacOSColors.ACCENT_BLUE, resourceBundle.getString("color_blue"));
        AbstractButton selectionPurple = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_PURPLE,
                MacOSColors.ACCENT_LILAC, resourceBundle.getString("color_purple"));
        AbstractButton selectionPink = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_PINK,
                MacOSColors.ACCENT_ROSE, resourceBundle.getString("color_pink"));
        AbstractButton selectionRed = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_RED,
                MacOSColors.ACCENT_RED, resourceBundle.getString("color_red"));
        AbstractButton selectionOrange = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_ORANGE,
                MacOSColors.ACCENT_ORANGE, resourceBundle.getString("color_orange"));
        AbstractButton selectionYellow = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_YELLOW,
                MacOSColors.ACCENT_YELLOW, resourceBundle.getString("color_yellow"));
        AbstractButton selectionGreen = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_GREEN,
                MacOSColors.ACCENT_GREEN, resourceBundle.getString("color_green"));
        AbstractButton selectionGraphite = addColoredButton(bgSelection, selectionBox, MacOSColors.SELECTION_GRAPHITE,
                MacOSColors.ACCENT_GRAPHITE, resourceBundle.getString("color_gray"));
        customSelection = addCustomButton(bgSelection, selectionBox, currentSelectionColor, defaultSelectionColor,
                resourceBundle.getString("color_custom"));

        JComponent accentBox = Box.createHorizontalBox();
        JLabel accentColorLabel = new JLabel(resourceBundle.getString("label_accent_color"));
        accentColorLabel.setLabelFor(accentBox);

        defaultAccentColor = createDefaultColor("themeAccentColor");
        bgAccent = new ButtonGroup();
        defaultAccent = addColoredButton(bgAccent, accentBox, ColoredRadioButton.DEFAULT_COLOR, defaultSelection,
                resourceBundle.getString("color_default"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_BLUE, selectionBlue,
                resourceBundle.getString("color_blue"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_LILAC, selectionPurple,
                resourceBundle.getString("color_lilac"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_ROSE, selectionPink,
                resourceBundle.getString("color_rose"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_RED, selectionRed,
                resourceBundle.getString("color_red"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_ORANGE, selectionOrange,
                resourceBundle.getString("color_orange"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_YELLOW, selectionYellow,
                resourceBundle.getString("color_yellow"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_GREEN, selectionGreen,
                resourceBundle.getString("color_green"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ACCENT_GRAPHITE, selectionGraphite,
                resourceBundle.getString("color_gray"));
        customAccent = addCustomButton(bgAccent, accentBox, currentAccentColor, defaultAccentColor,
                resourceBundle.getString("color_custom"));

        fontSlider = createFontSlider();
        JLabel fontSizeLabel = new JLabel(resourceBundle.getString("label_font_size"));
        fontSizeLabel.setLabelFor(fontSlider);

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(BorderFactory.createTitledBorder(resourceBundle.getString("title_general")));
        panel.add(getTwoColumnLayout(new JLabel[] {themeLabel, accentColorLabel, selectionColorLabel, fontSizeLabel},
                new JComponent[] {themeComboBox, accentBox, selectionBox, fontSlider}));
        return panel;
    }

    protected ColoredRadioButton addCustomButton(final ButtonGroup bg, final JComponent parent,
            final Color currentColor, final Color defaultColor, final String tipText) {
        Supplier<Color> supplier = () -> Optional.ofNullable(currentColor).orElse(defaultColor);
        ColoredRadioButton button = addColoredButton(bg, parent, supplier, tipText);
        QuickColorChooser.attachToComponent(button, button::setColor,
                () -> Optional.ofNullable(button.getColor()).orElse(supplier.get()), button::isSelected);
        return button;
    }

    protected Color createDefaultColor(final String key) {
        return new ThemedColor(key);
    }

    protected void setAccentColor(final Color accentColor) {
        setXColor(accentColor, bgAccent, customAccent, defaultAccent, defaultAccentColor);
    }

    protected void setSelectionColor(final Color selectionColor) {
        setXColor(selectionColor, bgSelection, customSelection, defaultSelection, defaultSelectionColor);
    }

    protected void setXColor(final Color color, final ButtonGroup bg, final ColoredRadioButton customButton,
            final ColoredRadioButton defaultButton, final Color defaultColor) {
        if (color == null) {
            defaultButton.setSelected(true);
            return;
        }
        if (!updateButtonGroup(bg, color, defaultButton, defaultColor)) {
            customButton.setSelected(true);
            if (customButton.getColor() == null)
                customButton.setColor(color);
        }
    }

    private JSlider createFontSlider() {
        JSlider fontSlider = new JSlider() {
            @Override
            public String getToolTipText(final MouseEvent event) {
                return getValue() + "%";
            }
        };
        ToolTipContext context = new ToolTipContext().setAlignment(Alignment.CENTER).setCenterAlignment(Alignment.NORTH)
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
        fontSlider.putClientProperty(ToolTipConstants.KEY_CONTEXT, context);
        fontSlider.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        fontSlider.setToolTipText(String.valueOf(fontSlider.getValue()));
        fontSlider.setOrientation(JSlider.HORIZONTAL);
        fontSlider.setPaintTicks(true);
        fontSlider.setMinimum(FontSizePreset.TINY.getPercentage());
        fontSlider.setMaximum(FontSizePreset.HUGE.getPercentage());
        int tickSpacing = 25;
        Dictionary<Integer, JComponent> dict = fontSlider.createStandardLabels(tickSpacing);
        JLabel min = ((JLabel) dict.get(fontSlider.getMinimum()));
        UIUpdater.registerComponent(min);
        min.setText(resourceBundle.getString("label_font_smaller"));
        min.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        min.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);

        JLabel mid = ((JLabel) dict.get(fontSlider.getMinimum() + tickSpacing));
        UIUpdater.registerComponent(mid);
        dict.remove(fontSlider.getMinimum() + tickSpacing);
        dict.put(FontSizePreset.NORMAL.getPercentage(), mid);
        mid.setText(resourceBundle.getString("label_font_default"));
        mid.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        mid.setHorizontalTextPosition(JLabel.RIGHT);

        JLabel max = ((JLabel) dict.get(fontSlider.getMaximum()));
        max.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);
        max.setText(resourceBundle.getString("label_font_bigger"));
        max.setAlignmentX(JComponent.RIGHT_ALIGNMENT);
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

    private Component createMonitorSettings() {
        accentColorFollowsSystem = new JCheckBox(resourceBundle.getString("check_system_accent_color")) {
            @Override
            public void setEnabled(final boolean b) {
                boolean enabled = b && ThemePreferencesHandler.getSharedInstance().supportsNativeAccentColor();
                super.setEnabled(enabled);
            }
        };
        accentColorFollowsSystem.setEnabled(false);
        accentColorFollowsSystem.setSelected(false);

        selectionColorFollowsSystem = new JCheckBox(resourceBundle.getString("check_system_selection_color")) {
            @Override
            public void setEnabled(final boolean b) {
                boolean enabled = b && ThemePreferencesHandler.getSharedInstance().supportsNativeSelectionColor();
                super.setEnabled(enabled);
            }
        };
        selectionColorFollowsSystem.setEnabled(false);
        selectionColorFollowsSystem.setSelected(false);

        fontSizeFollowsSystem = new JCheckBox(resourceBundle.getString("check_system_font")) {
            @Override
            public void setEnabled(final boolean b) {
                boolean enabled = b && ThemePreferencesHandler.getSharedInstance().supportsNativeFontSize();
                super.setEnabled(enabled);
            }
        };
        fontSizeFollowsSystem.setEnabled(false);
        fontSizeFollowsSystem.setSelected(false);

        themeFollowsSystem = new JCheckBox(resourceBundle.getString("check_system_theme")) {
            @Override
            public void setEnabled(final boolean b) {
                boolean enabled = b && ThemePreferencesHandler.getSharedInstance().supportsNativeTheme();
                super.setEnabled(enabled);
            }
        };
        themeFollowsSystem.setEnabled(false);
        themeFollowsSystem.setSelected(false);

        enabledSystemPreferences = new TristateCheckBox(resourceBundle.getString("check_system_preferences"));

        enabledSystemPreferences.addChangeListener(e -> {
            if (!enabledSystemPreferences.getTristateModel().isIndeterminate()) {
                boolean selected = enabledSystemPreferences.getTristateModel().isSelected();

                if (themeFollowsSystem.isEnabled())
                    themeFollowsSystem.setSelected(selected);
                if (accentColorFollowsSystem.isEnabled())
                    accentColorFollowsSystem.setSelected(selected);
                if (selectionColorFollowsSystem.isEnabled())
                    selectionColorFollowsSystem.setSelected(selected);
                if (fontSizeFollowsSystem.isEnabled())
                    fontSizeFollowsSystem.setSelected(selected);
            }
            update();
        });
        themeFollowsSystem.addActionListener(e -> update());

        accentColorFollowsSystem.addActionListener(e -> update());
        selectionColorFollowsSystem.addActionListener(e -> update());
        fontSizeFollowsSystem.addActionListener(e -> update());

        enabledSystemPreferences.setSelected(LafManager.isPreferenceChangeReportingEnabled());

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(BorderFactory.createTitledBorder(resourceBundle.getString("title_monitoring")));
        panel.add(getTwoColumnLayout(
                new JComponent[] {enabledSystemPreferences, themeFollowsSystem, accentColorFollowsSystem},
                new JComponent[] {new JLabel(), fontSizeFollowsSystem, selectionColorFollowsSystem},
                GroupLayout.Alignment.LEADING, GroupLayout.Alignment.LEADING));
        return panel;
    }

    private JComponent getTwoColumnLayout(final JComponent[] left, final JComponent[] right) {
        return getTwoColumnLayout(left, right, GroupLayout.Alignment.TRAILING, GroupLayout.Alignment.LEADING);
    }

    private JComponent getTwoColumnLayout(final JComponent[] left, final JComponent[] right,
            final GroupLayout.Alignment leftColumn, final GroupLayout.Alignment rightColumn) {
        if (left.length != right.length) {
            String s = left.length + " labels supplied for " + right.length + " fields!";
            throw new IllegalArgumentException(s);
        }
        JComponent panel = new JPanel();
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(false);

        // Create a sequential group for the horizontal axis.
        GroupLayout.SequentialGroup horizontalGroup = layout.createSequentialGroup();

        GroupLayout.Group verticalLabelGroup = layout.createParallelGroup(leftColumn);
        horizontalGroup.addGroup(verticalLabelGroup);

        GroupLayout.Group verticalComponentGroup = layout.createParallelGroup(rightColumn);
        horizontalGroup.addGroup(verticalComponentGroup);
        layout.setHorizontalGroup(horizontalGroup);

        // Create a sequential group for the vertical axis.
        GroupLayout.SequentialGroup verticalGroup = layout.createSequentialGroup();
        layout.setVerticalGroup(verticalGroup);

        int p = GroupLayout.PREFERRED_SIZE;
        // add the components to the groups
        for (JComponent label : left) {
            verticalLabelGroup.addComponent(label);
        }
        for (JComponent field : right) {
            verticalComponentGroup.addComponent(field, p, p, p);
        }
        for (int i = 0; i < left.length; i++) {
            verticalGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER).addComponent(left[i])
                    .addComponent(right[i], p, p, p));
        }
        return panel;
    }

    private void enableButtonGroup(final ButtonGroup bg, final boolean enabled) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            buttons.nextElement().setEnabled(enabled);
        }
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent, final Color color,
            final String tipText) {
        return addColoredButton(bg, parent, color, color, tipText);
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent, final Color color,
            final AbstractButton peer, final String tipText) {
        return addColoredButton(bg, parent, color, color, peer, tipText);
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent, final Color color,
            final Color focusColor, final String tipText) {
        return addColoredButton(bg, parent, color, focusColor, null, tipText);
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent, final Color color,
            final Color focusColor, final AbstractButton peer, final String tipText) {
        ColoredRadioButton button =
                addColoredButtonImpl(new ColoredRadioButton(null, color, focusColor), bg, parent, tipText);
        if (peer != null) {
            button.addActionListener(e -> {
                if (button.isSelected()) {
                    peer.setSelected(true);
                }
            });
        }
        return button;
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent,
            final Supplier<Color> colorSupplier, final String tipText) {
        return addColoredButton(bg, parent, colorSupplier, colorSupplier, tipText);
    }

    private ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent,
            final Supplier<Color> colorSupplier, final Supplier<Color> focusColorSupplier, final String tipText) {
        return addColoredButtonImpl(new ColoredRadioButton(null, null) {
            {
                addActionListener(e -> getColor());
            }

            @Override
            public Color getColor() {
                Color c = super.getColor();
                if (c == null) {
                    setColors(colorSupplier.get(), focusColorSupplier.get());
                }
                return super.getColor();
            }
        }, bg, parent, tipText);
    }

    private ColoredRadioButton addColoredButtonImpl(final ColoredRadioButton button, final ButtonGroup bg,
            final JComponent parent, final String tipText) {
        setupButton(button, bg, tipText);
        parent.add(button);
        return button;
    }

    private void setupButton(final ColoredRadioButton button, final ButtonGroup bg, final String tipText) {
        bg.add(button);
        button.setName(tipText);
        ToolTipContext context =
                new ToolTipContext().setAlignment(Alignment.CENTER).setCenterAlignment(Alignment.NORTH);
        button.setToolTipText(tipText);
        button.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        button.putClientProperty(ToolTipConstants.KEY_CONTEXT, context);
    }

    private class SettingsPanelConfiguration extends SettingsConfiguration {

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

        @Override
        public AccentColorRule getAccentColorRule() {
            PreferredThemeStyle preferredThemeStyle = LafManager.getPreferredThemeStyle();
            return getAccentColorRule(getTheme(preferredThemeStyle));
        }

        private AccentColorRule getAccentColorRule(final Theme theme) {
            if (theme == null)
                return AccentColorRule.getDefault();
            Color accentColor = getAccentColor(theme, isAccentColorFollowsSystem());
            Color selectionColor = getSelectionColor(theme, isSelectionColorFollowsSystem());
            return AccentColorRule.fromColor(accentColor, selectionColor);
        }

        private Color getAccentColor(final Theme theme, final boolean useThemeColor) {
            return theme.supportsCustomAccentColor()
                    ? useThemeColor ? theme.getAccentColorRule().getAccentColor()
                            : getSelectedColor(bgAccent, defaultAccent)
                    : null;
        }

        private Color getSelectionColor(final Theme theme, final boolean useThemeColor) {
            return theme.supportsCustomSelectionColor()
                    ? useThemeColor ? theme.getAccentColorRule().getSelectionColor()
                            : getSelectedColor(bgSelection, defaultSelection)
                    : null;
        }

        @Override
        public FontSizeRule getFontSizeRule() {
            PreferredThemeStyle preferredThemeStyle = LafManager.getPreferredThemeStyle();
            return getFontSizeRule(getTheme(preferredThemeStyle), preferredThemeStyle);
        }

        private FontSizeRule getFontSizeRule(final Theme theme, final PreferredThemeStyle preferredThemeStyle) {
            if (theme == null)
                return FontSizeRule.getDefault();
            return isFontSizeFollowsSystem() ? preferredThemeStyle.getFontSizeRule()
                    : FontSizeRule.relativeAdjustment(fontSlider.getValue()); // Todo
        }

        @Override
        public Theme getTheme() {
            return getTheme(LafManager.getPreferredThemeStyle());
        }

        private Theme getTheme(final PreferredThemeStyle preferredThemeStyle) {
            return isThemeFollowsSystem() ? LafManager.themeForPreferredStyle(preferredThemeStyle) : getSelectedTheme();
        }

        private Theme getSelectedTheme() {
            return (Theme) themeComboBox.getSelectedItem();
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
            ThemeSettingsPanel.this.accentColorFollowsSystem.setSelected(accentColorFollowsSystem);
        }

        @Override
        public void setFontSizeFollowsSystem(final boolean fontSizeFollowsSystem) {
            ThemeSettingsPanel.this.fontSizeFollowsSystem.setSelected(fontSizeFollowsSystem);
        }

        @Override
        public void setSelectionColorFollowsSystem(final boolean selectionColorFollowsSystem) {
            ThemeSettingsPanel.this.selectionColorFollowsSystem.setSelected(selectionColorFollowsSystem);
        }

        @Override
        public void setThemeFollowsSystem(final boolean themeFollowsSystem) {
            ThemeSettingsPanel.this.themeFollowsSystem.setSelected(themeFollowsSystem);
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
        public void setTheme(final Theme theme) {
            themeComboBox.setSelectedItem(LafManager.getClosestMatchForTheme(theme));
        }
    }
}
