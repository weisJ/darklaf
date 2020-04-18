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
package com.github.weisj.darklaf.settings;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.ColoredRadioButton;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tristate.TristateCheckBox;
import com.github.weisj.darklaf.components.tristate.TristateState;
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

import javax.swing.*;
import javax.swing.plaf.SliderUI;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.*;

public class ThemeSettingsPanel extends JPanel {

    private final ResourceBundle resourceBundle;
    private Icon icon;

    private JCheckBox fontSizeFollowsSystem;
    private JCheckBox accentColorFollowsSystem;
    private JCheckBox selectionColorFollowsSystem;
    private JComboBox<Theme> themeComboBox;
    private JCheckBox themeFollowsSystem;
    private JSlider fontSlider;
    private TristateCheckBox enabledSystemPreferences;

    private ButtonGroup bgSelection;
    private ButtonGroup bgAccent;

    private boolean followFontSize;
    private boolean followAccentColor;
    private boolean followSelectionColor;
    private boolean followTheme;
    private boolean systemPreferences;
    private ColoredRadioButton customAccent;
    private ColoredRadioButton defaultAccent;
    private ColoredRadioButton customSelection;
    private ColoredRadioButton defaultSelection;

    public ThemeSettingsPanel(final ResourceBundle resourceBundle) {
        this.resourceBundle = resourceBundle;
        init();
    }

    protected void init() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        add(createGeneralSettings(), BorderLayout.CENTER);
        add(createMonitorSettings(), BorderLayout.SOUTH);
        update(true);
    }

    public void saveSettings() {
        systemPreferences = !enabledSystemPreferences.getTristateModel().isDeselected();
        followTheme = fontSizeFollowsSystem.isSelected();
        followAccentColor = accentColorFollowsSystem.isSelected();
        followSelectionColor = selectionColorFollowsSystem.isSelected();
        followFontSize = fontSizeFollowsSystem.isSelected();
        LafManager.enabledPreferenceChangeReporting(!enabledSystemPreferences.getTristateModel().isDeselected());
    }

    public void discardChanges() {
        setFontSizeFollowsSystem(followFontSize);
        setThemeFollowsSystem(followTheme);
        setAccentColorFollowsSystem(followAccentColor);
        setSelectionColorFollowsSystem(followSelectionColor);
        setEnabledSystemPreferences(systemPreferences);
    }

    private void update(final boolean skipPreferences) {
        Theme selectedTheme = (Theme) themeComboBox.getSelectedItem();
        if (selectedTheme == null) selectedTheme = LafManager.getTheme();

        boolean enabled = !enabledSystemPreferences.getTristateModel().isDeselected();
        themeFollowsSystem.setEnabled(enabled);
        accentColorFollowsSystem.setEnabled(enabled);
        selectionColorFollowsSystem.setEnabled(enabled);
        fontSizeFollowsSystem.setEnabled(enabled);
        if (enabledSystemPreferences.isSelected()) {
            themeFollowsSystem.setSelected(themeFollowsSystem.isSelected() && themeFollowsSystem.isEnabled());
            accentColorFollowsSystem.setSelected(
                accentColorFollowsSystem.isSelected() && accentColorFollowsSystem.isEnabled());
            selectionColorFollowsSystem.setSelected(
                selectionColorFollowsSystem.isSelected() && selectionColorFollowsSystem.isEnabled());
            fontSizeFollowsSystem.setSelected(fontSizeFollowsSystem.isSelected() && fontSizeFollowsSystem.isEnabled());
        }
        if (!skipPreferences) {
            enabledSystemPreferences.setEnabled(ThemePreferencesHandler.getSharedInstance().canReport());
        }
        themeComboBox.setEnabled(!(isThemeFollowsSystem() && isSystemPreferencesEnabled()));

        enableButtonGroup(bgAccent, selectedTheme.supportsCustomAccentColor()
                                    && !accentColorFollowsSystem.isSelected());
        enableButtonGroup(bgSelection, selectedTheme.supportsCustomSelectionColor()
                                       && !selectionColorFollowsSystem.isSelected());

        fontSlider.setEnabled(!(isFontSizeFollowsSystem() && isSystemPreferencesEnabled()));
    }

    protected void fetch(final PreferredThemeStyle themeStyle, final boolean ignoreSettings) {
        Theme theme = LafManager.themeForPreferredStyle(themeStyle);
        fetch(theme, ignoreSettings);
    }

    protected void fetch(final Theme theme, final boolean ignoreSettings) {
        Theme currentTheme = LafManager.getTheme();
        Theme selectedTheme = (Theme) themeComboBox.getSelectedItem();
        themeComboBox.setModel(LafManager.getThemeComboBoxModel());
        if (ignoreSettings || isThemeFollowsSystem()) {
            setTheme(isThemeFollowsSystem() ? theme : selectedTheme);
        } else {
            themeComboBox.setSelectedItem(selectedTheme);
        }
        if (ignoreSettings || isFontSizeFollowsSystem()) {
            setFontSizeRule(isFontSizeFollowsSystem() ? theme.getFontSizeRule()
                                                      : currentTheme.getFontSizeRule());
        }
        if (ignoreSettings || isAccentColorFollowsSystem()) {
            setAccentColor(getAccentColor(isAccentColorFollowsSystem() ? theme : currentTheme, true));
        }
        if (ignoreSettings || isSelectionColorFollowsSystem()) {
            setSelectionColor(getSelectionColor(isSelectionColorFollowsSystem() ? theme : currentTheme, true));
        }
        if (!ignoreSettings) {
            setEnabledSystemPreferences(LafManager.isPreferenceChangeReportingEnabled());
        }
        update(false);
    }

    public void fetch(final PreferredThemeStyle themeStyle) {
        fetch(themeStyle, false);
    }

    public void fetch() {
        fetch(false);
    }

    public void fetch(final boolean ignoreSettings) {
        fetch(LafManager.getPreferredThemeStyle(), ignoreSettings);
    }

    private boolean updateButtonGroup(final ButtonGroup bg, final Color currentColor,
                                      final ColoredRadioButton defaultButton) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            ColoredRadioButton radioButton = ((ColoredRadioButton) buttons.nextElement());
            boolean selected = (currentColor == null && radioButton == defaultButton)
                               || Objects.equals(radioButton.getColor(), currentColor);
            bg.setSelected(radioButton.getModel(), selected);
            if (selected) return true;
        }
        return false;
    }

    public Theme getEffectiveTheme(final PreferredThemeStyle preferredThemeStyle) {
        Theme theme = getTheme(preferredThemeStyle);
        if (theme == null) return null;
        FontSizeRule fontSizeRule = getFontSizeRule(theme, preferredThemeStyle);
        AccentColorRule accentColorRule = getAccentColorRule(theme, preferredThemeStyle);
        return theme.derive(fontSizeRule, accentColorRule);
    }

    public Theme getEffectiveTheme() {
        return getEffectiveTheme(LafManager.getPreferredThemeStyle());
    }

    protected Color getSelectedColor(final ButtonGroup bg) {
        Enumeration<?> enumeration = bg.getElements();
        ButtonModel model = bg.getSelection();
        while (enumeration.hasMoreElements()) {
            ColoredRadioButton button = (ColoredRadioButton) enumeration.nextElement();
            if (button.getModel() == model) {
                Color c = button.getColor();
                return c != ColoredRadioButton.DEFAULT_FILLED ? c : null;
            }
        }
        return null;
    }

    private Component createGeneralSettings() {
        JLabel themeLabel = new JLabel(resourceBundle.getString("label_theme"));
        themeComboBox = new JComboBox<Theme>(LafManager.getThemeComboBoxModel()) {{
            setSelectedItem(LafManager.getTheme());
        }};
        themeComboBox.putClientProperty(ComboBoxConstants.KEY_DO_NOT_UPDATE_WHEN_SCROLLED, true);
        themeComboBox.addItemListener(e -> update(false));
        themeLabel.setLabelFor(themeComboBox);

        JComponent themeBox = new JPanel(new FlowLayout(FlowLayout.LEFT));
        themeBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        themeBox.add(themeLabel);
        themeBox.add(themeComboBox);
        add(themeBox);

        Color currentAccentColor = LafManager.getTheme().getAccentColorRule().getAccentColor();
        Color currentSelectionColor = LafManager.getTheme().getAccentColorRule().getSelectionColor();

        JComponent accentBox = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLabel accentColorLabel = new JLabel(resourceBundle.getString("label_accent_color"));
        accentColorLabel.setLabelFor(accentBox);

        bgAccent = new ButtonGroup();
        defaultAccent = addColoredButton(bgAccent, accentBox, ColoredRadioButton.DEFAULT_FILLED, currentAccentColor,
                                         resourceBundle.getString("color_default"));
        addColoredButton(bgAccent, accentBox, MacOSColors.BLUE, currentAccentColor,
                         resourceBundle.getString("color_Blue"));
        addColoredButton(bgAccent, accentBox, MacOSColors.LILAC, currentAccentColor,
                         resourceBundle.getString("color_lilac"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ROSE, currentAccentColor,
                         resourceBundle.getString("color_rose"));
        addColoredButton(bgAccent, accentBox, MacOSColors.RED, currentAccentColor,
                         resourceBundle.getString("color_red"));
        addColoredButton(bgAccent, accentBox, MacOSColors.ORANGE, currentAccentColor,
                         resourceBundle.getString("color_orange"));
        addColoredButton(bgAccent, accentBox, MacOSColors.YELLOW, currentAccentColor,
                         resourceBundle.getString("color_yellow"));
        addColoredButton(bgAccent, accentBox, MacOSColors.GREEN, currentAccentColor,
                         resourceBundle.getString("color_green"));
        addColoredButton(bgAccent, accentBox, MacOSColors.GRAY, currentAccentColor,
                         resourceBundle.getString("color_gray"));
        customAccent = addColoredButton(bgAccent, accentBox, null, currentAccentColor,
                                        resourceBundle.getString("color_custom"));
        QuickColorChooser.attachToComponent(customAccent, customAccent::setColor,
                                            () -> Optional.ofNullable(customAccent.getColor())
                                                          .orElse(currentAccentColor),
                                            customAccent::isSelected);

        JComponent selectionBox = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLabel selectionColorLabel = new JLabel(resourceBundle.getString("label_selection_color"));
        selectionColorLabel.setLabelFor(selectionBox);

        bgSelection = new ButtonGroup();
        Color defaultColor = new ThemedColor() {

            private final Properties props = new Properties();
            private final UIDefaults defaults = new UIDefaults();

            @Override
            protected Color getUpdatedColor() {
                LafManager.getTheme().loadDefaults(props, defaults);
                Object obj = props.get("textSelectionBackground");
                props.clear();
                defaults.clear();
                return obj instanceof Color ? (Color) obj : null;
            }
        };
        defaultSelection = addColoredButton(bgSelection, selectionBox, defaultColor, currentSelectionColor,
                                            resourceBundle.getString("color_default"));

        customSelection = addColoredButton(bgSelection, selectionBox, null, currentSelectionColor,
                                           resourceBundle.getString("color_custom"));
        QuickColorChooser.attachToComponent(customSelection, customSelection::setColor,
                                            () -> Optional.ofNullable(customSelection.getColor())
                                                          .orElse(currentSelectionColor),
                                            customSelection::isSelected);
        selectionBox.add(customSelection);

        fontSlider = createFontSlider();
        JLabel fontSizeLabel = new JLabel(resourceBundle.getString("label_font_size"));
        fontSizeLabel.setLabelFor(fontSlider);

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(BorderFactory.createTitledBorder(resourceBundle.getString("title_general")));
        panel.add(getTwoColumnLayout(new JLabel[]{themeLabel,
                                                  accentColorLabel,
                                                  selectionColorLabel,
                                                  fontSizeLabel},
                                     new JComponent[]{themeComboBox,
                                                      accentBox,
                                                      selectionBox,
                                                      fontSlider}));
        return panel;
    }

    public boolean isSystemPreferencesEnabled() {
        return !enabledSystemPreferences.getTristateModel().isDeselected() && enabledSystemPreferences.isEnabled();
    }

    public boolean isAccentColorFollowsSystem() {
        return accentColorFollowsSystem.isSelected() && accentColorFollowsSystem.isEnabled();
    }

    public boolean isFontSizeFollowsSystem() {
        return fontSizeFollowsSystem.isSelected() && fontSizeFollowsSystem.isEnabled();
    }

    public boolean isSelectionColorFollowsSystem() {
        return selectionColorFollowsSystem.isSelected() && selectionColorFollowsSystem.isEnabled();
    }

    public boolean isThemeFollowsSystem() {
        return themeFollowsSystem.isSelected() && themeFollowsSystem.isEnabled();
    }

    public Theme getTheme() {
        return getTheme(LafManager.getPreferredThemeStyle());
    }

    protected Theme getTheme(final PreferredThemeStyle preferredThemeStyle) {
        return isThemeFollowsSystem()
               ? LafManager.themeForPreferredStyle(preferredThemeStyle)
               : (Theme) themeComboBox.getSelectedItem();
    }

    public FontSizeRule getFontSizeRule() {
        PreferredThemeStyle preferredThemeStyle = LafManager.getPreferredThemeStyle();
        return getFontSizeRule(getTheme(preferredThemeStyle), preferredThemeStyle);
    }

    protected FontSizeRule getFontSizeRule(final Theme theme, final PreferredThemeStyle preferredThemeStyle) {
        if (theme == null) return FontSizeRule.getDefault();
        return isFontSizeFollowsSystem()
               ? preferredThemeStyle.getFontSizeRule()
               : FontSizeRule.relativeAdjustment(fontSlider.getValue());
    }

    public AccentColorRule getAccentColorRule() {
        PreferredThemeStyle preferredThemeStyle = LafManager.getPreferredThemeStyle();
        return getAccentColorRule(getTheme(preferredThemeStyle), preferredThemeStyle);
    }

    protected AccentColorRule getAccentColorRule(final Theme theme, final PreferredThemeStyle preferredThemeStyle) {
        if (theme == null) return AccentColorRule.getDefault();
        Color accentColor = getAccentColor(theme, isAccentColorFollowsSystem());
        Color selectionColor = getSelectionColor(theme, isSelectionColorFollowsSystem());
        return AccentColorRule.fromColor(accentColor, selectionColor);
    }

    protected Color getAccentColor(final Theme theme, final boolean useThemeColor) {
        return theme.supportsCustomAccentColor()
               ? useThemeColor ? theme.getAccentColorRule().getAccentColor()
                               : getSelectedColor(bgAccent)
               : null;
    }

    protected Color getSelectionColor(final Theme theme, final boolean useThemeColor) {
        return theme.supportsCustomSelectionColor()
               ? useThemeColor ? theme.getAccentColorRule().getSelectionColor()
                               : getSelectedColor(bgSelection)
               : null;
    }

    public void setEnabledSystemPreferences(final boolean enabled) {
        TristateState state = TristateState.DESELECTED;
        if (enabled
            && (isFontSizeFollowsSystem() || !fontSizeFollowsSystem.isEnabled())
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

    public void setAccentColorFollowsSystem(final boolean accentColorFollowsSystem) {
        this.accentColorFollowsSystem.setSelected(accentColorFollowsSystem);
    }

    public void setFontSizeFollowsSystem(final boolean fontSizeFollowsSystem) {
        this.fontSizeFollowsSystem.setSelected(fontSizeFollowsSystem);
    }

    public void setSelectionColorFollowsSystem(final boolean selectionColorFollowsSystem) {
        this.selectionColorFollowsSystem.setSelected(selectionColorFollowsSystem);
    }

    public void setThemeFollowsSystem(final boolean themeFollowsSystem) {
        this.themeFollowsSystem.setSelected(themeFollowsSystem);
    }

    public void setTheme(final Theme theme) {
        themeComboBox.setSelectedItem(LafManager.getClosestMatchForTheme(theme));
    }

    public void setAccentColorRule(final AccentColorRule accentColorRule) {
        setAccentColorRule(accentColorRule.getAccentColor(), accentColorRule.getSelectionColor());
    }

    protected void setAccentColorRule(final Color accentColor, final Color selectionColor) {
        setAccentColor(accentColor);
        setSelectionColor(selectionColor);
    }

    protected void setAccentColor(final Color accentColor) {
        setXColor(accentColor, bgAccent, customAccent, defaultAccent);
    }

    protected void setSelectionColor(final Color selectionColor) {
        setXColor(selectionColor, bgSelection, customSelection, defaultSelection);
    }

    protected void setXColor(final Color color, final ButtonGroup bg,
                             final ColoredRadioButton customButton,
                             final ColoredRadioButton defaultButton) {
        if (color == null) {
            defaultButton.setSelected(true);
            return;
        }
        if (!updateButtonGroup(bg, color, defaultButton)) {
            customButton.setSelected(true);
            if (customButton.getColor() == null) customButton.setColor(color);
        }
    }

    public void setFontSizeRule(final FontSizeRule fontSizeRule) {
        if (fontSizeRule == null) {
            fontSlider.setValue(FontSizePreset.NORMAL.getPercentage());
        } else {
            fontSlider.setValue(fontSizeRule.getPercentage());
        }
    }

    private JSlider createFontSlider() {
        JSlider fontSlider = new JSlider() {
            @Override
            public String getToolTipText(final MouseEvent event) {
                return getValue() + "%";
            }
        };
        ToolTipContext context = new ToolTipContext()
            .setAlignment(Alignment.CENTER)
            .setCenterAlignment(Alignment.NORTH)
            .setUseBestFit(true)
            .setToolTipRectSupplier(e -> {
                SliderUI ui = fontSlider.getUI();
                if (ui instanceof DarkSliderUI) {
                    Rectangle r = ((DarkSliderUI) ui).getThumbRect();
                    r.x -= 1;
                    return r;
                }
                return new Rectangle(0, 0, fontSlider.getWidth(),
                                     fontSlider.getHeight());
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
        min.setText(resourceBundle.getString("label_font_smaller"));
        min.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        min.putClientProperty(DarkSliderUI.KEY_MANUAL_LABEL_ALIGN, true);

        JLabel mid = ((JLabel) dict.get(fontSlider.getMinimum() + tickSpacing));
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
                boolean enabled = b && ThemePreferencesHandler.getSharedInstance().supportsNativeFontSize();
                super.setEnabled(enabled);
            }
        };
        themeFollowsSystem.setEnabled(false);
        themeFollowsSystem.setSelected(false);

        enabledSystemPreferences = new TristateCheckBox(resourceBundle.getString("check_system_preferences"));

        ActionListener actionListener = e -> SwingUtilities.invokeLater(() -> update(false));
        enabledSystemPreferences.addChangeListener(e -> {
            if (!enabledSystemPreferences.getTristateModel().isIndeterminate()) {
                boolean selected = enabledSystemPreferences.getTristateModel().isSelected();
                if (themeFollowsSystem.isEnabled()) themeFollowsSystem.setSelected(selected);
                if (accentColorFollowsSystem.isEnabled()) accentColorFollowsSystem.setSelected(selected);
                if (selectionColorFollowsSystem.isEnabled()) selectionColorFollowsSystem.setSelected(selected);
                if (fontSizeFollowsSystem.isEnabled()) fontSizeFollowsSystem.setSelected(selected);
            }
            update(true);
        });
        themeFollowsSystem.addActionListener(actionListener);
        accentColorFollowsSystem.addActionListener(actionListener);
        selectionColorFollowsSystem.addActionListener(actionListener);
        fontSizeFollowsSystem.addActionListener(actionListener);

        enabledSystemPreferences.setSelected(LafManager.isPreferenceChangeReportingEnabled());

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(BorderFactory.createTitledBorder(resourceBundle.getString("title_monitoring")));
        panel.add(getTwoColumnLayout(new JComponent[]{enabledSystemPreferences,
                                                      themeFollowsSystem,
                                                      accentColorFollowsSystem},
                                     new JComponent[]{new JLabel(),
                                                      fontSizeFollowsSystem,
                                                      selectionColorFollowsSystem},
                                     GroupLayout.Alignment.LEADING, GroupLayout.Alignment.LEADING));
        return panel;
    }

    private JComponent getTwoColumnLayout(final JComponent[] left, final JComponent[] right) {
        return getTwoColumnLayout(left, right, GroupLayout.Alignment.TRAILING, GroupLayout.Alignment.LEADING);
    }

    private JComponent getTwoColumnLayout(final JComponent[] left, final JComponent[] right,
                                          final GroupLayout.Alignment leftColumn,
                                          final GroupLayout.Alignment rightColumn) {
        if (left.length != right.length) {
            String s = left.length + " labels supplied for "
                       + right.length + " fields!";
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
            verticalGroup.addGroup(layout.createParallelGroup(
                GroupLayout.Alignment.CENTER).addComponent(left[i]).addComponent(right[i], p, p, p));
        }
        return panel;
    }

    private void enableButtonGroup(final ButtonGroup bg, final boolean enabled) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            buttons.nextElement().setEnabled(enabled);
        }
    }

    private AbstractButton getSelectedButton(final ButtonGroup bg) {
        Enumeration<AbstractButton> enumeration = bg.getElements();
        ButtonModel selected = bg.getSelection();
        while (enumeration.hasMoreElements()) {
            AbstractButton button = enumeration.nextElement();
            if (selected == button.getModel()) return button;
        }
        return null;
    }

    public ColoredRadioButton addColoredButton(final ButtonGroup bg, final JComponent parent, final Color color,
                                               final Color selectedColor, final String tipText) {
        ColoredRadioButton button = new ColoredRadioButton(null, color);
        setupButton(button, bg, selectedColor, tipText);
        parent.add(button);
        return button;
    }

    private void setupButton(final ColoredRadioButton button, final ButtonGroup bg, final Color selectedColor,
                             final String tipText) {
        bg.add(button);
        button.setName(tipText);
        ToolTipContext context = new ToolTipContext().setAlignment(Alignment.CENTER)
                                                     .setCenterAlignment(Alignment.NORTH);
        button.setToolTipText(tipText);
        button.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        button.putClientProperty(ToolTipConstants.KEY_CONTEXT, context);
    }

    public String getTitle() {
        return resourceBundle.getString("title");
    }

    public Icon getIcon() {
        return icon;
    }

    @Override
    public void updateUI() {
        super.updateUI();
        icon = UIManager.getIcon("ThemeSettings.icon");
    }
}
