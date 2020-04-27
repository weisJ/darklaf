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
package com.github.weisj.darklaf.settings;

import java.awt.*;
import java.awt.event.WindowEvent;
import java.util.ResourceBundle;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.DefaultButton;
import com.github.weisj.darklaf.graphics.ImageUtil;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.event.ThemePreferenceChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemePreferenceListener;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.theme.info.FontSizeRule;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LazyValue;

public class ThemeSettings implements ThemePreferenceListener {

    private static final LazyValue<ThemeSettings> instance = new LazyValue<>(ThemeSettings::new);
    private final JPanel contentPane;
    private final ThemeSettingsPanel settingsPanel;
    private final ResourceBundle resourceBundle = ResourceBundle.getBundle("theme_settings");
    private JDialog dialog;

    public static ThemeSettings getInstance() {
        return instance.get();
    }

    public static boolean isInitialized() {
        return instance.isInitialized();
    }

    /**
     * Show the settings as a dialog.
     *
     * @param  parent                the parent component.
     * @throws IllegalStateException If {@link #getSettingsPanel()} is already in use.
     */
    public static void showSettingsDialog(final Component parent) {
        showSettingsDialog(parent, Dialog.ModalityType.MODELESS);
    }

    /**
     * Show the settings as a dialog.
     *
     * @param  parent                the parent component.
     * @param  modalityType          the modality type.
     * @throws IllegalStateException If {@link #getSettingsPanel()} is already in use.
     */
    public static void showSettingsDialog(final Component parent, final Dialog.ModalityType modalityType) {
        getInstance().showDialog(parent, modalityType);
    }

    protected ThemeSettings() {
        settingsPanel = new ThemeSettingsPanel(resourceBundle);
        contentPane = new JPanel(new BorderLayout());
        contentPane.add(settingsPanel, BorderLayout.CENTER);
        contentPane.add(createButtonPanel(), BorderLayout.SOUTH);
        settingsPanel.fetch(true);
        LafManager.addThemePreferenceChangeListener(this);
    }

    protected JDialog createDialog(final Window parent) {
        JDialog dialog = new JDialog(parent);
        dialog.setIconImage(ImageUtil.createFrameIcon(settingsPanel.getIcon()));
        dialog.setTitle(settingsPanel.getTitle());
        dialog.setContentPane(contentPane);
        dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        dialog.pack();
        dialog.setLocationByPlatform(true);
        dialog.setLocationRelativeTo(parent);
        return dialog;
    }

    /**
     * Show the settings as a dialog.
     *
     * @param  parent                the parent component.
     * @param  modal                 the modality type.
     * @throws IllegalStateException If {@link #getSettingsPanel()} is already in use.
     */
    public void showDialog(final Component parent, final Dialog.ModalityType modal) {
        Window parentWindow = DarkUIUtil.getWindow(getSettingsPanel());
        if (parentWindow != null && parentWindow != DarkUIUtil.getWindow(dialog)) {
            throw new IllegalStateException("Can't show dialog while settings panel is used elsewhere");
        }
        refresh();
        Window window = DarkUIUtil.getWindow(parent);
        dialog = createDialog(window);
        dialog.setModalityType(modal);
        dialog.setVisible(true);
    }

    /**
     * Returns the settings panel for custom use. Note that this is a shared resource and while it is used {@link
     * #showDialog(Component, java.awt.Dialog.ModalityType)} can't be called. Note that all communication to the
     * settings panel should happen through this class by calling {@link #peek()}, {@link #save()},{@link
     * #refresh()},{@link #revert()} or {@link #apply()}
     *
     * @return the settings panel.
     */
    public JComponent getSettingsPanel() {
        return settingsPanel;
    }

    /**
     * Returns whether the option to follow the system settings is enabled.
     *
     * @return true if enabled.
     * @see    #isSystemPreferencesEnabled() (boolean)
     * @see    #isThemeFollowsSystem()
     * @see    #isAccentColorFollowsSystem()
     * @see    #isSelectionColorFollowsSystem()
     * @see    #isFontSizeFollowsSystem()
     */
    public boolean isSystemPreferencesEnabled() {
        return settingsPanel.isSystemPreferencesEnabled();
    }

    /**
     * Returns whether the accent color follows the system settings.
     *
     * @return true if accent color follows system settings.
     * @see    #setAccentColorFollowsSystem(boolean)
     */
    public boolean isAccentColorFollowsSystem() {
        return settingsPanel.isAccentColorFollowsSystem();
    }

    /**
     * Returns whether the font size follows the system settings.
     *
     * @return true if font size follows system settings.
     * @see    #setFontSizeFollowsSystem(boolean)
     */
    public boolean isFontSizeFollowsSystem() {
        return settingsPanel.isFontSizeFollowsSystem();
    }

    /**
     * Returns whether the selection color follows the system settings.
     *
     * @return true if selection color follows system settings.
     * @see    #setSelectionColorFollowsSystem(boolean)
     */
    public boolean isSelectionColorFollowsSystem() {
        return settingsPanel.isSelectionColorFollowsSystem();
    }

    /**
     * Returns whether the theme follows the system settings.
     *
     * @return true if theme follows system settings.
     * @see    #setThemeFollowsSystem(boolean)
     */
    public boolean isThemeFollowsSystem() {
        return settingsPanel.isThemeFollowsSystem();
    }

    /**
     * Get the currently selected accent color rule. This is not the same as the rule of {@link LafManager#getTheme()}
     * as the current settings might not have been applied.
     *
     * @return the current selected accent color rule.
     * @see    #setAccentColorRule(AccentColorRule)
     */
    public AccentColorRule getAccentColorRule() {
        return settingsPanel.getAccentColorRule();
    }

    /**
     * Get the currently selected font size rule. This is not the same as the rule of {@link LafManager#getTheme()} as
     * the current settings might not have been applied.
     *
     * @return the current selected font size rule.
     * @see    #setFontSizeRule(FontSizeRule)
     */
    public FontSizeRule getFontSizeRule() {
        return settingsPanel.getFontSizeRule();
    }

    /**
     * Get the currently selected theme. This is not the same as {@link LafManager#getTheme()} as the current settings
     * might not have been applied.
     *
     * @return the current selected theme.
     * @see    #setTheme(Theme)
     */
    public Theme getTheme() {
        return settingsPanel.getTheme();
    }

    /**
     * Enables the option to follow system preferences.
     *
     * @param enabled true if enabled.
     * @see           #setAccentColorFollowsSystem(boolean)
     * @see           #setSelectionColorFollowsSystem(boolean)
     * @see           #setFontSizeFollowsSystem(boolean)
     * @see           #setThemeFollowsSystem(boolean)
     */
    public void setEnabledSystemPreferences(final boolean enabled) {
        settingsPanel.setEnabledSystemPreferences(enabled);
    }

    /**
     * Sets whether the accent color should follow the system settings. This only works if {@link
     * #isSelectionColorFollowsSystem()} is true.
     *
     * @param accentColorFollowsSystem true if accent color should follow system.
     * @see                            #isAccentColorFollowsSystem()
     */
    public void setAccentColorFollowsSystem(final boolean accentColorFollowsSystem) {
        settingsPanel.setAccentColorFollowsSystem(accentColorFollowsSystem);
    }

    /**
     * Sets whether the font size should follow the system settings. This only works if {@link
     * #isSelectionColorFollowsSystem()} is true.
     *
     * @param fontSizeFollowsSystem true if font size should follow system.
     * @see                         #isFontSizeFollowsSystem()
     */
    public void setFontSizeFollowsSystem(final boolean fontSizeFollowsSystem) {
        settingsPanel.setFontSizeFollowsSystem(fontSizeFollowsSystem);
    }

    /**
     * Sets whether the selection color should follow the system settings. This only works if {@link
     * #isSelectionColorFollowsSystem()} is true.
     *
     * @param selectionColorFollowsSystem true if selection color should follow system.
     * @see                               #isSelectionColorFollowsSystem()
     */
    public void setSelectionColorFollowsSystem(final boolean selectionColorFollowsSystem) {
        settingsPanel.setSelectionColorFollowsSystem(selectionColorFollowsSystem);
    }

    /**
     * Sets whether the theme should follow the system settings. This only works if {@link
     * #isSelectionColorFollowsSystem()} is true.
     *
     * @param themeFollowsSystem true if theme should follow system.
     * @see                      #isThemeFollowsSystem()
     */
    public void setThemeFollowsSystem(final boolean themeFollowsSystem) {
        settingsPanel.setThemeFollowsSystem(themeFollowsSystem);
    }

    /**
     * Sets the font accent color rule. The theme is not updated until {@link #apply()} is called.
     *
     * @param accentColorRule the accent color rule
     * @see                   #getAccentColorRule()
     */
    public void setAccentColorRule(final AccentColorRule accentColorRule) {
        settingsPanel.setAccentColorRule(accentColorRule);
    }

    /**
     * Sets the font size rule. The theme is not updated until {@link #apply()} is called.
     *
     * @param fontSizeRule the font size rule.
     * @see                #getFontSizeRule()
     */
    public void setFontSizeRule(final FontSizeRule fontSizeRule) {
        settingsPanel.setFontSizeRule(fontSizeRule);
    }

    /**
     * Sets the theme. The theme is not updated until {@link #apply()} is called.
     *
     * @param theme the theme.
     * @see         #getTheme()
     */
    public void setTheme(final Theme theme) {
        settingsPanel.setTheme(theme);
    }

    protected Component createButtonPanel() {
        JButton ok = new DefaultButton(resourceBundle.getString("dialog_ok"));
        ok.setDefaultCapable(true);
        ok.addActionListener(e -> {
            apply();
            dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
        });

        JButton cancel = new JButton(resourceBundle.getString("dialog_cancel"));
        cancel.addActionListener(e -> {
            revert();
            dialog.dispatchEvent(new WindowEvent(dialog, WindowEvent.WINDOW_CLOSING));
        });
        JButton apply = new JButton(resourceBundle.getString("dialog_apply"));
        apply.addActionListener(e -> {
            apply();
        });

        Box box = Box.createHorizontalBox();
        box.add(Box.createHorizontalGlue());
        box.add(ok);
        box.add(cancel);
        box.add(apply);
        box.setBorder(settingsPanel.getBorder());
        return box;
    }

    /**
     * Updates all values according to the current settings.
     */
    public void refresh() {
        settingsPanel.fetch(true);
    }

    /**
     * Saves the settings.
     */
    public void save() {
        settingsPanel.saveSettings();
    }

    /**
     * Saves the settings and updates the laf. This is them same as calling {@link #save()} and {@link #peek()}.
     */
    public void apply() {
        save();
        peek();
    }

    /**
     * Sets the theme according to the selected options. Does not save the settings.
     */
    public void peek() {
        applyTheme(settingsPanel.getEffectiveTheme());
    }

    /**
     * Revert the settings to the last time they have been saved.
     *
     * @see #save()
     */
    public void revert() {
        settingsPanel.discardChanges();
        refresh();
    }

    protected void applyTheme(final Theme theme) {
        if (theme == null) return;
        if (LafManager.getTheme().appearsEqualTo(theme)) return;
        SwingUtilities.invokeLater(() -> {
            LafManager.installTheme(theme);
            settingsPanel.fetch(true);
        });
    }

    /**
     * Returns the recommended display icon.
     *
     * @return the icon
     */
    public Icon getIcon() {
        return settingsPanel.getIcon();
    }

    /**
     * Returns the recommended display title.
     *
     * @return the title
     */
    public String getTitle() {
        return settingsPanel.getTitle();
    }

    @Override
    public void themePreferenceChanged(final ThemePreferenceChangeEvent e) {
        settingsPanel.fetch(e.getPreferredThemeStyle());
        applyTheme(settingsPanel.getEffectiveTheme(e.getPreferredThemeStyle()));
    }
}
