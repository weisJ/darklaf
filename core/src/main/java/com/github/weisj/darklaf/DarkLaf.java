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
package com.github.weisj.darklaf;

import java.awt.*;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.plaf.metal.MetalLookAndFeel;

import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.task.*;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.laf.ThemedLookAndFeel;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.ui.popupmenu.MouseGrabberUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.SystemInfo;

/** @author Jannis Weis */
public class DarkLaf extends ThemedLookAndFeel {

    public static final String SYSTEM_PROPERTY_PREFIX = "darklaf.";
    public static final String ALLOW_NATIVE_CODE_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "allowNativeCode";
    private static final Logger LOGGER = LogUtil.getLogger(DarkLaf.class);
    /*
     * All tasks for initializing the ui defaults in order of execution.
     */
    private static final DefaultsInitTask[] INIT_TASKS =
            new DefaultsInitTask[] {new RemoveUnusedInitTask(), new ThemeDefaultsInitTask(),
                    new InputDefaultsInitTask(), new IdeaDefaultsInitTask(), new FontDefaultsInitTask(),
                    new StyleSheetInitTask(), new ResourceBundleInitTask(), new SystemDefaultsInitTask(),
                    new PlatformDefaultsInitTask(), new UserInitTask(), new UtilityDefaultsInitTask()};
    /*
     * The base look and feel. This may vary to handle different platform support.
     */
    private final LookAndFeel base;
    /*
     * Indicated whether #initialize has been called. This prevents the theme to change for
     * Borders/Icons etc. if #getDefaults is called outside of LaF installation.
     */
    private boolean isInitialized;

    /*
     * The current theme.
     */
    private Theme theme;

    /** Create Custom Darcula LaF. */
    public DarkLaf() {
        base = getBase();
    }

    private LookAndFeel getBase() {
        LookAndFeel baseLaf;
        if (SystemInfo.isWindows || SystemInfo.isLinux) {
            baseLaf = new MetalLookAndFeel();
        } else {
            final String systemLafClassName = UIManager.getSystemLookAndFeelClassName();
            final LookAndFeel currentLaf = UIManager.getLookAndFeel();
            if (currentLaf != null && systemLafClassName.equals(currentLaf.getClass().getName())) {
                baseLaf = currentOrFallback(currentLaf);
            } else {
                try {
                    UIManager.setLookAndFeel(systemLafClassName);
                    baseLaf = currentOrFallback(UIManager.getLookAndFeel());
                } catch (final Exception e) {
                    LOGGER.log(Level.SEVERE, e.getMessage(), e.getStackTrace());
                    throw new IllegalStateException("Could not load base LaF class." + e.getMessage());
                }
            }
        }
        return baseLaf;
    }

    private LookAndFeel currentOrFallback(final LookAndFeel currentLaf) {
        if (currentLaf != null) {
            return currentLaf;
        } else {
            return new MetalLookAndFeel();
        }
    }

    @Override
    public void initialize() {
        base.initialize();
        PopupFactory.setSharedInstance(new DarkPopupFactory());
        setupDecorations();
        isInitialized = true;
        SwingUtilities.invokeLater(LafManager::notifyThemeInstalled);
    }

    private void setupDecorations() {
        DecorationsHandler.getSharedInstance().initialize();
    }

    @Override
    public void uninitialize() {
        base.uninitialize();
        MouseGrabberUtil.uninstallMouseGrabber();
        isInitialized = false;
        PopupFactory.setSharedInstance(new PopupFactory());
    }

    @Override
    public UIDefaults getDefaults() {
        final UIDefaults defaults = base.getDefaults();
        final Theme currentTheme = getTheme();
        if (isInitialized && !LafManager.getTheme().equals(currentTheme)) {
            LafManager.setTheme(currentTheme);
        }
        for (DefaultsInitTask task : INIT_TASKS) {
            if (task.onlyDuringInstallation() && !isInitialized) continue;
            task.run(currentTheme, defaults);
        }
        return defaults;
    }

    @Override
    public LayoutStyle getLayoutStyle() {
        return base.getLayoutStyle();
    }

    @Override
    public void provideErrorFeedback(final Component component) {
        base.provideErrorFeedback(component);
    }

    @Override
    public Icon getDisabledIcon(final JComponent component, final Icon icon) {
        return base.getDisabledIcon(component, icon);
    }

    @Override
    public Icon getDisabledSelectedIcon(final JComponent component, final Icon icon) {
        return base.getDisabledSelectedIcon(component, icon);
    }

    @Override
    protected void setTheme(final Theme theme) {
        this.theme = theme;
    }

    @Override
    public Theme getTheme() {
        return theme != null ? theme : LafManager.getTheme();
    }

    @Override
    public String getName() {
        return "Darklaf";
    }

    @Override
    public String getID() {
        return getName();
    }

    @Override
    public String getDescription() {
        return "A themeable Look and Feel";
    }

    @Override
    public boolean isNativeLookAndFeel() {
        return true;
    }

    @Override
    public boolean isSupportedLookAndFeel() {
        return true;
    }

    @Override
    public boolean getSupportsWindowDecorations() {
        /*
         * Return false to avoid setting the windows to undecorated. This results in rootpane-styles not
         * being set. However darklaf only distinguished between Frames and Dialogs.
         */
        return false;
    }
}
