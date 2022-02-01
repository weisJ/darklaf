/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.ui.demo;

import java.util.Arrays;
import java.util.Enumeration;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.settings.ThemeSettingsMenuItem;
import com.github.weisj.darklaf.theme.event.ThemeInstalledListener;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.util.Lambdas;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DemoMenuBar extends JMenuBar {

    public DemoMenuBar() {
        add(createThemeMenu());
        add(createSettingsMenu());
        add(createDevSettings());
    }

    private JMenu createThemeMenu() {
        JMenu menu = new JMenu("Theme");
        menu.setMnemonic('T');
        ButtonGroup bg = new ButtonGroup();
        for (UIManager.LookAndFeelInfo theme : LafManager.getRegisteredThemeInfos()) {
            createThemeItem(menu, bg, theme);
        }
        Runnable updater = () -> bg.setSelected(
                Optional.ofNullable(getSelectedThemeButton(bg)).map(AbstractButton::getModel).orElse(null), true);
        menu.addMenuListener(new MenuListener() {
            @Override
            public void menuSelected(final MenuEvent e) {
                updater.run();
            }

            @Override
            public void menuDeselected(final MenuEvent e) {}

            @Override
            public void menuCanceled(final MenuEvent e) {}
        });
        LafManager.addThemeChangeListener((ThemeInstalledListener) e -> updater.run());
        return menu;
    }

    private AbstractButton getSelectedThemeButton(final ButtonGroup bg) {
        String currentThemeName = LafManager.getInstalledTheme().getName();
        Enumeration<AbstractButton> enumeration = bg.getElements();
        while (enumeration.hasMoreElements()) {
            JMenuItem mi = (JMenuItem) enumeration.nextElement();
            if (Objects.equals(currentThemeName, mi.getText())) {
                return mi;
            }
        }
        return null;
    }

    private void createThemeItem(final JMenu menu, final ButtonGroup bg, final UIManager.LookAndFeelInfo info) {
        JMenuItem mi = new JRadioButtonMenuItem(info.getName());
        mi.addActionListener(event -> {
            try {
                UIManager.setLookAndFeel(info.getClassName());
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
                    | UnsupportedLookAndFeelException e) {
                e.printStackTrace();
            }
            LafManager.updateLaf();
        });
        if (LafManager.getTheme().getName().equals(mi.getText())) {
            mi.setSelected(true);
        }
        menu.add(mi);
        bg.add(mi);
    }

    private JMenuItem createSettingsMenu() {
        JMenu menu = new JMenu("Settings");
        menu.add(new ThemeSettingsMenuItem("Theme Options"));
        return menu;
    }

    private JMenu createDevSettings() {
        JMenu dev = new JMenu("Dev");
        dev.add(createLoggingMenu());
        dev.add(createPaintingMenu());
        dev.add(new JCheckBoxMenuItem("Custom Decorations") {
            {
                setSelected(LafManager.isDecorationsEnabled());
                addActionListener(e -> LafManager.setDecorationsEnabled(isSelected()));
            }
        });
        if (SystemInfo.isWindows) {
            dev.add(new JCheckBoxMenuItem("Unified Menubar") {
                {
                    SwingUtilities.invokeLater(() -> setSelected(PropertyUtil.getBooleanProperty(
                            SwingUtilities.getRootPane(dev), DarkRootPaneUI.KEY_UNIFIED_MENUBAR)));
                    addActionListener(e -> SwingUtilities.getRootPane(dev)
                            .putClientProperty(DarkRootPaneUI.KEY_UNIFIED_MENUBAR, isSelected()));
                }
            });
        }
        if (SystemInfo.isMac) {
            dev.add(new JCheckBoxMenuItem("Colored Titlebar") {
                {
                    SwingUtilities.invokeLater(() -> setSelected(PropertyUtil.getBooleanProperty(
                            SwingUtilities.getRootPane(dev), DarkRootPaneUI.KEY_COLORED_TITLE_BAR)));
                    addActionListener(e -> SwingUtilities.getRootPane(dev)
                            .putClientProperty(DarkRootPaneUI.KEY_COLORED_TITLE_BAR, isSelected()));
                }
            });
        }
        dev.add(createOSModeMenu());
        dev.add(createLafMenu());
        return dev;
    }

    @NotNull
    private JMenu createOSModeMenu() {
        JMenu osMode = new JMenu("OS Mode");
        ButtonGroup osModeBg = new ButtonGroup();
        JMenuItem host = new JRadioButtonMenuItem("host");
        host.setSelected(true);
        host.addActionListener(e -> {
            System.clearProperty("darklaf.internal.osname");
            LafManager.install();
        });
        osModeBg.add(host);
        osMode.add(host);
        Arrays.asList("windows", "mac", "linux").forEach(s -> {
            JMenuItem item = new JRadioButtonMenuItem(s);
            item.addActionListener(e -> {
                System.setProperty("darklaf.internal.osname", s);
                LafManager.install();
            });
            osModeBg.add(item);
            osMode.add(item);
        });
        return osMode;
    }

    @NotNull
    private JMenu createLafMenu() {
        JMenu lookAndFeels = new JMenu("Laf");
        ButtonGroup lafBg = new ButtonGroup();
        JMenuItem darklafLaf = new JRadioButtonMenuItem("Darklaf");
        lafBg.add(darklafLaf);
        lookAndFeels.add(darklafLaf);
        darklafLaf.setSelected(true);
        darklafLaf.addActionListener(e -> LafManager.install());
        for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
            if (info.getName().startsWith("com.github.weisj.darklaf")) {
                continue;
            }
            JMenuItem item = new JRadioButtonMenuItem(info.getName());
            lafBg.add(item);
            lookAndFeels.add(item);
            if (UIManager.getLookAndFeel().getClass().getName().equals(info.getClassName())) {
                item.setSelected(true);
            }
            Runnable lafSetter = Lambdas.wrap(() -> {
                UIManager.setLookAndFeel(info.getClassName());
                LafManager.updateLaf();
            });
            item.addActionListener(e -> lafSetter.run());
        }
        return lookAndFeels;
    }

    @NotNull
    private JMenu createPaintingMenu() {
        JCheckBoxMenuItem aaPainting = new JCheckBoxMenuItem("Translucent Antialiasing");
        JCheckBoxMenuItem experimentalAA = new JCheckBoxMenuItem("Experimental Antialiasing");
        aaPainting.addActionListener(e -> {
            StringPainter.setTranslucentAAPaintingEnabled(aaPainting.isSelected());
            experimentalAA.setEnabled(aaPainting.isSelected());
        });
        aaPainting.setSelected(StringPainter.isTranslucentAAPaintingEnabled());
        experimentalAA
                .addActionListener(e -> StringPainter.setExperimentalAntialiasingEnabled(experimentalAA.isSelected()));
        experimentalAA.setSelected(StringPainter.isExperimentalAntialiasingEnabled());

        JMenu painting = new JMenu("Painting");
        painting.add(aaPainting);
        painting.add(experimentalAA);
        return painting;
    }

    @NotNull
    private JMenu createLoggingMenu() {
        JMenu logging = new JMenu("Logging");
        ButtonGroup bg = new ButtonGroup();
        Level[] levels = new Level[] {
                Level.ALL, Level.FINEST, Level.FINER, Level.FINE, Level.INFO,
                Level.WARNING, Level.SEVERE, Level.OFF};
        Level currentLevel = LafManager.getLogLevel();
        for (Level level : levels) {
            JRadioButtonMenuItem mi = new JRadioButtonMenuItem(level.getName());
            mi.addActionListener(e -> LafManager.setLogLevel(level));
            bg.add(mi);
            logging.add(mi);
            if (level.equals(currentLevel)) {
                mi.setSelected(true);
            }
        }
        return logging;
    }
}
