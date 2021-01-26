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
package ui;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowListener;
import java.util.Enumeration;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;

import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.settings.ThemeSettingsMenuItem;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.event.ThemeInstalledListener;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.util.PropertyUtil;

public interface ComponentDemo {

    static Theme getTheme() {
        PreferredThemeStyle themeStyle = LafManager.getPreferredThemeStyle();
        return LafManager.themeForPreferredStyle(
                new PreferredThemeStyle(themeStyle.getContrastRule(), themeStyle.getColorToneRule()));
    }

    default Theme createTheme() {
        return getTheme();
    }

    JComponent createComponent();

    static void showDemo(final ComponentDemo demo) {
        showDemo(demo, false);
    }

    static void showDemo(final ComponentDemo demo, final boolean asDialog) {
        LafManager.enabledPreferenceChangeReporting(false);
        LafManager.addThemePreferenceChangeListener(LafManager::installTheme);
        LafManager.setDecorationsEnabled(true);
        LafManager.setLogLevel(Level.FINE);
        SwingUtilities.invokeLater(() -> {
            if (demo.useDarkLaf()) {
                if (!LafManager.isInstalled()) {
                    LafManager.install(demo.createTheme());
                }
            } else {
                installSystemLaf();
            }
            Window window;
            if (!asDialog) {
                JFrame frame = new JFrame();
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.addWindowListener(demo.createWindowListener());
                frame.setTitle(demo.getTitle());
                frame.setContentPane(demo.createComponent());
                frame.setJMenuBar(demo.createMenuBar());
                window = frame;
            } else {
                JDialog dialog = new JDialog();
                dialog.setModalityType(Dialog.ModalityType.MODELESS);
                dialog.setTitle(demo.getTitle());
                dialog.setContentPane(demo.createComponent());
                dialog.setJMenuBar(demo.createMenuBar());
                window = dialog;
            }

            Icon icon = demo.getFrameIcon();
            if (icon != null) {
                window.setIconImage(IconLoader.createFrameIcon(icon, window));
            }

            window.pack();
            Dimension dimension = demo.getDisplayDimension();
            if (dimension == null) {
                Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
                Dimension dim = new Dimension(screenSize.width / 2, screenSize.height / 2);
                Dimension targetSize = window.getSize();
                targetSize.width = Math.min(targetSize.width, dim.width);
                targetSize.height = Math.min(targetSize.height, dim.height);
                window.setSize(targetSize);
            } else {
                window.setSize(dimension);
            }

            window.setVisible(true);
            window.setLocationRelativeTo(null);
        });
    }

    default boolean useDarkLaf() {
        return true;
    }

    default Dimension getDisplayDimension() {
        return null;
    }

    default Icon getFrameIcon() {
        return null;
    }

    default WindowListener createWindowListener() {
        return new WindowAdapter() {};
    }

    static JMenu createThemeMenu() {
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

    static AbstractButton getSelectedThemeButton(final ButtonGroup bg) {
        String currentThemeName = LafManager.getInstalledTheme().getName();
        Enumeration<AbstractButton> enumeration = bg.getElements();
        while (enumeration.hasMoreElements()) {
            JMenuItem mi = (JMenuItem) enumeration.nextElement();
            if (Objects.equals(currentThemeName, mi.getText())) return mi;
        }
        return null;
    }

    static JMenuItem createSettingsMenu() {
        JMenu menu = new JMenu("Settings");
        menu.add(new ThemeSettingsMenuItem("Theme Options"));
        return menu;
    }

    static void createThemeItem(final JMenu menu, final ButtonGroup bg, final UIManager.LookAndFeelInfo info) {
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

    default JMenuBar createMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(createThemeMenu());
        menuBar.add(createSettingsMenu());
        menuBar.add(createDevSettings());
        return menuBar;
    }

    default JMenu createDevSettings() {
        JMenu dev = new JMenu("Dev");
        JMenu logging = new JMenu("Logging");
        ButtonGroup bg = new ButtonGroup();
        Level[] levels = new Level[] {Level.ALL, Level.FINEST, Level.FINER, Level.FINE, Level.INFO, Level.WARNING,
                Level.SEVERE, Level.OFF};
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

        dev.add(logging);
        dev.add(aaPainting);
        dev.add(experimentalAA);
        dev.add(new JCheckBoxMenuItem("Custom Decorations") {
            {
                setSelected(LafManager.isDecorationsEnabled());
                addActionListener(e -> LafManager.setDecorationsEnabled(isSelected()));
            }
        });
        dev.add(new JCheckBoxMenuItem("Unified Menubar") {
            {
                SwingUtilities.invokeLater(() -> setSelected(PropertyUtil
                        .getBooleanProperty(SwingUtilities.getRootPane(dev), DarkRootPaneUI.KEY_UNIFIED_MENUBAR)));
                addActionListener(e -> SwingUtilities.getRootPane(dev)
                        .putClientProperty(DarkRootPaneUI.KEY_UNIFIED_MENUBAR, isSelected()));
            }
        });
        dev.add(new JCheckBoxMenuItem("Darklaf/System Laf") {
            {
                setSelected(LafManager.isInstalled());
                addActionListener(e -> {
                    if (isSelected()) {
                        LafManager.install();
                    } else {
                        installSystemLaf();
                    }
                });
            }
        });
        return dev;
    }

    static void installSystemLaf() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            LafManager.updateLaf();
        } catch (ClassNotFoundException | UnsupportedLookAndFeelException | IllegalAccessException
                | InstantiationException classNotFoundException) {
            classNotFoundException.printStackTrace();
        }
    }

    String getTitle();
}
