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
package ui;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.*;
import com.github.weisj.darklaf.theme.info.ColorToneRule;
import com.github.weisj.darklaf.theme.info.ContrastRule;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowListener;

public interface ComponentDemo {

    static Theme getTheme() {
        return LafManager.themeForPreferredStyle(new PreferredThemeStyle(ContrastRule.STANDARD,
                                                                         ColorToneRule.LIGHT));
    }

    JComponent createComponent();

    static void showDemo(final ComponentDemo demo) {
        showDemo(demo, null);
    }

    static void showDemo(final ComponentDemo demo, final Dimension dimension) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install(demo.createTheme());
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.addWindowListener(demo.createWindowListener());
            frame.setTitle(demo.getTitle());
            frame.setContentPane(demo.createComponent());
            frame.setJMenuBar(demo.createMenuBar());
            frame.pack();
            if (dimension != null) frame.setSize(dimension);
            frame.setVisible(true);
            frame.setLocationRelativeTo(null);
        });
    }

    default WindowListener createWindowListener() {
        return new WindowAdapter() {
        };
    }

    static JMenu createThemeMenu() {
        String currentThemeName = LafManager.getTheme().getClass().getSimpleName();
        JMenu menu = new JMenu("Theme");
        ButtonGroup bg = new ButtonGroup();
        for (Theme theme : new Theme[]{new DarculaTheme(),
                                       new IntelliJTheme(),
                                       new SolarizedLightTheme(),
                                       new SolarizedDarkTheme(),
                                       new OneDarkTheme(),
                                       new HighContrastLightTheme(),
                                       new HighContrastDarkTheme()}) {
            createThemeItem(currentThemeName, menu, bg, theme);
        }
        return menu;
    }

    static void createThemeItem(final String currentThemeName, final JMenu menu,
                                final ButtonGroup bg, final Theme theme) {
        final String name = theme.getClass().getSimpleName();
        final Action action = new AbstractAction(name) {
            @Override
            public void actionPerformed(final ActionEvent e) {
                Theme current = LafManager.getTheme();
                theme.setFontSizeRule(current.getFontSizeRule());
                theme.setAccentColorRule(current.getAccentColorRule());
                LafManager.install(theme);
            }
        };
        final JRadioButtonMenuItem mi = new JRadioButtonMenuItem(action);
        menu.add(mi);
        bg.add(mi);
        if (name.equals(currentThemeName)) {
            mi.setSelected(true);
        }
    }

    default JMenuBar createMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(createThemeMenu());
        return menuBar;
    }

    default Theme createTheme() {
        return getTheme();
    }

    String getTitle();
}
