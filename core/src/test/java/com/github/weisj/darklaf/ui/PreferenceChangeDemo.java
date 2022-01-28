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
package com.github.weisj.darklaf.ui;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.properties.icons.SolidColorIcon;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class PreferenceChangeDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        LafManager.enabledPreferenceChangeReporting(false);
        DemoExecutor.showDemo(new PreferenceChangeDemo());
    }

    @Override
    public JComponent createComponent() {
        LafManager.addThemePreferenceChangeListener(LafManager::installTheme);
        DemoPanel panel = new DemoPanel(new JToggleButton("Start") {
            {
                addActionListener(e -> {
                    setText(isSelected() ? "Stop" : "Start");
                    LafManager.enabledPreferenceChangeReporting(isSelected());
                });
            }
        });
        Icon accentColorIcon = new SolidColorIcon() {
            @Override
            public Color getColor() {
                return LafManager.getTheme().getAccentColorRule().getAccentColor();
            }
        };
        Icon selectionColorIcon = new SolidColorIcon() {
            @Override
            public Color getColor() {
                return LafManager.getTheme().getAccentColorRule().getSelectionColor();
            }
        };
        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JLabel("Accent Color", accentColorIcon, JLabel.LEFT));
        controlPanel.add(new JLabel("Selection Color", selectionColorIcon, JLabel.LEFT));

        controlPanel = panel.addControls();
        controlPanel.add(new JTextArea() {
            {
                setMargin(new Insets(5, 5, 5, 5));
                setEditable(false);
                setText("Press start/stop to enable/disable preference monitoring.\n" + "Then do one of the following\n"
                        + " - switch between dark/light theme (Windows/macOS)\n"
                        + " - toggle high contrast mode (Windows/macOS)\n"
                        + " - change accent color (Windows/macOS)\n"
                        + " - change selection color (macOS)\n" + " - change font scaling (Windows)\n"
                        + "The theme should then adjust automatically (if monitoring is started).\n");
            }
        });
        return panel;
    }

    @Override
    public void configureWindow(final Window window) {
        window.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(final WindowEvent e) {
                LafManager.enabledPreferenceChangeReporting(false);
            }
        });
    }

    @Override
    public String getName() {
        return "Preference Change Demo";
    }

    @Override
    public Theme getTheme() {
        return LafManager.themeForPreferredStyle(LafManager.getPreferredThemeStyle());
    }
}
