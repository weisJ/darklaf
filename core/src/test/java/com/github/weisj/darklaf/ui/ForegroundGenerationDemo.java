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
package com.github.weisj.darklaf.ui;

import java.awt.*;
import java.util.function.Consumer;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.task.ForegroundColorGenerationTask;
import com.github.weisj.darklaf.theme.event.ThemeInstalledListener;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class ForegroundGenerationDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ForegroundGenerationDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel holder = new JPanel(new GridBagLayout());
        holder.setPreferredSize(new Dimension(300, 100));
        holder.setOpaque(true);
        JLabel label = new JLabel("Demo Readability Text");
        holder.add(label);

        DemoPanel panel = new DemoPanel(holder, new BorderLayout(), 0);
        JPanel controls = panel.addControls();

        Consumer<Color> updater = c -> SwingUtilities.invokeLater(() -> {
            holder.setBackground(c);
            label.setForeground(ForegroundColorGenerationTask.makeForeground(c));
            label.repaint();
        });
        Color current = UIManager.getColor("textCompSelectionBackground");
        updater.accept(current);
        QuickColorChooser quickColorChooser = new QuickColorChooser("Background", current, updater);
        controls.add(quickColorChooser);

        LafManager.addThemeChangeListener((ThemeInstalledListener) e -> {
            Color c = UIManager.getColor("textCompSelectionBackground");
            quickColorChooser.setColor(c);
            updater.accept(c);
        });

        return panel;
    }

    @Override
    public String getName() {
        return "Foreground Color Generation Demo";
    }
}
