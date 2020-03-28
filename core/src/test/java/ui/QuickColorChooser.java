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

import com.github.weisj.darklaf.components.color.PopupColorChooser;
import com.github.weisj.darklaf.decorators.MouseClickListener;
import com.github.weisj.darklaf.icons.EmptyIcon;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class QuickColorChooser extends JPanel {

    private final SolidColorIcon icon;
    private final JCheckBox checkBox;

    public QuickColorChooser(final String title, final Color color, final Consumer<Color> onColorChange) {
        this(title, color, (b, c) -> onColorChange.accept(c), false);
    }

    public QuickColorChooser(final String title, final Color color, final BiConsumer<Boolean, Color> onStatusChange,
                             final boolean showCheckBox) {
        super(new FlowLayout(FlowLayout.LEFT, 0, 0));
        checkBox = new JCheckBox();
        if (showCheckBox) {
            checkBox.addActionListener(e -> onStatusChange.accept(isSelected(), getColor()));
            add(checkBox);
        }

        icon = new SolidColorIcon(color);
        JLabel colorLabel = new JLabel(icon, JLabel.LEFT);
        AtomicBoolean isShowing = new AtomicBoolean(false);
        colorLabel.addMouseListener((MouseClickListener) e -> {
            if (isShowing.get()) return;
            isShowing.set(true);
            PopupColorChooser.showColorChooser(colorLabel, icon.getColor(), c -> {
                if (c != null) {
                    onStatusChange.accept(isSelected(), c);
                    icon.setColor(c);
                    colorLabel.repaint();
                }
            }, () -> isShowing.set(false));
        });
        add(colorLabel);
        add(new JLabel(title, EmptyIcon.create(2, 2), JLabel.LEFT));
    }

    public QuickColorChooser(final String title, final Color color, final BiConsumer<Boolean, Color> onStatusChange) {
        this(title, color, onStatusChange, true);
    }

    public boolean isSelected() {
        return checkBox.isSelected();
    }

    public Color getColor() {
        return icon.getColor();
    }
}
