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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;
import java.util.Optional;

import javax.swing.*;

import com.github.weisj.darklaf.ui.ComponentDemo;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.util.AlignmentExt;

public class GridButtonsDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new GridButtonsDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel panel = new JPanel(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        ButtonGroup bg = new ButtonGroup();

        AbstractButton[][] buttons = new AbstractButton[3][3];
        for (int y = 0; y < 3; y++) {
            for (int x = 0; x < 3; x++) {
                int index = 3 * y + x + 1;
                buttons[x][y] = createButton(index, bg, index == 5);
            }
        }

        add(panel, buttons, 0, 0, c, AlignmentExt.NORTH_WEST);
        add(panel, buttons, 1, 0, c, AlignmentExt.NORTH);
        add(panel, buttons, 2, 0, c, AlignmentExt.NORTH_EAST);

        add(panel, buttons, 0, 1, c, AlignmentExt.WEST);
        add(panel, buttons, 1, 1, c, AlignmentExt.CENTER);
        add(panel, buttons, 2, 1, c, AlignmentExt.EAST);

        add(panel, buttons, 0, 2, c, AlignmentExt.SOUTH_WEST);
        add(panel, buttons, 1, 2, c, AlignmentExt.SOUTH);
        add(panel, buttons, 2, 2, c, AlignmentExt.SOUTH_EAST);

        return new DemoPanel(panel);
    }

    private void add(final JPanel p, final AbstractButton[][] buttons, final int x, final int y,
            final GridBagConstraints c, final AlignmentExt a) {
        Optional.ofNullable(get(buttons, x, y)).ifPresent(b -> {
            b.putClientProperty(ButtonConstants.KEY_CORNER, a);

            b.putClientProperty(ButtonConstants.KEY_TOP_LEFT_NEIGHBOUR, get(buttons, x - 1, y - 1));
            b.putClientProperty(ButtonConstants.KEY_TOP_NEIGHBOUR, get(buttons, x, y - 1));
            b.putClientProperty(ButtonConstants.KEY_TOP_RIGHT_NEIGHBOUR, get(buttons, x + 1, y - 1));
            b.putClientProperty(ButtonConstants.KEY_LEFT_NEIGHBOUR, get(buttons, x - 1, y));
            b.putClientProperty(ButtonConstants.KEY_RIGHT_NEIGHBOUR, get(buttons, x + 1, y));
            b.putClientProperty(ButtonConstants.KEY_BOTTOM_LEFT_NEIGHBOUR, get(buttons, x - 1, y + 1));
            b.putClientProperty(ButtonConstants.KEY_BOTTOM_NEIGHBOUR, get(buttons, x, y + 1));
            b.putClientProperty(ButtonConstants.KEY_BOTTOM_RIGHT_NEIGHBOUR, get(buttons, x + 1, y + 1));

            c.gridx = x;
            c.gridy = y;
            c.gridwidth = 1;
            c.gridheight = 1;
            c.anchor = GridBagConstraints.CENTER;
            c.fill = GridBagConstraints.BOTH;
            p.add(b, c);
        });
    }

    private AbstractButton get(final AbstractButton[][] buttons, final int x, final int y) {
        if (x < 0 || y < 0 || x >= buttons.length || y >= buttons[x].length) {
            return null;
        }
        return buttons[x][y];
    }

    protected AbstractButton createButton(final int number, final ButtonGroup bg, final boolean isSelected) {
        AbstractButton b = new JToggleButton();
        b.setText(String.valueOf(number));
        b.putClientProperty(ButtonConstants.KEY_SQUARE, true);
        b.setSelected(isSelected);
        bg.add(b);
        return b;
    }

    @Override
    public String getTitle() {
        return "Grouped Button Demo";
    }
}
