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

import javax.swing.*;

import net.miginfocom.swing.MigLayout;

import com.github.weisj.darklaf.components.border.DarkBorders;

public class DemoPanel extends JPanel {

    private final JComponent controls;

    public DemoPanel(final JComponent component) {
        this(component, 10);
    }

    public DemoPanel(final JComponent component, final int hGap) {
        this(component, new GridBagLayout(), hGap);
    }

    public DemoPanel(final JComponent component, final LayoutManager layoutManager, final int hGap) {
        super(new BorderLayout());
        JPanel contentHolder = new JPanel(new BorderLayout());
        JPanel content = new JPanel(layoutManager);
        if (layoutManager instanceof BorderLayout) {
            content.add(component, BorderLayout.CENTER);
        } else {
            contentHolder.add(Box.createVerticalStrut(hGap), BorderLayout.NORTH);
            contentHolder.add(Box.createVerticalStrut(hGap), BorderLayout.SOUTH);
            content.add(component);
        }

        contentHolder.add(content, BorderLayout.CENTER);
        add(contentHolder, BorderLayout.CENTER);
        controls = Box.createVerticalBox();
        add(controls, BorderLayout.SOUTH);
    }

    public JPanel addControls(final LayoutManager layoutManager) {
        JPanel control = new JPanel();
        control.setLayout(layoutManager);
        return addControls(control);
    }

    public <T extends JComponent> T addControls(final T component) {
        component.setBorder(DarkBorders.createLineBorder(1, 0, 0, 0));
        controls.add(component);
        return component;
    }

    public JPanel addControls(final int columns) {
        String constraints = "fillx";
        if (columns > 0) {
            constraints += ", wrap" + columns;
        }
        return addControls(constraints);
    }

    public JPanel addControls(final String constraints) {
        return addControls(new MigLayout(constraints, "[][grow]"));
    }

    public JPanel addControls() {
        return addControls(2);
    }
}
