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

import com.github.weisj.darklaf.components.border.DarkBorders;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class DemoPanel extends JPanel {

    private final JPanel controls;

    public DemoPanel(final JComponent component) {
        this(component, 10);
    }

    public DemoPanel(final JComponent component, final int hGap) {
        this(component, new GridBagLayout(), hGap);
    }

    public DemoPanel(final JComponent component, final LayoutManager layoutManager, final int hGap) {
        super(new BorderLayout());
        JPanel contentHolder = new JPanel(new BorderLayout());
        contentHolder.add(Box.createVerticalStrut(hGap), BorderLayout.NORTH);
        contentHolder.add(Box.createVerticalStrut(hGap), BorderLayout.SOUTH);
        JPanel content = new JPanel(layoutManager);
        content.add(component);

        contentHolder.add(content, BorderLayout.CENTER);
        add(contentHolder, BorderLayout.CENTER);
        controls = new JPanel();
        controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
        add(controls, BorderLayout.SOUTH);
    }

    public JPanel addControls(final int columns) {
        JPanel control = new JPanel();
        String constraints = "fillx";
        if (columns > 0) {
            constraints += ", wrap" + columns;
        }
        control.setLayout(new MigLayout(constraints, "[][grow]"));
        control.setBorder(DarkBorders.createLineBorder(1, 0, 0, 0));
        controls.add(control);
        return control;
    }

    public JPanel addControls() {
        return addControls(2);
    }
}
