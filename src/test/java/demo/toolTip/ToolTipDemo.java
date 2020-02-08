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
package demo.toolTip;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

public class ToolTipDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JFrame f = new JFrame();
            JPanel p = new JPanel(new GridBagLayout());
            p.add(new JButton("Button with very very long text") {
                private final ToolTipContext context = new ToolTipContext(this).setAlignment(Alignment.CENTER)
                                                                               .setCenterAlignment(Alignment.SOUTH_EAST);

                {
                    setToolTipText("ToolTip \n multiline \n third line's a charm");
//                    setToolTipText("ToolTip");
                }

                @Override
                protected void paintComponent(final Graphics g) {
                    super.paintComponent(g);
                    g.setColor(Color.RED);
                    g.fillRect(getWidth() / 2, getHeight() / 2, 1, 1);
                }

                @Override
                public Point getToolTipLocation(final MouseEvent event) {
                    return context.getToolTipLocation(event);
                }

                @Override
                public JToolTip createToolTip() {
                    return context.getToolTip();
                }


            });
            f.setContentPane(p);
            f.setLocationRelativeTo(null);
            f.setSize(100, 100);
            f.setVisible(true);
        });
    }
}
