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
package com.github.weisj.darklaf.components.text;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import java.awt.*;

public class LineHighlighter implements Highlighter.HighlightPainter, ChangeListener {
    private JTextComponent component;
    private Color color;
    private Rectangle lastView;

    /**
     * Manually control the line color
     *
     * @param component text component that requires background line painting
     * @param color     the color of the background line
     */
    public LineHighlighter(final JTextComponent component, final Color color) {
        this.component = component;
        lastView = new Rectangle(0, 0, 0, 0);
        setColor(color);
    }

    /*
     *	You can reset the line color at any time
     *
     *  @param color  the color of the background line
     */
    public void setColor(final Color color) {
        this.color = color;
    }

    public void paint(final Graphics g, final int p0, final int p1, final Shape bounds,
                      final JTextComponent c) {
        try {
            Rectangle r = c.modelToView(c.getCaretPosition());
            g.setColor(color);
            g.fillRect(0, r.y, c.getWidth(), r.height);

            if (lastView == null) {
                lastView = r;
            }
        } catch (BadLocationException ble) {
            ble.printStackTrace();
        }
    }

    public void setComponent(final JTextComponent component) {
        this.component = component;
    }

    @Override
    public void stateChanged(final ChangeEvent e) {
        resetHighlight();
    }

    /*
     *   Caret position has changed, remove the highlight
     */
    private void resetHighlight() {
        //  Use invokeLater to make sure updates to the Document are completed,
        //  otherwise Undo processing causes the modelToView method to loop.
        if (component == null) return;
        SwingUtilities.invokeLater(() -> {
            try {
                if (component == null) return;
                int offset = component.getCaretPosition();
                Rectangle view = component.modelToView(offset);
                if (view == null) return;
                Rectangle currentView = view.getBounds();

                //  Remove the highlighting from the previously highlighted line
                if (lastView != null && lastView.y != currentView.y) {
                    if (lastView.isEmpty()) {
                        component.repaint();
                    } else {
                        component.repaint(0, lastView.y, component.getWidth(), lastView.height);
                    }
                    lastView = currentView;
                }
            } catch (BadLocationException ignored) {
            }
        });
    }
}
