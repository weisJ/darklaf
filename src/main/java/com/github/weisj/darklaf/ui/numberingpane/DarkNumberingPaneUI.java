/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.ui.numberingpane;

import com.github.weisj.darklaf.components.text.LineHighlighter;
import com.github.weisj.darklaf.components.text.NumberingPane;
import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkNumberingPaneUI extends ComponentUI {

    protected static final int OUTER_PAD = 7;
    protected static final int PAD = 5;
    protected Handler handler;
    protected NumberingPane numberingPane;
    protected Color backgroundHighlight;
    protected Color foregroundHighlight;
    protected LineHighlighter currentLinePainter;
    protected int textWidth = 0;
    protected JTextComponent textComponent;
    protected JViewport viewport;
    protected int maxIconWidth = 0;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkNumberingPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        numberingPane = (NumberingPane) c;
        super.installUI(c);
        installDefaults(c);
        installListeners(c);
    }

    protected void installDefaults(final JComponent c) {
        LookAndFeel.installColorsAndFont(c, "NumberingPane.background",
                                         "NumberingPane.foreground",
                                         "NumberingPane.font");
        foregroundHighlight = UIManager.getColor("NumberingPane.currentLineForeground");
        backgroundHighlight = UIManager.getColor("NumberingPane.currentLineBackground");
        LookAndFeel.installBorder(c, "NumberingPane.border");
    }

    protected void installListeners(final JComponent c) {
        currentLinePainter = new LineHighlighter(null, backgroundHighlight);
        numberingPane.addMouseListener(getMouseListener());
        numberingPane.addMouseMotionListener(getMouseMotionListener());
        numberingPane.addPropertyChangeListener(getPropertyChangeListener());
    }

    protected MouseListener getMouseListener() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    protected MouseMotionListener getMouseMotionListener() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    protected PropertyChangeListener getPropertyChangeListener() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallListeners(c);
        currentLinePainter.setComponent(null);
        currentLinePainter = null;
        numberingPane = null;
    }

    protected void uninstallListeners(final JComponent c) {
        if (textComponent != null) {
            textComponent.getCaret().removeChangeListener(getChangeListener());
            textComponent.getCaret().removeChangeListener(currentLinePainter);
            textComponent.removePropertyChangeListener(getPropertyChangeListener());
        }
        numberingPane.removePropertyChangeListener(getPropertyChangeListener());
        numberingPane.removeMouseListener(getMouseListener());
        numberingPane.removeMouseMotionListener(getMouseMotionListener());
    }

    protected ChangeListener getChangeListener() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        super.paint(g, c);
        if (textComponent == null || viewport == null) return;
        var metrics = textComponent.getFontMetrics(textComponent.getFont());
        int descent = metrics.getDescent();

        var doc = textComponent.getDocument();
        var viewRect = viewport.getViewRect();
        var p = viewRect.getLocation();
        int startIndex = textComponent.viewToModel2D(p);
        p.y += viewRect.height;
        int endIndex = textComponent.viewToModel2D(p);

        int currOffset = textComponent.getCaretPosition();
        Element root = doc.getDefaultRootElement();
        int startLine = root.getElementIndex(startIndex);
        int endLine = root.getElementIndex(endIndex);

        int yCurr = drawHighlightBackground(g, currOffset);
        drawNumbering(g, startLine, endLine, yCurr, root, descent);
        paintIcons(g, startLine, endLine, root);
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        if (textComponent == null || viewport == null) return super.getPreferredSize(c);
        int lines = textComponent.getDocument().getDefaultRootElement().getElementCount();
        int pad = 2 * OUTER_PAD;
        if (maxIconWidth > 0) pad += PAD;
        textWidth = numberingPane.getFontMetrics(numberingPane.getFont()).stringWidth(String.valueOf(lines));
        return new Dimension(maxIconWidth + pad + textWidth, viewport.getView().getHeight());
    }

    protected int drawHighlightBackground(@NotNull final Graphics g, final int currOffset) {
        g.setColor(backgroundHighlight);
        Rectangle rect;
        try {
            rect = textComponent.modelToView2D(currOffset).getBounds();
        } catch (BadLocationException e) {
            rect = new Rectangle(0, 0, 0, 0);
        }
        g.fillRect(0, rect.y, numberingPane.getWidth(), rect.height);
        return rect.y;
    }

    protected void drawNumbering(@NotNull final Graphics g, final int startLine, final int endLine, final int yCur,
                                 @NotNull final Element root, final int descent) {
        g.setColor(numberingPane.getForeground());
        int digits = String.valueOf(root.getElementCount()).length();
        for (int i = startLine; i <= endLine; i++) {
            int off = root.getElement(i).getStartOffset();
            try {
                Rectangle lineRect = textComponent.modelToView2D(off).getBounds();
                g.setColor(lineRect.y == yCur ? foregroundHighlight : numberingPane.getForeground());
                g.drawString(String.format("%1$" + digits + "d", i), OUTER_PAD,
                             lineRect.y + lineRect.height - descent);
            } catch (BadLocationException ignored) { }
        }
    }

    protected void paintIcons(final Graphics g, final int startLine, final int endLine, @NotNull final Element root) {
        var icons = numberingPane.getIconsInRange(root.getElement(startLine).getStartOffset(),
                                                  root.getElement(endLine).getEndOffset());
        for (var icon : icons) {
            Rectangle lineRect;
            try {
                lineRect = textComponent.modelToView2D(icon.getKey().getOffset()).getBounds();
                int h = icon.getValue().getIconHeight();
                int x = OUTER_PAD + PAD + textWidth;
                int y = lineRect.y + lineRect.height / 2 - h / 2;
                icon.getValue().paintIcon(numberingPane, g, x, y);
            } catch (BadLocationException ignored) {
            }
        }
    }

    protected int calculateMaxIconWidth() {
        var icons = numberingPane.getIcons();
        int max = 0;
        for (var icon : icons) {
            max = Math.max(icon.getIconWidth(), max);
        }
        return max;
    }

    protected class Handler extends MouseAdapter implements PropertyChangeListener, ChangeListener {

        protected int selectionLineStart;
        protected int selectionLineEnd;
        protected Object currentHighlight;

        @Override
        public void mouseClicked(final MouseEvent e) {
            if (textComponent == null) return;
            var p = e.getPoint();
            int iconCount = numberingPane.getIconCount();
            int width = numberingPane.getWidth();
            if (iconCount > 0 && p.x > PAD + OUTER_PAD + textWidth && p.x <= width - PAD) {
                int offset = textComponent.viewToModel2D(new Point(0, p.y));
                var doc = textComponent.getDocument();
                int start = doc.getDefaultRootElement().getElementIndex(offset);
                int startOffset = doc.getDefaultRootElement().getElement(start).getStartOffset();
                int endOffset = doc.getDefaultRootElement().getElement(start).getEndOffset();
                var icons = numberingPane.getIconsInRange(startOffset, endOffset);
                if (!icons.isEmpty()) {
                    var icon = icons.get(0).getValue();
                    Rectangle lineRect;
                    try {
                        lineRect = textComponent.modelToView2D(start).getBounds();
                        int h = icon.getIconHeight();
                        int x = OUTER_PAD + PAD + textWidth;
                        int y = lineRect.y + lineRect.height / 2 - h / 2;
                        if (p.x >= x && p.y >= y && p.y <= y + h) {
                            System.out.println("clicked icon" + start);
                            return;
                        }
                    } catch (BadLocationException ignored) { }
                }
                System.out.println("clicked line: " + start);
            }
        }

        @Override
        public void mousePressed(final MouseEvent e) {
            if (textComponent == null) return;
            var p = e.getPoint();
            if (p.x <= OUTER_PAD + textWidth) {
                selectionLineStart = textComponent.viewToModel2D(new Point(0, p.y));
                selectionLineEnd = textComponent.viewToModel2D(new Point(textComponent.getWidth(), p.y));
                textComponent.getCaret().setDot(selectionLineEnd + 1);
                textComponent.getCaret().moveDot(Math.min(selectionLineStart,
                                                          textComponent.getDocument().getLength()));
            }
        }

        @Override
        public void stateChanged(final ChangeEvent e) {
            numberingPane.repaint();
        }

        @Override
        public void propertyChange(@NotNull final PropertyChangeEvent evt) {
            var key = evt.getPropertyName();
            if ("caret".equals(key)) {
                if (evt.getNewValue() instanceof Caret) {
                    var oldCaret = evt.getOldValue();
                    if (oldCaret instanceof Caret) {
                        ((Caret) oldCaret).removeChangeListener(getChangeListener());
                        ((Caret) oldCaret).removeChangeListener(currentLinePainter);
                    }
                    var newCaret = evt.getNewValue();
                    if (newCaret instanceof Caret) {
                        ((Caret) newCaret).addChangeListener(getChangeListener());
                        ((Caret) newCaret).addChangeListener(currentLinePainter);
                    }
                }
            } else if ("font".equals(key)) {
                var font = textComponent.getFont();
                numberingPane.setFont(font.deriveFont(Math.max(font.getSize() - 1, 1.0f)));
            } else if ("editorPane".equals(key)) {
                var newPane = evt.getNewValue();
                if (textComponent != null) {
                    currentLinePainter.setComponent(null);
                    textComponent.getHighlighter().removeHighlight(currentHighlight);
                    textComponent.getCaret().removeChangeListener(getChangeListener());
                    textComponent.getCaret().removeChangeListener(currentLinePainter);
                    textComponent.removePropertyChangeListener(getPropertyChangeListener());
                }
                if (newPane instanceof JTextComponent) {
                    textComponent = (JTextComponent) newPane;
                    try {
                        currentHighlight = textComponent.getHighlighter().addHighlight(0, 0, currentLinePainter);
                        textComponent.getCaret().addChangeListener(currentLinePainter);
                        currentLinePainter.setComponent(textComponent);
                    } catch (BadLocationException ignored) {}
                    textComponent.addPropertyChangeListener(getPropertyChangeListener());
                    textComponent.getCaret().addChangeListener(getChangeListener());
                    var font = textComponent.getFont();
                    numberingPane.setFont(font.deriveFont(Math.max(font.getSize() - 1, 1.0f)));
                }
            } else if ("icons".equals(key)) {
                var oldVal = evt.getOldValue();
                var newVal = evt.getNewValue();
                if (oldVal instanceof Icon) {
                    maxIconWidth = calculateMaxIconWidth();
                }
                if (newVal instanceof Icon) {
                    maxIconWidth = Math.max(maxIconWidth, ((Icon) newVal).getIconWidth());
                }
            } else if ("ancestor".equals(key)) {
                if (evt.getSource() == numberingPane) {
                    var parent = DarkUIUtil.getParentOfType(JScrollPane.class, (Component) evt.getNewValue());
                    if (parent != null) {
                        viewport = parent.getViewport();
                    } else {
                        viewport = null;
                    }
                }
            }
        }
    }
}
