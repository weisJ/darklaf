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
package com.github.weisj.darklaf.ui.numberingpane;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.*;

import com.github.weisj.darklaf.components.text.IconListener;
import com.github.weisj.darklaf.components.text.IndexListener;
import com.github.weisj.darklaf.components.text.LineHighlighter;
import com.github.weisj.darklaf.components.text.NumberingPane;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public class DarkNumberingPaneUI extends ComponentUI {

    private static final Logger LOGGER = LogUtil.getLogger(DarkNumberingPaneUI.class);
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
    protected Color oldBackground;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkNumberingPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        numberingPane = (NumberingPane) c;
        super.installUI(c);
        updateViewport(c);
        installDefaults(c);
        installListeners(c);
    }

    protected void installDefaults(final JComponent c) {
        LookAndFeel.installColorsAndFont(c, "NumberingPane.background", "NumberingPane.foreground",
                "NumberingPane.font");
        foregroundHighlight = UIManager.getColor("NumberingPane.currentLineForeground");
        backgroundHighlight = UIManager.getColor("NumberingPane.currentLineBackground");
        LookAndFeel.installProperty(c, PropertyKey.OPAQUE, true);
        LookAndFeel.installBorder(c, "NumberingPane.border");
        maxIconWidth = calculateMaxIconWidth();
    }

    protected void installListeners(final JComponent c) {
        currentLinePainter = new LineHighlighter(null, backgroundHighlight);
        numberingPane.addMouseListener(getMouseListener());
        numberingPane.addMouseMotionListener(getMouseMotionListener());
        numberingPane.addPropertyChangeListener(getPropertyChangeListener());
    }

    protected MouseListener getMouseListener() {
        return getHandler();
    }

    protected MouseMotionListener getMouseMotionListener() {
        return getHandler();
    }

    protected PropertyChangeListener getPropertyChangeListener() {
        return getHandler();
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
        return getHandler();
    }

    protected Handler getHandler() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsUtil.setupStrokePainting(g);
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
        if (textComponent == null || viewport == null) return;
        FontMetrics metrics = textComponent.getFontMetrics(textComponent.getFont());
        int descent = metrics.getDescent();

        Document doc = textComponent.getDocument();
        Rectangle viewRect = viewport.getViewRect();
        Point p = viewRect.getLocation();
        int startIndex = textComponent.viewToModel(p);
        p.y += viewRect.height;
        int endIndex = textComponent.viewToModel(p);

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

    protected int drawHighlightBackground(final Graphics g, final int currOffset) {
        g.setColor(backgroundHighlight);
        Rectangle rect;
        try {
            rect = textComponent.modelToView(currOffset).getBounds();
        } catch (final BadLocationException e) {
            rect = new Rectangle(0, 0, 0, 0);
        }
        g.fillRect(0, rect.y, numberingPane.getWidth(), rect.height);
        return rect.y;
    }

    protected void drawNumbering(final Graphics g, final int startLine, final int endLine, final int yCur,
            final Element root, final int descent) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        g.setColor(numberingPane.getForeground());

        Font font = getNumberingFont(numberingPane.getTextComponent(), g, numberingPane.getFont());
        g.setFont(font);
        FontMetrics fm = numberingPane.getFontMetrics(font);

        int width = numberingPane.getWidth();
        for (int i = startLine; i <= endLine; i++) {
            int off = root.getElement(i).getStartOffset();
            try {
                String numberStr = String.valueOf(i);
                Rectangle lineRect = textComponent.modelToView(off);
                g.setColor(lineRect.y == yCur ? foregroundHighlight : numberingPane.getForeground());
                g.drawString(numberStr, width - OUTER_PAD - fm.stringWidth(numberStr) - maxIconWidth,
                        lineRect.y + lineRect.height - descent);
            } catch (final BadLocationException e) {
                LOGGER.log(Level.SEVERE, "Painting numbering failed", e);
            }
        }
        config.restore();
    }

    private Font getNumberingFont(final JComponent c, final Graphics g, final Font f) {
        Font baseFont = c.getFont();
        Font font = f;
        if (font instanceof UIResource) {
            int newSize = baseFont.getSize() - 1;
            if (newSize > 0) {
                font = font.deriveFont((float) newSize);
            } else {
                font = font.deriveFont(baseFont.getSize2D());
            }
        } else {
            font = g.getFont();
        }
        return font;
    }

    protected void paintIcons(final Graphics g, final int startLine, final int endLine, final Element root) {
        List<Map.Entry<Position, Icon>> icons = numberingPane
                .getIconsInRange(root.getElement(startLine).getStartOffset(), root.getElement(endLine).getEndOffset());
        for (Map.Entry<Position, Icon> icon : icons) {
            Rectangle lineRect;
            try {
                lineRect = textComponent.modelToView(icon.getKey().getOffset());
                int h = icon.getValue().getIconHeight();
                int x = OUTER_PAD + PAD + textWidth;
                int y = lineRect.y + (lineRect.height - h) / 2;
                icon.getValue().paintIcon(numberingPane, g, x, y);
            } catch (final BadLocationException e) {
                LOGGER.log(Level.SEVERE, "Painting icons failed", e);
            }
        }
    }

    protected int calculateMaxIconWidth() {
        Collection<Icon> icons = numberingPane.getIcons();
        int max = numberingPane.getMinimumIconWidth();
        for (Icon icon : icons) {
            max = Math.max(icon.getIconWidth(), max);
        }
        return max;
    }

    protected void updateViewport(final Component comp) {
        JScrollPane parent = DarkUIUtil.getParentOfType(JScrollPane.class, comp);
        if (parent != null) {
            viewport = parent.getViewport();
        } else {
            viewport = null;
        }
    }

    protected class Handler extends MouseAdapter implements PropertyChangeListener, ChangeListener {

        protected int selectionLineStart;
        protected int selectionLineEnd;
        protected Object currentHighlight;

        @Override
        public void mouseClicked(final MouseEvent e) {
            if (textComponent == null) return;
            Point p = e.getPoint();
            int width = numberingPane.getWidth();
            if (p.x > PAD + OUTER_PAD + textWidth && p.x <= width - PAD) {
                int offset = textComponent.viewToModel(new Point(0, p.y));
                Document doc = textComponent.getDocument();
                int start = doc.getDefaultRootElement().getElementIndex(offset);
                int startOffset = doc.getDefaultRootElement().getElement(start).getStartOffset();
                int endOffset = doc.getDefaultRootElement().getElement(start).getEndOffset();
                List<Map.Entry<Position, Icon>> icons = numberingPane.getIconsInRange(startOffset, endOffset);
                if (!icons.isEmpty()) {
                    Icon icon = icons.get(0).getValue();
                    Rectangle lineRect;
                    try {
                        lineRect = textComponent.modelToView(start).getBounds();
                        int h = icon.getIconHeight();
                        int x = OUTER_PAD + PAD + textWidth;
                        int y = lineRect.y + lineRect.height / 2 - h / 2;
                        if (p.x >= x && p.y >= y && p.y <= y + h) {
                            List<IconListener> list = numberingPane.getIconListeners(startOffset, endOffset);
                            for (IconListener listener : list) {
                                listener.iconClicked(e);
                            }
                        }
                    } catch (final BadLocationException ignored) {
                        LOGGER.log(Level.SEVERE, "Handling mouse click failed", e);
                    }
                }
                IndexListener[] list = numberingPane.getIndexListeners();
                for (IndexListener listener : list) {
                    listener.indexClicked(start, offset, e);
                }
            }
        }

        @Override
        public void mousePressed(final MouseEvent e) {
            if (textComponent == null) return;
            Point p = e.getPoint();
            selectionLineStart = textComponent.viewToModel(new Point(0, p.y));
            selectionLineEnd = textComponent.viewToModel(new Point(textComponent.getWidth(), p.y));
            if (p.x <= OUTER_PAD + textWidth) {
                textComponent.getCaret().setDot(selectionLineEnd + 1);
                textComponent.getCaret().moveDot(Math.min(selectionLineStart, textComponent.getDocument().getLength()));
            }
        }

        @Override
        public void mouseReleased(final MouseEvent e) {
            selectionLineStart = -1;
            selectionLineEnd = -1;
        }

        @Override
        public void mouseDragged(final MouseEvent e) {
            if (numberingPane.getTextComponent() == null) return;
            JTextComponent textPane = numberingPane.getTextComponent();
            Point p = e.getPoint();
            if (selectionLineEnd >= 0 && selectionLineStart >= 0) {
                int end = textPane.viewToModel(new Point(textPane.getWidth(), p.y));
                int start = textPane.viewToModel(new Point(0, p.y));
                if (selectionLineStart > end) {
                    textPane.getCaret().setDot(selectionLineEnd + 1);
                    textPane.getCaret().moveDot(Math.min(start, textPane.getDocument().getLength() - 1));
                } else {
                    textPane.getCaret().setDot(selectionLineStart);
                    textPane.getCaret().moveDot(Math.min(end + 1, textPane.getDocument().getLength()));
                }
            }
        }

        @Override
        public void stateChanged(final ChangeEvent e) {
            numberingPane.repaint();
        }

        @Override
        public void propertyChange(final PropertyChangeEvent evt) {
            String key = evt.getPropertyName();
            if (PropertyKey.CARET.equals(key)) {
                if (evt.getNewValue() instanceof Caret) {
                    Object oldCaret = evt.getOldValue();
                    if (oldCaret instanceof Caret) {
                        ((Caret) oldCaret).removeChangeListener(getChangeListener());
                        ((Caret) oldCaret).removeChangeListener(currentLinePainter);
                    }
                    Object newCaret = evt.getNewValue();
                    if (newCaret instanceof Caret) {
                        ((Caret) newCaret).addChangeListener(getChangeListener());
                        ((Caret) newCaret).addChangeListener(currentLinePainter);
                    }
                }
            } else if (NumberingPane.KEY_EDITOR.equals(key)) {
                Object newPane = evt.getNewValue();
                if (textComponent != null) {
                    currentLinePainter.setComponent(null);
                    textComponent.setBackground(oldBackground);
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
                    } catch (final BadLocationException e) {
                        LOGGER.log(Level.SEVERE, "Setting highlight failed", e);
                    }
                    textComponent.addPropertyChangeListener(getPropertyChangeListener());
                    textComponent.getCaret().addChangeListener(getChangeListener());
                    oldBackground = textComponent.getBackground();
                    textComponent.setBackground(UIManager.getColor("NumberingPane.textBackground"));
                }
            } else if (NumberingPane.KEY_ICONS.equals(key)) {
                Object oldVal = evt.getOldValue();
                Object newVal = evt.getNewValue();
                if (oldVal instanceof Icon) {
                    maxIconWidth = calculateMaxIconWidth();
                }
                if (newVal instanceof Icon) {
                    maxIconWidth = Math.max(maxIconWidth, ((Icon) newVal).getIconWidth());
                }
            } else if (PropertyKey.ANCESTOR.equals(key)) {
                if (evt.getSource() == numberingPane) {
                    updateViewport((Component) evt.getNewValue());
                }
            } else if (NumberingPane.KEY_MIN_ICON_WIDTH.equals(key)) {
                maxIconWidth = calculateMaxIconWidth();
            }
        }
    }
}
