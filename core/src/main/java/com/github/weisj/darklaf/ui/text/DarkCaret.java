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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.*;
import javax.swing.plaf.TextUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.*;
import javax.swing.text.DefaultHighlighterDark.DarkHighlightPainter;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.text.action.SelectLineAction;
import com.github.weisj.darklaf.ui.text.action.SelectWordAction;

/** @author Jannis Weis */
public class DarkCaret extends DefaultCaret implements UIResource {

    private static final int FLAG_SIZE = 3;

    private static Action selectWord;
    private static Action selectLine;
    private final Segment seg;
    private final DarkHighlightPainter selectionPainter;
    private MouseEvent selectedWordEvent;
    private CaretStyle style;
    private CaretStyle insertStyle;
    private boolean alwaysVisible;
    private boolean pasteOnMiddleMouseClick;

    private boolean insertMode;
    private boolean expandMode;

    private boolean dotLtr = true;
    private final int[] flagXPoints = new int[3];
    private final int[] flagYPoints = new int[3];

    public DarkCaret() {
        this(null, null);
    }

    public DarkCaret(final CaretStyle style, final CaretStyle insertStyle) {
        seg = new Segment();
        setStyles(style, insertStyle);
        selectionPainter = new DarkHighlightPainter();
        selectLine = new SelectLineAction();
        selectWord = new SelectWordAction();
        pasteOnMiddleMouseClick = true;
    }

    public boolean isInsertMode() {
        return insertMode;
    }

    public void setInsertMode(final boolean insertMode) {
        if (this.insertMode != insertMode) {
            this.insertMode = insertMode;
            repaint();
        }
    }

    public void setExpandMode(final boolean expandMode) {
        this.expandMode = expandMode;
    }

    @Override
    public int getMark() {
        int mark = super.getMark();
        int dot = super.getDot();
        JTextComponent target = getComponent();
        if (expandMode && isInsertMode() && target != null && mark == dot && !isEndOfLine(target, dot)) {
            mark += 1;
        }
        return mark;
    }

    private boolean isEndOfLine(final JTextComponent target, final int dot) {
        Document doc = target.getDocument();
        if (dot >= doc.getLength()) return true;
        try {
            return target.getText(dot, 1).equals("\n");
        } catch (final BadLocationException e) {
            return false;
        }
    }

    public boolean getRoundedSelectionEdges() {
        return getDarkSelectionPainter().getRoundedEdges();
    }

    public void setRoundedSelectionEdges(final boolean rounded) {
        getDarkSelectionPainter().setRoundedEdges(rounded);
    }

    public boolean isExtendingEnabled() {
        return getDarkSelectionPainter().isLineExtendingEnabled();
    }

    public void setLineExtendingEnabled(final boolean enabled) {
        getDarkSelectionPainter().setLineExtendingEnabled(enabled);
    }

    public CaretStyle getEffectiveStyle() {
        return isInsertMode() ? insertStyle : style;
    }

    public CaretStyle getStyle() {
        return style;
    }

    public CaretStyle getInsertStyle() {
        return insertStyle;
    }

    public void setStyle(final CaretStyle style) {
        CaretStyle s = style;
        if (s == null) {
            s = CaretStyle.THICK_VERTICAL_LINE_STYLE;
        }
        if (s != this.style) {
            this.style = s;
            repaint();
        }
    }

    public void setInsertStyle(final CaretStyle insertStyle) {
        CaretStyle is = insertStyle;
        if (is == null) {
            is = CaretStyle.BLOCK_BORDER_STYLE;
        }
        if (is != this.insertStyle) {
            this.insertStyle = is;
            repaint();
        }
    }

    public void setStyles(final CaretStyle style, final CaretStyle insertStyle) {
        setInsertStyle(insertStyle);
        setStyle(style);
    }

    public boolean isAlwaysVisible() {
        return alwaysVisible;
    }

    /**
     * Toggles whether this caret should always be visible (as opposed to blinking, or not visible when
     * the editor's window is not focused). This can be used by popup windows that want the caret's
     * location to still be visible for contextual purposes while they are displayed.
     *
     * @param alwaysVisible Whether this caret should always be visible.
     * @see #isAlwaysVisible()
     */
    public void setAlwaysVisible(final boolean alwaysVisible) {
        if (alwaysVisible != this.alwaysVisible) {
            this.alwaysVisible = alwaysVisible;
            if (!isVisible()) {
                // Force painting of caret since super class's "flasher" timer
                // won't fire when the window doesn't have focus
                repaint();
            }
        }
    }

    public enum CaretStyle {
        VERTICAL_LINE_STYLE(1, false),
        UNDERLINE_STYLE(1, true),
        BLOCK_STYLE(1, true),
        BLOCK_BORDER_STYLE(1, true),
        THICK_VERTICAL_LINE_STYLE(2, false);

        private final int size;
        private final boolean charWidth;

        CaretStyle(final int size, final boolean charWidth) {
            this.size = size;
            this.charWidth = charWidth;
        }

        public int getSize() {
            return size;
        }

        public boolean isCharacterWidth() {
            return charWidth;
        }
    }

    @Override
    public double getWidth() {
        return getEffectiveStyle().getSize();
    }

    /**
     * Called when the mouse is clicked. If the click was generated from button1, a double click selects
     * a word, and a triple click the current line.
     *
     * @param e the mouse event
     */
    @Override
    public void mouseClicked(final MouseEvent e) {
        if (!e.isConsumed()) {
            JTextComponent textArea = getComponent();
            int clickCount = e.getClickCount();

            if (SwingUtilities.isLeftMouseButton(e)) {
                if (clickCount > 2) {
                    clickCount %= 2; // Alternate selecting word/line.
                    switch (clickCount) {
                        case 0:
                            selectWord(e);
                            selectedWordEvent = null;
                            break;
                        case 1:
                            selectLine.actionPerformed(new ActionEvent(textArea, ActionEvent.ACTION_PERFORMED, null,
                                    e.getWhen(), e.getModifiersEx()));
                            break;
                    }
                }
            } else if (SwingUtilities.isMiddleMouseButton(e) && getPasteOnMiddleMouseClick()) {
                if (clickCount == 1 && textArea.isEditable() && textArea.isEnabled()) {
                    // Paste the system selection, if it exists (e.g., on UNIX
                    // platforms, the user can select text, the middle-mouse click
                    // to paste it; this doesn't work on Windows). If the system
                    // doesn't support system selection, just do a normal paste.
                    JTextComponent c = (JTextComponent) e.getSource();
                    if (c != null) {
                        try {
                            Toolkit tk = c.getToolkit();
                            Clipboard buffer = tk.getSystemSelection();
                            // If the system supports system selections, (e.g. UNIX),
                            // try to do it.
                            if (buffer != null) {
                                adjustCaretLocation(e);
                                TransferHandler th = c.getTransferHandler();
                                if (th != null) {
                                    Transferable trans = buffer.getContents(null);
                                    if (trans != null) {
                                        th.importData(c, trans);
                                    }
                                }
                                adjustFocus(true);
                            }
                            // If the system doesn't support system selections
                            // (e.g. Windows), just do a normal paste.
                            else {
                                textArea.paste();
                            }
                        } catch (final HeadlessException ignored) {
                        }
                    }
                }
            }
        }
    }

    private void adjustCaretLocation(final MouseEvent e) {
        if (e.isShiftDown() && getDot() != -1) {
            moveCaret(e);
        } else {
            positionCaret(e);
        }
    }

    private void adjustFocus(final boolean inWindow) {
        JTextComponent textArea = getComponent();
        if ((textArea != null) && textArea.isEnabled() && textArea.isRequestFocusEnabled()) {
            if (inWindow) {
                textArea.requestFocusInWindow();
            } else {
                textArea.requestFocusInWindow();
            }
        }
    }

    @Override
    protected synchronized void damage(final Rectangle r) {
        if (r != null) {
            validateWidth(r); // Check for "0" or "1" caret width
            x = r.x - 1;
            y = r.y;
            width = r.width + 4;
            height = r.height;

            if (isBidiText() && !dotLtr) {
                x -= FLAG_SIZE;
                width += FLAG_SIZE;
            }

            repaint();
        }
    }

    @Override
    protected Highlighter.HighlightPainter getSelectionPainter() {
        return getDarkSelectionPainter();
    }

    protected DarkHighlightPainter getDarkSelectionPainter() {
        return selectionPainter;
    }

    @Override
    public boolean isSelectionVisible() {
        return super.isSelectionVisible() && getDarkSelectionPainter().isEnabled();
    }

    public void setPaintSelectionHighlight(final boolean paintSelectionHighlight) {
        getDarkSelectionPainter().setEnabled(paintSelectionHighlight);
    }

    public boolean getPasteOnMiddleMouseClick() {
        return pasteOnMiddleMouseClick;
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        super.mousePressed(e);
        if (!e.isConsumed() && SwingUtilities.isRightMouseButton(e)) {
            JTextComponent c = getComponent();
            if (c != null && c.isEnabled() && c.isRequestFocusEnabled()) {
                c.requestFocusInWindow();
            }
        }
    }

    @Override
    public boolean isVisible() {
        return super.isVisible() || isAlwaysVisible();
    }

    @Override
    public void paint(final Graphics g) {
        if (isVisible()) {
            JTextComponent textArea = getComponent();
            g.setColor(textArea.getCaretColor());
            TextUI mapper = textArea.getUI();
            Rectangle r;
            try {
                r = mapper.modelToView(textArea, getDot(), getDotBias());
            } catch (final BadLocationException ex) {
                r = new Rectangle(0, 0, 0, 0);
            }
            validateWidth(r);

            if (width > 0 && height > 0 && !contains(r.x, r.y, r.width, r.height)) {
                Rectangle clip = g.getClipBounds();
                if (clip != null && !clip.contains(this)) {
                    // Clip doesn't contain the old location, force it
                    // to be repainted lest we leave a caret around.
                    repaint();
                }
                damage(r);
            }

            // Need to subtract 2 from height, otherwise
            // the caret will expand too far vertically.
            r.height -= 2;
            r.y += 1;

            Color textAreaBg = textArea.getBackground();
            if (textAreaBg == null) {
                textAreaBg = Color.white;
            }
            switch (getEffectiveStyle()) {
                case BLOCK_STYLE:
                    g.setXORMode(textAreaBg);
                    g.fillRect(r.x, r.y, r.width, r.height);
                    break;
                case BLOCK_BORDER_STYLE:
                    PaintUtil.drawRect(g, r.x, r.y, r.width, r.height, getEffectiveStyle().getSize());
                    break;
                case UNDERLINE_STYLE:
                    g.setXORMode(textAreaBg);
                    int y = r.y + r.height;
                    g.fillRect(r.x, y - getEffectiveStyle().getSize(), r.width, getEffectiveStyle().getSize());
                    break;
                case THICK_VERTICAL_LINE_STYLE:
                case VERTICAL_LINE_STYLE:
                    g.fillRect(r.x, r.y, style.getSize(), r.height);
                    if (isBidiText()) {
                        flagXPoints[0] = r.x + (dotLtr ? style.getSize() : 0);
                        flagYPoints[0] = r.y;
                        flagXPoints[1] = flagXPoints[0];
                        flagYPoints[1] = flagYPoints[0] + FLAG_SIZE;
                        flagXPoints[2] = flagXPoints[0] + ((dotLtr) ? FLAG_SIZE : -FLAG_SIZE);
                        flagYPoints[2] = flagYPoints[0];
                        g.fillPolygon(flagXPoints, flagYPoints, 3);
                    }
                    break;
            }
        }
    }

    protected boolean isBidiText() {
        Document doc = getComponent().getDocument();
        if (doc instanceof AbstractDocument) {
            Element bidi = ((AbstractDocument) doc).getBidiRootElement();
            return (bidi != null) && (bidi.getElementCount() > 1);
        }
        return false;
    }

    protected boolean isPositionLTR(int position, final Position.Bias bias) {
        Document doc = getComponent().getDocument();
        if (bias == Position.Bias.Backward && --position < 0) position = 0;
        return isLeftToRight(doc, position, position);
    }

    protected static boolean isLeftToRight(final Document doc, final int p0, final int p1) {
        if (Boolean.TRUE.equals(doc.getProperty("i18n"))) {
            if (doc instanceof AbstractDocument) {
                AbstractDocument adoc = (AbstractDocument) doc;
                Element bidiRoot = adoc.getBidiRootElement();
                int index = bidiRoot.getElementIndex(p0);
                Element bidiElem = bidiRoot.getElement(index);
                if (bidiElem.getEndOffset() >= p1) {
                    AttributeSet bidiAttrs = bidiElem.getAttributes();
                    return ((StyleConstants.getBidiLevel(bidiAttrs) % 2) == 0);
                }
            }
        }
        return true;
    }

    /** Selects word based on a mouse event. */
    private void selectWord(final MouseEvent e) {
        if (selectedWordEvent != null && selectedWordEvent.getX() == e.getX() && selectedWordEvent.getY() == e.getY()) {
            // We've already the done selection for this.
            return;
        }
        JTextComponent textArea = getComponent();
        selectWord.actionPerformed(
                new ActionEvent(textArea, ActionEvent.ACTION_PERFORMED, null, e.getWhen(), e.getModifiersEx()));
        selectedWordEvent = e;
    }

    public void setPasteOnMiddleMouseClick(final boolean paste) {
        pasteOnMiddleMouseClick = paste;
    }

    @Override
    public void setSelectionVisible(final boolean visible) {
        super.setSelectionVisible(true);
    }

    private void validateWidth(final Rectangle rect) {
        if (rect != null && (rect.width <= 1 || getEffectiveStyle().isCharacterWidth())) {
            JTextComponent textArea = getComponent();
            try {
                textArea.getDocument().getText(getDot(), 1, seg);
            } catch (final BadLocationException ble) {
                // This shouldn't ever happen.
                ble.printStackTrace();
                rect.width = 8;
            }
            Font font = textArea.getFont();
            FontMetrics fm = textArea.getFontMetrics(font);
            rect.width = fm.charWidth(seg.array[seg.offset]);

            if (rect.width == 0) {
                rect.width = fm.charWidth(' ');
            }
        }
    }

    @Override
    public void setDot(final int d, final Position.Bias bias) {
        super.setDot(d, bias);
        updateDot(d, bias);
    }

    public void updateDot(final int d, final Position.Bias bias) {
        Document doc = getComponent().getDocument();
        int dot = d;
        Position.Bias dotBias = bias;
        if (doc != null) {
            dot = Math.min(dot, doc.getLength());
        }
        dot = Math.max(dot, 0);

        // The position (0,Backward) is out of range so disallow it.
        if (dot == 0) dotBias = Position.Bias.Forward;
        dotLtr = isPositionLTR(d, dotBias);
    }

    @Override
    public void moveDot(final int dot, final Position.Bias dotBias) {
        super.moveDot(dot, dotBias);
        updateDot(dot, dotBias);
    }
}
