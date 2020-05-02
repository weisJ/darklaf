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

/**
 * @author Jannis Weis
 */
public class DarkCaret extends DefaultCaret implements UIResource {

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
    private boolean deleteCharMode;

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

    public void setDeleteCharMode(final boolean deleteCharMode) {
        this.deleteCharMode = deleteCharMode;
    }

    @Override
    public int getMark() {
        int mark = super.getMark();
        int dot = super.getDot();
        JTextComponent target = getComponent();
        if (isInsertMode()
            && target != null
            && mark == dot
            && !deleteCharMode
            && !isEndOfLine(target, dot)) {
            mark += 1;
        }
        return mark;
    }

    private boolean isEndOfLine(final JTextComponent target, final int dot) {
        Document doc = target.getDocument();
        if (dot >= doc.getLength()) return true;
        try {
            return target.getText(dot, 1).equals("\n");
        } catch (BadLocationException e) {
            return false;
        }
    }

    public boolean getRoundedSelectionEdges() {
        return ((DarkHighlightPainter) getSelectionPainter()).getRoundedEdges();
    }

    public void setRoundedSelectionEdges(final boolean rounded) {
        ((DarkHighlightPainter) getSelectionPainter()).setRoundedEdges(rounded);
    }

    public CaretStyle getStyle() {
        return isInsertMode() ? insertStyle : style;
    }

    public void setStyles(final CaretStyle style, final CaretStyle insertStyle) {
        CaretStyle s = style;
        CaretStyle is = insertStyle;
        if (s == null) {
            s = CaretStyle.THICK_VERTICAL_LINE_STYLE;
        }
        if (is == null) {
            is = CaretStyle.BLOCK_BORDER_STYLE;
        }
        if (s != this.style || is != this.insertStyle) {
            this.style = s;
            this.insertStyle = is;
            repaint();
        }
    }

    public boolean isAlwaysVisible() {
        return alwaysVisible;
    }

    /**
     * Toggles whether this caret should always be visible (as opposed to blinking, or not visible when the editor's
     * window is not focused). This can be used by popup windows that want the caret's location to still be visible for
     * contextual purposes while they are displayed.
     *
     * @param alwaysVisible Whether this caret should always be visible.
     * @see                 #isAlwaysVisible()
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
        return getStyle().getSize();
    }

    /**
     * Called when the mouse is clicked. If the click was generated from button1, a double click selects a word, and a
     * triple click the current line.
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
                        case 0 :
                            selectWord(e);
                            selectedWordEvent = null;
                            break;
                        case 1 :
                            selectLine.actionPerformed(new ActionEvent(textArea,
                                                                       ActionEvent.ACTION_PERFORMED,
                                                                       null, e.getWhen(), e.getModifiersEx()));
                            break;
                    }
                }
            } else if (SwingUtilities.isMiddleMouseButton(e) &&
                       getPasteOnMiddleMouseClick()) {
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
                        } catch (HeadlessException ignored) {}
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
        if ((textArea != null) && textArea.isEnabled() &&
            textArea.isRequestFocusEnabled()) {
            if (inWindow) {
                textArea.requestFocusInWindow();
            } else {
                textArea.requestFocus();
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
            repaint();
        }
    }

    @Override
    protected Highlighter.HighlightPainter getSelectionPainter() {
        return selectionPainter;
    }

    @Override
    public boolean isSelectionVisible() {
        return super.isSelectionVisible() && selectionPainter.isEnabled();
    }

    public void setPaintSelectionHighlight(final boolean paintSelectionHighlight) {
        selectionPainter.setEnabled(paintSelectionHighlight);
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
            } catch (BadLocationException ex) {
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
            switch (getStyle()) {
                case BLOCK_STYLE :
                    g.setXORMode(textAreaBg);
                    g.fillRect(r.x, r.y, r.width, r.height);
                    break;
                case BLOCK_BORDER_STYLE :
                    PaintUtil.drawRect(g, r.x, r.y, r.width, r.height, getStyle().getSize());
                    break;
                case UNDERLINE_STYLE :
                    g.setXORMode(textAreaBg);
                    int y = r.y + r.height;
                    g.fillRect(r.x, y - getStyle().getSize(), r.width, getStyle().getSize());
                    break;
                case THICK_VERTICAL_LINE_STYLE :
                case VERTICAL_LINE_STYLE :
                    g.fillRect(r.x, r.y, style.getSize(), r.height);
                    break;
            }
        }
    }

    /**
     * Selects word based on a mouse event.
     */
    private void selectWord(final MouseEvent e) {
        if (selectedWordEvent != null &&
            selectedWordEvent.getX() == e.getX() &&
            selectedWordEvent.getY() == e.getY()) {
            // We've already the done selection for this.
            return;
        }
        JTextComponent textArea = getComponent();
        selectWord.actionPerformed(new ActionEvent(textArea, ActionEvent.ACTION_PERFORMED,
                                                   null, e.getWhen(), e.getModifiersEx()));
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
        if (rect != null && (rect.width <= 1 || getStyle().isCharacterWidth())) {
            JTextComponent textArea = getComponent();
            try {
                textArea.getDocument().getText(getDot(), 1, seg);
            } catch (BadLocationException ble) {
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
}
