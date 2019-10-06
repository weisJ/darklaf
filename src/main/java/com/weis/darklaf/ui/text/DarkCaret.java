package com.weis.darklaf.ui.text;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.TextUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.Segment;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

public class DarkCaret extends DefaultCaret implements UIResource {

    private static Action selectWord;
    private static Action selectLine;
    private final Segment seg;
    private final DarkHighlightPainter selectionPainter;
    private MouseEvent selectedWordEvent;
    private CaretStyle style;
    private boolean alwaysVisible;
    private boolean pasteOnMiddleMouseClick;

    public DarkCaret() {
        this(CaretStyle.THICK_VERTICAL_LINE_STYLE);
    }

    public DarkCaret(final CaretStyle style) {
        seg = new Segment();
        setStyle(style);
        selectionPainter = new DarkHighlightPainter();
        selectLine = new SelectLineAction();
        selectWord = new SelectWordAction();
        pasteOnMiddleMouseClick = true;
    }

    public boolean getRoundedSelectionEdges() {
        return ((DarkHighlightPainter) getSelectionPainter()).getRoundedEdges();
    }    @Override
    protected void positionCaret(final MouseEvent e) {
        super.positionCaret(e);
    }

    public void setRoundedSelectionEdges(final boolean rounded) {
        ((DarkHighlightPainter) getSelectionPainter()).setRoundedEdges(rounded);
    }    private void adjustCaretLocation(@NotNull final MouseEvent e) {
        if (e.isShiftDown() && getDot() != -1) {
            moveCaret(e);
        } else {
            positionCaret(e);
        }
    }

    public CaretStyle getStyle() {
        return style;
    }    private void adjustFocus(final boolean inWindow) {
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

    public void setStyle(final CaretStyle style) {
        var s = style;
        if (s == null) {
            s = CaretStyle.THICK_VERTICAL_LINE_STYLE;
        }
        if (s != this.style) {
            this.style = s;
            repaint();
        }
    }    @Override
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

    public boolean isAlwaysVisible() {
        return alwaysVisible;
    }    public boolean getPasteOnMiddleMouseClick() {
        return pasteOnMiddleMouseClick;
    }

    /**
     * Toggles whether this caret should always be visible (as opposed to
     * blinking, or not visible when the editor's window is not focused).
     * This can be used by popup windows that want the caret's location
     * to still be visible for contextual purposes while they are displayed.
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
        VERTICAL_LINE_STYLE,
        UNDERLINE_STYLE,
        BLOCK_STYLE,
        BLOCK_BORDER_STYLE,
        THICK_VERTICAL_LINE_STYLE
    }    @Override
    protected Highlighter.HighlightPainter getSelectionPainter() {
        return selectionPainter;
    }





    /**
     * Called when the mouse is clicked.  If the click was generated from
     * button1, a double click selects a word, and a triple click the
     * current line.
     *
     * @param e the mouse event
     */
    @Override
    public void mouseClicked(@NotNull final MouseEvent e) {
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
                    // to paste it; this doesn't work on Windows).  If the system
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
                        } catch (HeadlessException ignored) {
                        }
                    }
                }
            }
        }
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
    public void paint(final Graphics g) {
        if (isVisible() || alwaysVisible) {
            try {
                JTextComponent textArea = getComponent();
                g.setColor(textArea.getCaretColor());
                TextUI mapper = textArea.getUI();
                Rectangle r = mapper.modelToView2D(textArea, getDot(), Position.Bias.Forward).getBounds();

                validateWidth(r);

                if (width > 0 && height > 0 &&
                        !contains(r.x, r.y, r.width, r.height)) {
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
                switch (style) {
                    case BLOCK_STYLE:
                        if (textAreaBg == null) {
                            textAreaBg = Color.white;
                        }
                        g.setXORMode(textAreaBg);
                        g.fillRect(r.x, r.y, r.width, r.height);
                        break;
                    case BLOCK_BORDER_STYLE:
                        g.drawRect(r.x, r.y, r.width - 1, r.height);
                        break;
                    case UNDERLINE_STYLE:
                        if (textAreaBg == null) {
                            textAreaBg = Color.white;
                        }
                        g.setXORMode(textAreaBg);
                        int y = r.y + r.height;
                        g.drawLine(r.x, y, r.x + r.width - 1, y);
                        break;
                    case THICK_VERTICAL_LINE_STYLE:
                        g.drawLine(r.x, r.y, r.x, r.y + r.height);
                        r.x++;
                        g.drawLine(r.x, r.y, r.x, r.y + r.height);
                        break;
                    case VERTICAL_LINE_STYLE:
                        g.drawLine(r.x, r.y, r.x, r.y + r.height);
                        break;
                }
            } catch (BadLocationException ble) {
                ble.printStackTrace();
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
        if (rect != null && rect.width <= 1) {
            try {
                JTextComponent textArea = getComponent();
                textArea.getDocument().getText(getDot(), 1, seg);
                Font font = textArea.getFont();
                FontMetrics fm = textArea.getFontMetrics(font);
                rect.width = fm.charWidth(seg.array[seg.offset]);

                if (rect.width == 0) {
                    rect.width = fm.charWidth(' ');
                }

            } catch (BadLocationException ble) {
                // This shouldn't ever happen.
                ble.printStackTrace();
                rect.width = 8;
            }
        }
    }


}
