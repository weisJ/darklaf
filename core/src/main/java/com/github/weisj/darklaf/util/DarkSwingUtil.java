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
package com.github.weisj.darklaf.util;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.font.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.print.PrinterGraphics;
import java.beans.PropertyChangeEvent;
import java.text.AttributedString;
import java.text.BreakIterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import static java.awt.RenderingHints.*;

public class DarkSwingUtil {

    public static final int MIN_LAYOUT_CHARCODE = 0x0300;
    public static final int MAX_LAYOUT_CHARCODE = 0x206F;
    public static final int HI_SURROGATE_START = 0xD800;
    public static final int LO_SURROGATE_END = 0xDFFF;
    public static final FontRenderContext DEFAULT_FRC = new FontRenderContext(null, false, false);
    // Windows defines 6 font desktop properties, we will therefore only
    // cache the metrics for 6 fonts.
    private static final int CACHE_SIZE = 6;
    private static final int MIN_CHAR_INDEX = 'W';
    private static final int MAX_CHAR_INDEX = (int) 'W' + 1;
    private static final int CHAR_BUFFER_SIZE = 100;
    private static final Object charsBufferLock = new Object();
    private static final StringBuilder SKIP_CLICK_COUNT = new StringBuilder("skipClickCount");
    // Maintain a cache of CACHE_SIZE fonts and the left side bearing
    // of the characters falling into the range MIN_CHAR_INDEX to
    // MAX_CHAR_INDEX. The values in fontCache are created as needed.
    private static LSBCacheEntry[] fontCache = new LSBCacheEntry[CACHE_SIZE];
    // nextIndex in fontCache to insert a font into.
    private static int nextIndex;
    // LSBCacheEntry used to search in fontCache to see if we already
    // have an entry for a particular font
    private static LSBCacheEntry searchKey;
    private static char[] charsBuffer = new char[CHAR_BUFFER_SIZE];

    public static int setAltGraphMask(final int modifier) {
        return (modifier | InputEvent.ALT_GRAPH_DOWN_MASK);
    }

    public static boolean isScaleChanged(final PropertyChangeEvent ev) {
        return isScaleChanged(ev.getPropertyName(), ev.getOldValue(), ev.getNewValue());
    }

    public static boolean isScaleChanged(final String name, final Object oldValue, final Object newValue) {
        if (oldValue != newValue && "graphicsConfiguration".equals(name)) {
            GraphicsConfiguration newGC = (GraphicsConfiguration) oldValue;
            GraphicsConfiguration oldGC = (GraphicsConfiguration) newValue;
            AffineTransform newTx = newGC != null ? newGC.getDefaultTransform() : null;
            AffineTransform oldTx = oldGC != null ? oldGC.getDefaultTransform() : null;
            return !Objects.equals(newTx, oldTx);
        } else {
            return false;
        }
    }

    /**
     * Set the lead and anchor without affecting selection.
     */
    public static void setLeadAnchorWithoutSelection(final ListSelectionModel model,
                                                     final int lead, int anchor) {
        if (anchor == -1) {
            anchor = lead;
        }
        if (lead == -1) {
            model.setAnchorSelectionIndex(-1);
            model.setLeadSelectionIndex(-1);
        } else {
            if (model.isSelectedIndex(lead)) {
                model.addSelectionInterval(lead, lead);
            } else {
                model.removeSelectionInterval(lead, lead);
            }
            model.setAnchorSelectionIndex(anchor);
        }
    }

    /**
     * Returns the FontMetrics for the current Font of the passed
     * in Graphics.  This method is used when a Graphics
     * is available, typically when painting.  If a Graphics is not
     * available the JComponent method of the same name should be used.
     * <p>
     * Callers should pass in a non-null JComponent, the exception
     * to this is if a JComponent is not readily available at the time of
     * painting.
     * <p>
     * This does not necessarily return the FontMetrics from the
     * Graphics.
     *
     * @param c JComponent requesting FontMetrics, may be null
     * @param g Graphics Graphics
     */
    public static FontMetrics getFontMetrics(final JComponent c, final Graphics g) {
        return getFontMetrics(c, g, g.getFont());
    }

    public static FontMetrics getFontMetrics(final JComponent c, final Graphics g,
                                             final Font font) {
        if (c != null) {
            // Note: We assume that we're using the FontMetrics
            // from the widget to layout out text, otherwise we can get
            // mismatches when printing.
            return c.getFontMetrics(font);
        }
        return Toolkit.getDefaultToolkit().getFontMetrics(font);
    }

    /**
     * Ignore mouse events if the component is null, not enabled, the event
     * is not associated with the left mouse button, or the event has been
     * consumed.
     */
    public static boolean shouldIgnore(final MouseEvent me, final JComponent c) {
        return c == null || !c.isEnabled()
            || !SwingUtilities.isLeftMouseButton(me)
            || me.isConsumed();
    }

    /**
     * Maps the index of the column in the view at
     * {@code viewColumnIndex} to the index of the column
     * in the table model.  Returns the index of the corresponding
     * column in the model.  If {@code viewColumnIndex}
     * is less than zero, returns {@code viewColumnIndex}.
     *
     * @param cm              the table model
     * @param viewColumnIndex the index of the column in the view
     * @return the index of the corresponding column in the model
     * @see JTable#convertColumnIndexToModel(int)
     * @see javax.swing.plaf.basic.BasicTableHeaderUI
     */
    public static int convertColumnIndexToModel(final TableColumnModel cm,
                                                final int viewColumnIndex) {
        if (viewColumnIndex < 0) {
            return viewColumnIndex;
        }
        return cm.getColumn(viewColumnIndex).getModelIndex();
    }

    /**
     * Maps the index of the column in the {@code cm} at
     * {@code modelColumnIndex} to the index of the column
     * in the view.  Returns the index of the
     * corresponding column in the view; returns {@code -1} if this column
     * is not being displayed. If {@code modelColumnIndex} is less than zero,
     * returns {@code modelColumnIndex}.
     *
     * @param cm               the table model
     * @param modelColumnIndex the index of the column in the model
     * @return the index of the corresponding column in the view
     * @see JTable#convertColumnIndexToView(int)
     * @see javax.swing.plaf.basic.BasicTableHeaderUI
     */
    public static int convertColumnIndexToView(final TableColumnModel cm,
                                               final int modelColumnIndex) {
        if (modelColumnIndex < 0) {
            return modelColumnIndex;
        }
        for (int column = 0; column < cm.getColumnCount(); column++) {
            if (cm.getColumn(column).getModelIndex() == modelColumnIndex) {
                return column;
            }
        }
        return -1;
    }

    /**
     * Returns true if the given point is outside the preferredSize of the
     * item at the given row of the table.  (Column must be 0).
     * Does not check the "Table.isFileList" property. That should be checked
     * before calling this method.
     * This is used to make Windows {@literal L&F} JFileChooser act
     * like native dialogs.
     */
    public static boolean pointOutsidePrefSize(final JTable table, final int row, final int column, final Point p) {
        if (table.convertColumnIndexToModel(column) != 0 || row == -1) {
            return true;
        }
        TableCellRenderer tcr = table.getCellRenderer(row, column);
        Object value = table.getValueAt(row, column);
        Component cell = tcr.getTableCellRendererComponent(table, value, false,
                                                           false, row, column);
        Dimension itemSize = cell.getPreferredSize();
        Rectangle cellBounds = table.getCellRect(row, column, false);
        cellBounds.width = itemSize.width;
        cellBounds.height = itemSize.height;

        // See if coords are inside
        // ASSUME: mouse x,y will never be < cell's x,y
        assert (p.x >= cellBounds.x && p.y >= cellBounds.y);
        return p.x > cellBounds.x + cellBounds.width ||
            p.y > cellBounds.y + cellBounds.height;
    }

    /**
     * Draws the string at the specified location underlining the specified
     * character.
     *
     * @param c               JComponent that will display the string, may be null
     * @param g               Graphics to draw the text to
     * @param text            String to display
     * @param underlinedIndex Index of a character in the string to underline
     * @param x               X coordinate to draw the text at
     * @param y               Y coordinate to draw the text at
     */

    public static void drawStringUnderlineCharAt(final JComponent c, final Graphics g,
                                                 final String text, final int underlinedIndex, final int x, final int y) {
        drawStringUnderlineCharAt(c, g, text, underlinedIndex, x, y, false);
    }

    /**
     * Draws the string at the specified location underlining the specified
     * character.
     *
     * @param c               JComponent that will display the string, may be null
     * @param g               Graphics to draw the text to
     * @param text            String to display
     * @param underlinedIndex Index of a character in the string to underline
     * @param x               X coordinate to draw the text at
     * @param y               Y coordinate to draw the text at
     * @param useFPAPI        use floating point API
     */
    public static void drawStringUnderlineCharAt(final JComponent c, final Graphics g,
                                                 final String text, final int underlinedIndex,
                                                 final float x, final float y,
                                                 final boolean useFPAPI) {
        if (text == null || text.length() <= 0) {
            return;
        }
        DarkSwingUtil.drawString(c, g, text, x, y, useFPAPI);
        int textLength = text.length();
        if (underlinedIndex >= 0 && underlinedIndex < textLength) {
            float underlineRectY = y;
            int underlineRectHeight = 1;
            float underlineRectX = 0;
            int underlineRectWidth = 0;
            boolean isPrinting = isPrinting(g);
            boolean needsTextLayout = isPrinting;
            if (!needsTextLayout) {
                synchronized (charsBufferLock) {
                    syncCharsBuffer(text);
                    needsTextLayout =
                        isComplexLayout(charsBuffer, 0, textLength);
                }
            }
            if (!needsTextLayout) {
                FontMetrics fm = g.getFontMetrics();
                underlineRectX = x +
                    DarkSwingUtil.stringWidth(c, fm, text.substring(0, underlinedIndex));
                underlineRectWidth = fm.charWidth(text.charAt(underlinedIndex));
            } else {
                Graphics2D g2d = (Graphics2D) (g);
                if (g2d != null) {
                    TextLayout layout =
                        createTextLayout(c, text, g2d.getFont(),
                                         g2d.getFontRenderContext());
                    if (isPrinting) {
                        float screenWidth = (float) g2d.getFont().
                            getStringBounds(text, DEFAULT_FRC).getWidth();
                        layout = layout.getJustifiedLayout(screenWidth);
                    }
                    TextHitInfo leading =
                        TextHitInfo.leading(underlinedIndex);
                    TextHitInfo trailing =
                        TextHitInfo.trailing(underlinedIndex);
                    Shape shape =
                        layout.getVisualHighlightShape(leading, trailing);
                    Rectangle rect = shape.getBounds();
                    underlineRectX = x + rect.x;
                    underlineRectWidth = rect.width;
                }
            }
            g.fillRect((int) underlineRectX, (int) underlineRectY + 1,
                       underlineRectWidth, underlineRectHeight);
        }
    }

    /**
     * Fill the character buffer cache.  Return the buffer length.
     */
    private static int syncCharsBuffer(final String s) {
        int length = s.length();
        if ((charsBuffer == null) || (charsBuffer.length < length)) {
            charsBuffer = s.toCharArray();
        } else {
            s.getChars(0, length, charsBuffer, 0);
        }
        return length;
    }

    static boolean isPrinting(final Graphics g) {
        return (g instanceof PrinterGraphics || g instanceof PrintGraphics);
    }


    private static TextLayout createTextLayout(final JComponent c, final String s,
                                               final Font f, final FontRenderContext frc) {
        Object shaper = (c == null ?
                         null : c.getClientProperty(TextAttribute.NUMERIC_SHAPING));
        if (shaper == null) {
            return new TextLayout(s, f, frc);
        } else {
            Map<TextAttribute, Object> a = new HashMap<TextAttribute, Object>();
            a.put(TextAttribute.FONT, f);
            a.put(TextAttribute.NUMERIC_SHAPING, shaper);
            return new TextLayout(s, a, frc);
        }
    }

    /**
     * checks whether TextLayout is required to handle characters.
     *
     * @param text  characters to be tested
     * @param start start
     * @param limit limit
     * @return {@code true}  if TextLayout is required
     * {@code false} if TextLayout is not required
     */
    public static boolean isComplexLayout(final char[] text, final int start, final int limit) {
        return isComplexText(text, start, limit);
    }

    /**
     * If there is anything in the text which triggers a case
     * where char->glyph does not map 1:1 in straightforward
     * left->right ordering, then this method returns true.
     * Scripts which might require it but are not treated as such
     * due to JDK implementations will not return true.
     * ie a 'true' return is an indication of the treatment by
     * the implementation.
     * Whether supplementary characters should be considered is dependent
     * on the needs of the caller. Since this method accepts the 'char' type
     * then such chars are always represented by a pair. From a rendering
     * perspective these will all (in the cases I know of) still be one
     * unicode character -> one glyph. But if a caller is using this to
     * discover any case where it cannot make naive assumptions about
     * the number of chars, and how to index through them, then it may
     * need the option to have a 'true' return in such a case.
     */
    public static boolean isComplexText(final char[] chs, final int start, final int limit) {

        for (int i = start; i < limit; i++) {
            if (chs[i] < MIN_LAYOUT_CHARCODE) {
                continue;
            } else if (isNonSimpleChar(chs[i])) {
                return true;
            }
        }
        return false;
    }

    public static boolean isNonSimpleChar(final char ch) {
        return isComplexCharCode(ch) ||
            (ch >= HI_SURROGATE_START &&
                ch <= LO_SURROGATE_END);
    }

    public static boolean isComplexCharCode(final int code) {

        if (code < MIN_LAYOUT_CHARCODE || code > MAX_LAYOUT_CHARCODE) {
            return false;
        } else if (code <= 0x036f) {
            // Trigger layout for combining diacriticals 0x0300->0x036f
            return true;
        } else if (code < 0x0590) {
            // No automatic layout for Greek, Cyrillic, Armenian.
            return false;
        } else if (code <= 0x06ff) {
            // Hebrew 0590 - 05ff
            // Arabic 0600 - 06ff
            return true;
        } else if (code < 0x0900) {
            return false; // Syriac and Thaana
        } else if (code <= 0x0e7f) {
            // if Indic, assume shaping for conjuncts, reordering:
            // 0900 - 097F Devanagari
            // 0980 - 09FF Bengali
            // 0A00 - 0A7F Gurmukhi
            // 0A80 - 0AFF Gujarati
            // 0B00 - 0B7F Oriya
            // 0B80 - 0BFF Tamil
            // 0C00 - 0C7F Telugu
            // 0C80 - 0CFF Kannada
            // 0D00 - 0D7F Malayalam
            // 0D80 - 0DFF Sinhala
            // 0E00 - 0E7F if Thai, assume shaping for vowel, tone marks
            return true;
        } else if (code < 0x0f00) {
            return false;
        } else if (code <= 0x0fff) { // U+0F00 - U+0FFF Tibetan
            return true;
        } else if (code < 0x1100) {
            return false;
        } else if (code < 0x11ff) { // U+1100 - U+11FF Old Hangul
            return true;
        } else if (code < 0x1780) {
            return false;
        } else if (code <= 0x17ff) { // 1780 - 17FF Khmer
            return true;
        } else if (code < 0x200c) {
            return false;
        } else if (code <= 0x200d) { //  zwj or zwnj
            return true;
        } else // directional control
            if (code >= 0x202a && code <= 0x202e) { // directional control
                return true;
            } else return code >= 0x206a && code <= 0x206f;
    }

    /**
     * A variation of locationToIndex() which only returns an index if the
     * Point is within the actual bounds of a list item (not just in the cell)
     * and if the JList has the "List.isFileList" client property set.
     * Otherwise, this method returns -1.
     * This is used to make Windows {@literal L&F} JFileChooser act
     * like native dialogs.
     */
    public static int loc2IndexFileList(final JList<?> list, final Point point) {
        int index = list.locationToIndex(point);
        if (index != -1) {
            Object bySize = list.getClientProperty("List.isFileList");
            if (bySize instanceof Boolean && ((Boolean) bySize).booleanValue() &&
                !pointIsInActualBounds(list, index, point)) {
                index = -1;
            }
        }
        return index;
    }

    /**
     * Returns true if the given point is within the actual bounds of the
     * JList item at index (not just inside the cell).
     */
    private static <T> boolean pointIsInActualBounds(final JList<T> list, final int index,
                                                     final Point point) {
        ListCellRenderer<? super T> renderer = list.getCellRenderer();
        T value = list.getModel().getElementAt(index);
        Component item = renderer.getListCellRendererComponent(list,
                                                               value, index, false, false);
        Dimension itemSize = item.getPreferredSize();
        Rectangle cellBounds = list.getCellBounds(index, index);
        if (!item.getComponentOrientation().isLeftToRight()) {
            cellBounds.x += (cellBounds.width - itemSize.width);
        }
        cellBounds.width = itemSize.width;

        return cellBounds.contains(point);
    }

    /**
     * Draws the string at the specified location.
     *
     * @param c    JComponent that will display the string, may be null
     * @param g    Graphics to draw the text to
     * @param text String to display
     * @param x    X coordinate to draw the text at
     * @param y    Y coordinate to draw the text at
     */
    public static void drawString(final JComponent c, final Graphics g, final String text,
                                  final int x, final int y) {
        drawString(c, g, text, x, y, false);
    }

    /**
     * Draws the string at the specified location.
     *
     * @param c        JComponent that will display the string, may be null
     * @param g        Graphics to draw the text to
     * @param text     String to display
     * @param x        X coordinate to draw the text at
     * @param y        Y coordinate to draw the text at
     * @param useFPAPI use floating point API
     */
    public static void drawString(final JComponent c, final Graphics g, final String text,
                                  final float x, final float y, final boolean useFPAPI) {
        // c may be null

        // All non-editable widgets that draw strings call into this
        // methods.  By non-editable that means widgets like JLabel, JButton
        // but NOT JTextComponents.
        if (text == null || text.length() <= 0) { //no need to paint empty strings
            return;
        }
        if (isPrinting(g)) {
            Graphics2D g2d = (Graphics2D) (g);
            /* The printed text must scale linearly with the UI.
             * Calculate the width on screen, obtain a TextLayout with
             * advances for the printer graphics FRC, and then justify
             * it to fit in the screen width. This distributes the spacing
             * more evenly than directly laying out to the screen advances.
             */
            String trimmedText = trimTrailingSpaces(text);
            if (!trimmedText.isEmpty()) {
                float screenWidth = (float) g2d.getFont().getStringBounds
                    (trimmedText, DEFAULT_FRC).getWidth();
                TextLayout layout = createTextLayout(c, text, g2d.getFont(),
                                                     g2d.getFontRenderContext());

                layout = layout.getJustifiedLayout(screenWidth);
                /* Use alternate print color if specified */
                Color col = g2d.getColor();
//                if (col instanceof PrintColorUIResource) {
//                    g2d.setColor(((PrintColorUIResource) col).getPrintColor());
//                }

                layout.draw(g2d, x, y);

                g2d.setColor(col);
            }

            return;
        }

        // If we get here we're not printing
        if (g instanceof Graphics2D) {
            Graphics2D g2 = (Graphics2D) g;

            boolean needsTextLayout = ((c != null) &&
                (c.getClientProperty(TextAttribute.NUMERIC_SHAPING) != null));

            if (needsTextLayout) {
                synchronized (charsBufferLock) {
                    int length = syncCharsBuffer(text);
                    needsTextLayout = isComplexLayout(charsBuffer, 0, length);
                }
            }

            Object aaHint = (c == null)
                            ? null
                            : c.getClientProperty(KEY_TEXT_ANTIALIASING);
            if (aaHint != null) {
                Object oldContrast = null;
                Object oldAAValue = g2.getRenderingHint(KEY_TEXT_ANTIALIASING);
                if (aaHint != oldAAValue) {
                    g2.setRenderingHint(KEY_TEXT_ANTIALIASING, aaHint);
                } else {
                    oldAAValue = null;
                }

                Object lcdContrastHint = c.getClientProperty(
                    KEY_TEXT_LCD_CONTRAST);
                if (lcdContrastHint != null) {
                    oldContrast = g2.getRenderingHint(KEY_TEXT_LCD_CONTRAST);
                    if (lcdContrastHint.equals(oldContrast)) {
                        oldContrast = null;
                    } else {
                        g2.setRenderingHint(KEY_TEXT_LCD_CONTRAST,
                                            lcdContrastHint);
                    }
                }

                if (needsTextLayout) {
                    TextLayout layout = createTextLayout(c, text, g2.getFont(),
                                                         g2.getFontRenderContext());
                    layout.draw(g2, x, y);
                } else {
                    g2.drawString(text, x, y);
                }

                if (oldAAValue != null) {
                    g2.setRenderingHint(KEY_TEXT_ANTIALIASING, oldAAValue);
                }
                if (oldContrast != null) {
                    g2.setRenderingHint(KEY_TEXT_LCD_CONTRAST, oldContrast);
                }
                return;
            }
            if (needsTextLayout) {
                TextLayout layout = createTextLayout(c, text, g2.getFont(),
                                                     g2.getFontRenderContext());
                layout.draw(g2, x, y);
                return;
            }
        }
        g.drawString(text, (int) x, (int) y);
    }

    private static String trimTrailingSpaces(final String s) {
        int i = s.length() - 1;
        while (i >= 0 && Character.isWhitespace(s.charAt(i))) {
            i--;
        }
        return s.substring(0, i + 1);
    }

    /**
     * Returns the width of the passed in String.
     * If the passed String is {@code null}, returns zero.
     *
     * @param c      JComponent that will display the string, may be null
     * @param fm     FontMetrics used to measure the String width
     * @param string String to get the width of
     */
    public static int stringWidth(final JComponent c, final FontMetrics fm, final String string) {
        return (int) stringWidth(c, fm, string, false);
    }

    /**
     * Returns the width of the passed in String.
     * If the passed String is {@code null}, returns zero.
     *
     * @param c        JComponent that will display the string, may be null
     * @param fm       FontMetrics used to measure the String width
     * @param string   String to get the width of
     * @param useFPAPI use floating point API
     */
    public static float stringWidth(final JComponent c, final FontMetrics fm, final String string,
                                    final boolean useFPAPI) {
        if (string == null || string.equals("")) {
            return 0;
        }
        boolean needsTextLayout = ((c != null) &&
            (c.getClientProperty(TextAttribute.NUMERIC_SHAPING) != null));
        if (needsTextLayout) {
            synchronized (charsBufferLock) {
                int length = syncCharsBuffer(string);
                needsTextLayout = isComplexLayout(charsBuffer, 0, length);
            }
        }
        if (needsTextLayout) {
            TextLayout layout = createTextLayout(c, string,
                                                 fm.getFont(), fm.getFontRenderContext());
            return layout.getAdvance();
        } else {
            return getFontStringWidth(string, fm, useFPAPI);
        }
    }

    public static float getFontStringWidth(final String data, final FontMetrics fm,
                                           final boolean useFPAPI) {
        if (useFPAPI) {
            Rectangle2D bounds = fm.getFont()
                                   .getStringBounds(data, fm.getFontRenderContext());
            return (float) bounds.getWidth();
        } else {
            return fm.stringWidth(data);
        }
    }

    /**
     * This method should be used for drawing a borders over a filled rectangle.
     * Draws horizontal line, using the current color, between the points {@code
     * (x1, y)} and {@code (x2, y)} in graphics context's coordinate system.
     * Note: it use {@code Graphics.fillRect()} internally.
     *
     * @param g  Graphics to draw the line to.
     * @param x1 the first point's <i>x</i> coordinate.
     * @param x2 the second point's <i>x</i> coordinate.
     * @param y  the <i>y</i> coordinate.
     */
    public static void drawHLine(final Graphics g, int x1, int x2, final int y) {
        if (x2 < x1) {
            final int temp = x2;
            x2 = x1;
            x1 = temp;
        }
        g.fillRect(x1, y, x2 - x1 + 1, 1);
    }

    /**
     * This method should be used for drawing a borders over a filled rectangle.
     * Draws vertical line, using the current color, between the points {@code
     * (x, y1)} and {@code (x, y2)} in graphics context's coordinate system.
     * Note: it use {@code Graphics.fillRect()} internally.
     *
     * @param g  Graphics to draw the line to.
     * @param x  the <i>x</i> coordinate.
     * @param y1 the first point's <i>y</i> coordinate.
     * @param y2 the second point's <i>y</i> coordinate.
     */
    public static void drawVLine(final Graphics g, final int x, int y1, int y2) {
        if (y2 < y1) {
            final int temp = y2;
            y2 = y1;
            y1 = temp;
        }
        g.fillRect(x, y1, 1, y2 - y1 + 1);
    }

    /**
     * Returns an integer from the defaults table. If {@code key} does
     * not map to a valid {@code Integer}, or can not be convered from
     * a {@code String} to an integer, the value 0 is returned.
     *
     * @param key an {@code Object} specifying the int.
     * @return the int
     */
    public static int getUIDefaultsInt(final Object key) {
        return getUIDefaultsInt(key, null, 0);
    }

    /**
     * Returns an integer from the defaults table that is appropriate
     * for the given locale. If {@code key} does not map to a valid
     * {@code Integer}, or can not be convered from a {@code String}
     * to an integer, the value 0 is returned.
     *
     * @param key an {@code Object} specifying the int. Returned value
     *            is 0 if {@code key} is not available,
     * @param l   the {@code Locale} for which the int is desired
     * @return the int
     */
    public static int getUIDefaultsInt(final Object key, final Locale l) {
        return getUIDefaultsInt(key, l, 0);
    }

    /**
     * Returns an integer from the defaults table that is appropriate
     * for the given locale. If {@code key} does not map to a valid
     * {@code Integer}, or can not be convered from a {@code String}
     * to an integer, {@code default} is returned.
     *
     * @param key          an {@code Object} specifying the int. Returned value
     *                     is 0 if {@code key} is not available,
     * @param l            the {@code Locale} for which the int is desired
     * @param defaultValue Returned value if {@code key} is not available,
     *                     or is not an Integer
     * @return the int
     */
    public static int getUIDefaultsInt(final Object key, final Locale l, final int defaultValue) {
        Object value = UIManager.get(key, l);

        if (value instanceof Integer) {
            return (Integer) value;
        }
        if (value instanceof String) {
            try {
                return Integer.parseInt((String) value);
            } catch (NumberFormatException nfe) {
            }
        }
        return defaultValue;
    }

    /**
     * Clips the passed in String to the space provided.
     *
     * @param c              JComponent that will display the string, may be null
     * @param fm             FontMetrics used to measure the String width
     * @param string         String to display
     * @param availTextWidth Amount of space that the string can be drawn in
     * @return Clipped string that can fit in the provided space.
     */
    public static String clipStringIfNecessary(final JComponent c, final FontMetrics fm,
                                               final String string,
                                               final int availTextWidth) {
        if ((string == null) || (string.equals(""))) {
            return "";
        }
        int textWidth = stringWidth(c, fm, string);
        if (textWidth > availTextWidth) {
            return clipString(c, fm, string, availTextWidth);
        }
        return string;
    }


    /**
     * Clips the passed in String to the space provided.  NOTE: this assumes
     * the string does not fit in the available space.
     *
     * @param c              JComponent that will display the string, may be null
     * @param fm             FontMetrics used to measure the String width
     * @param string         String to display
     * @param availTextWidth Amount of space that the string can be drawn in
     * @return Clipped string that can fit in the provided space.
     */
    public static String clipString(final JComponent c, final FontMetrics fm,
                                    String string, int availTextWidth) {
        // c may be null here.
        String clipString = "...";
        availTextWidth -= stringWidth(c, fm, clipString);
        if (availTextWidth <= 0) {
            //can not fit any characters
            return clipString;
        }

        boolean needsTextLayout;
        synchronized (charsBufferLock) {
            int stringLength = syncCharsBuffer(string);
            needsTextLayout =
                isComplexLayout(charsBuffer, 0, stringLength);
            if (!needsTextLayout) {
                int width = 0;
                for (int nChars = 0; nChars < stringLength; nChars++) {
                    width += fm.charWidth(charsBuffer[nChars]);
                    if (width > availTextWidth) {
                        string = string.substring(0, nChars);
                        break;
                    }
                }
            }
        }
        if (needsTextLayout) {
            AttributedString aString = new AttributedString(string);
            if (c != null) {
                aString.addAttribute(TextAttribute.NUMERIC_SHAPING,
                                     c.getClientProperty(TextAttribute.NUMERIC_SHAPING));
            }
            LineBreakMeasurer measurer = new LineBreakMeasurer(
                aString.getIterator(), BreakIterator.getCharacterInstance(),
                getFontRenderContext(c, fm));
            string = string.substring(0, measurer.nextOffset(availTextWidth));

        }
        return string + clipString;
    }

    /*
     * Returns FontRenderContext associated with Component.
     * FontRenderContext from Component.getFontMetrics is associated
     * with the component.
     *
     * Uses Component.getFontMetrics to get the FontRenderContext from.
     * see JComponent.getFontMetrics and TextLayoutStrategy.java
     */
    public static FontRenderContext getFontRenderContext(final Component c) {
        if (c == null) {
            return DEFAULT_FRC;
        } else {
            return c.getFontMetrics(c.getFont()).getFontRenderContext();
        }
    }

    /**
     * A convenience method to get FontRenderContext.
     * Returns the FontRenderContext for the passed in FontMetrics or
     * for the passed in Component if FontMetrics is null
     */
    private static FontRenderContext getFontRenderContext(final Component c, final FontMetrics fm) {
        assert fm != null || c != null;
        return (fm != null) ? fm.getFontRenderContext()
                            : getFontRenderContext(c);
    }

    /**
     * Request focus on the given component if it doesn't already have it
     * and {@code isRequestFocusEnabled()} returns true.
     */
    public static void adjustFocus(final JComponent c) {
        if (!c.hasFocus() && c.isRequestFocusEnabled()) {
            c.requestFocus();
        }
    }

    /**
     * Change focus to the visible component in {@code JTabbedPane}.
     * This is not a general-purpose method and is here only to permit
     * sharing code.
     */
    @SuppressWarnings("deprecation")
    public static boolean tabbedPaneChangeFocusTo(final Component comp) {
        if (comp != null) {
            if (comp.isFocusTraversable()) {
                compositeRequestFocus(comp);
                return true;
            } else return comp instanceof JComponent
                && ((JComponent) comp).requestDefaultFocus();
        }

        return false;
    }

    // At this point we need this method here. But we assume that there
    // will be a common method for this purpose in the future releases.
    public static Component compositeRequestFocus(final Component component) {
        if (component instanceof Container) {
            Container container = (Container) component;
            if (container.isFocusCycleRoot()) {
                FocusTraversalPolicy policy = container.getFocusTraversalPolicy();
                Component comp = policy.getDefaultComponent(container);
                if (comp != null) {
                    comp.requestFocus();
                    return comp;
                }
            }
            Container rootAncestor = container.getFocusCycleRootAncestor();
            if (rootAncestor != null) {
                FocusTraversalPolicy policy = rootAncestor.getFocusTraversalPolicy();
                Component comp = policy.getComponentAfter(rootAncestor, container);

                if (comp != null && SwingUtilities.isDescendingFrom(comp, container)) {
                    comp.requestFocus();
                    return comp;
                }
            }
        }
        if (component.isFocusable()) {
            component.requestFocus();
            return component;
        }
        return null;
    }

    /**
     * Sets the {@code SKIP_CLICK_COUNT} client property on the component
     * if it is an instance of {@code JTextComponent} with a
     * {@code DefaultCaret}. This property, used for text components acting
     * as editors in a table or tree, tells {@code DefaultCaret} how many
     * clicks to skip before starting selection.
     */
    public static void setSkipClickCount(final Component comp, final int count) {
        if (comp instanceof JTextComponent
            && ((JTextComponent) comp).getCaret() instanceof DefaultCaret) {

            ((JTextComponent) comp).putClientProperty(SKIP_CLICK_COUNT, count);
        }
    }

    public static String displayPropertiesToCSS(final Font font, final Color fg) {
        StringBuilder rule = new StringBuilder("body {");
        if (font != null) {
            rule.append(" font-family: ");
            rule.append(font.getFamily());
            rule.append(" ; ");
            rule.append(" font-size: ");
            rule.append(font.getSize());
            rule.append("pt ;");
            if (font.isBold()) {
                rule.append(" font-weight: 700 ; ");
            }
            if (font.isItalic()) {
                rule.append(" font-style: italic ; ");
            }
        }
        if (fg != null) {
            rule.append(" color: #");
            if (fg.getRed() < 16) {
                rule.append('0');
            }
            rule.append(Integer.toHexString(fg.getRed()));
            if (fg.getGreen() < 16) {
                rule.append('0');
            }
            rule.append(Integer.toHexString(fg.getGreen()));
            if (fg.getBlue() < 16) {
                rule.append('0');
            }
            rule.append(Integer.toHexString(fg.getBlue()));
            rule.append(" ; ");
        }
        rule.append(" }");
        return rule.toString();
    }

    /**
     * Returns the left side bearing of the first character of string. The
     * left side bearing is calculated from the passed in
     * FontMetrics.  If the passed in String is less than one
     * character {@code 0} is returned.
     *
     * @param c      JComponent that will display the string
     * @param fm     FontMetrics used to measure the String width
     * @param string String to get the left side bearing for.
     * @return the left side bearing of the first character of string
     * or {@code 0} if the string is empty
     * @throws NullPointerException if {@code string} is {@code null}
     */
    public static int getLeftSideBearing(final JComponent c, final FontMetrics fm,
                                         final String string) {
        if ((string == null) || (string.length() == 0)) {
            return 0;
        }
        return getLeftSideBearing(c, fm, string.charAt(0));
    }

    /**
     * Returns the left side bearing of the first character of string. The
     * left side bearing is calculated from the passed in FontMetrics.
     *
     * @param c         JComponent that will display the string
     * @param fm        FontMetrics used to measure the String width
     * @param firstChar Character to get the left side bearing for.
     */
    public static int getLeftSideBearing(final JComponent c, final FontMetrics fm,
                                         final char firstChar) {
        int charIndex = firstChar;
        if (charIndex < MAX_CHAR_INDEX && charIndex >= MIN_CHAR_INDEX) {
            byte[] lsbs = null;

            FontRenderContext frc = getFontRenderContext(c, fm);
            Font font = fm.getFont();
            synchronized (DarkSwingUtil.class) {
                LSBCacheEntry entry = null;
                if (searchKey == null) {
                    searchKey = new LSBCacheEntry(frc, font);
                } else {
                    searchKey.reset(frc, font);
                }
                // See if we already have an entry for this pair
                for (LSBCacheEntry cacheEntry : fontCache) {
                    if (searchKey.equals(cacheEntry)) {
                        entry = cacheEntry;
                        break;
                    }
                }
                if (entry == null) {
                    // No entry for this pair, add it.
                    entry = searchKey;
                    fontCache[nextIndex] = searchKey;
                    searchKey = null;
                    nextIndex = (nextIndex + 1) % CACHE_SIZE;
                }
                return entry.getLeftSideBearing(firstChar);
            }
        }
        return 0;
    }

    /**
     * LSBCacheEntry is used to cache the left side bearing (lsb) for
     * a particular {@code Font} and {@code FontRenderContext}.
     * This only caches characters that fall in the range
     * {@code MIN_CHAR_INDEX} to {@code MAX_CHAR_INDEX}.
     */
    private static class LSBCacheEntry {
        // Used to indicate a particular entry in lsb has not been set.
        private static final byte UNSET = Byte.MAX_VALUE;
        // Used in creating a GlyphVector to get the lsb
        private static final char[] oneChar = new char[1];

        private byte[] lsbCache;
        private Font font;
        private FontRenderContext frc;


        public LSBCacheEntry(final FontRenderContext frc, final Font font) {
            lsbCache = new byte[MAX_CHAR_INDEX - MIN_CHAR_INDEX];
            reset(frc, font);

        }

        public void reset(final FontRenderContext frc, final Font font) {
            this.font = font;
            this.frc = frc;
            for (int counter = lsbCache.length - 1; counter >= 0; counter--) {
                lsbCache[counter] = UNSET;
            }
        }

        public int getLeftSideBearing(final char aChar) {
            int index = aChar - MIN_CHAR_INDEX;
            assert (index >= 0 && index < (MAX_CHAR_INDEX - MIN_CHAR_INDEX));
            byte lsb = lsbCache[index];
            if (lsb == UNSET) {
                oneChar[0] = aChar;
                GlyphVector gv = font.createGlyphVector(frc, oneChar);
                lsb = (byte) gv.getGlyphPixelBounds(0, frc, 0f, 0f).x;
                if (lsb < 0) {
                    /* HRGB/HBGR LCD glyph images will always have a pixel
                     * on the left used in colour fringe reduction.
                     * Text rendering positions this correctly but here
                     * we are using the glyph image to adjust that position
                     * so must account for it.
                     */
                    Object aaHint = frc.getAntiAliasingHint();
                    if (aaHint == VALUE_TEXT_ANTIALIAS_LCD_HRGB ||
                        aaHint == VALUE_TEXT_ANTIALIAS_LCD_HBGR) {
                        lsb++;
                    }
                }
                lsbCache[index] = lsb;
            }
            return lsb;


        }

        public boolean equals(final Object entry) {
            if (entry == this) {
                return true;
            }
            if (!(entry instanceof LSBCacheEntry)) {
                return false;
            }
            LSBCacheEntry oEntry = (LSBCacheEntry) entry;
            return (font.equals(oEntry.font) &&
                frc.equals(oEntry.frc));
        }

        public int hashCode() {
            int result = 17;
            if (font != null) {
                result = 37 * result + font.hashCode();
            }
            if (frc != null) {
                result = 37 * result + frc.hashCode();
            }
            return result;
        }
    }
}
