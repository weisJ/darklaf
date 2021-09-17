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
package com.intellij.util.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.awt.font.TextLayout;
import java.text.AttributedString;
import java.text.BreakIterator;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.util.PropertyUtil;


public class UIUtilities {
    private static final int MIN_LAYOUT_CHARCODE = 0x0300;
    private static final int MAX_LAYOUT_CHARCODE = 0x206F;
    private static final int HI_SURROGATE_START = 0xD800;
    private static final int LO_SURROGATE_END = 0xDFFF;

    private static final int CACHE_SIZE = 6;
    private static final LSBCacheEntry[] fontCache = new LSBCacheEntry[CACHE_SIZE];
    private static int nextIndex;
    private static LSBCacheEntry searchKey;
    private static final FontRenderContext DEFAULT_FRC = new FontRenderContext(null, false, false);

    private static final StringBuilder SKIP_CLICK_COUNT = new StringBuilder("skipClickCount");
    public static final StringUIClientPropertyKey BASICMENUITEMUI_MAX_TEXT_OFFSET =
            new StringUIClientPropertyKey("maxTextOffset");

    private static final Object charsBufferLock = new Object();
    private static final int CHAR_BUFFER_SIZE = 100;
    private static char[] charsBuffer = new char[CHAR_BUFFER_SIZE];

    public UIUtilities() {}

    private static int syncCharsBuffer(String s) {
        int length = s.length();
        if (charsBuffer != null && charsBuffer.length >= length) {
            s.getChars(0, length, charsBuffer, 0);
        } else {
            charsBuffer = s.toCharArray();
        }

        return length;
    }

    public static boolean isComplexLayout(char[] text, int start, int limit) {
        for (int i = start; i < limit; i++) {
            if (text[i] >= MIN_LAYOUT_CHARCODE && isNonSimpleChar(text[i])) {
                return true;
            }
        }
        return false;
    }

    private static boolean isNonSimpleChar(char ch) {
        return isComplexCharCode(ch) ||
                (ch >= HI_SURROGATE_START &&
                        ch <= LO_SURROGATE_END);
    }

    @SuppressWarnings("ConstantConditions")
    public static boolean isComplexCharCode(int code) {

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
        } else if (code < 0x10A0) { // U+1000 - U+109F Myanmar
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
        } else if (code <= 0x200d) { // zwj or zwnj
            return true;
        } else // directional control
        if (code >= 0x202a && code <= 0x202e) { // directional control
            return true;
        } else {
            return code >= 0x206a && code <= 0x206f;
        }
    }

    public static int getLeftSideBearing(JComponent c, FontMetrics fm, String string) {
        return string != null && string.length() != 0 ? getLeftSideBearing(c, fm, string.charAt(0)) : 0;
    }

    public static int getLeftSideBearing(JComponent c, FontMetrics fm, char firstChar) {
        if (firstChar < 'X' && firstChar >= 'W') {
            FontRenderContext frc = getFontRenderContext(c, fm);
            Font font = fm.getFont();
            synchronized (UIUtilities.class) {
                LSBCacheEntry entry = null;
                if (searchKey == null) {
                    searchKey = new LSBCacheEntry(frc, font);
                } else {
                    searchKey.reset(frc, font);
                }

                for (LSBCacheEntry cacheEntry : fontCache) {
                    if (searchKey.equals(cacheEntry)) {
                        entry = cacheEntry;
                        break;
                    }
                }

                if (entry == null) {
                    entry = searchKey;
                    fontCache[nextIndex] = searchKey;
                    searchKey = null;
                    nextIndex = (nextIndex + 1) % 6;
                }

                return entry.getLeftSideBearing(firstChar);
            }
        } else {
            return 0;
        }
    }

    public static int stringWidth(JComponent c, FontMetrics fm, String string) {
        if (string != null && !string.equals("")) {
            boolean needsTextLayout = c != null
                    && c.getClientProperty(TextAttribute.NUMERIC_SHAPING) != null;
            if (needsTextLayout) {
                synchronized (charsBufferLock) {
                    int length = syncCharsBuffer(string);
                    needsTextLayout = isComplexLayout(charsBuffer, 0, length);
                }
            }

            if (needsTextLayout) {
                TextLayout layout = createTextLayout(c, string, fm.getFont(), fm.getFontRenderContext());
                return (int) layout.getAdvance();
            } else {
                return fm.stringWidth(string);
            }
        } else {
            return 0;
        }
    }

    public static String clipStringIfNecessary(JComponent c, FontMetrics fm, String string, int availTextWidth) {
        if (string != null && !string.equals("")) {
            int textWidth = stringWidth(c, fm, string);
            return textWidth > availTextWidth ? clipString(c, fm, string, availTextWidth) : string;
        } else {
            return "";
        }
    }

    public static String clipString(JComponent c, FontMetrics fm, String string, int availTextWidth) {
        String clipString = "...";
        availTextWidth -= stringWidth(c, fm, clipString);
        if (availTextWidth <= 0) {
            return clipString;
        } else {
            boolean needsTextLayout;
            synchronized (charsBufferLock) {
                int stringLength = syncCharsBuffer(string);
                needsTextLayout = isComplexLayout(charsBuffer, 0, stringLength);
                if (!needsTextLayout) {
                    int width = 0;

                    for (int nChars = 0; nChars < stringLength; ++nChars) {
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

                LineBreakMeasurer measurer = new LineBreakMeasurer(aString.getIterator(),
                        BreakIterator.getCharacterInstance(), getFontRenderContext(c, fm));
                string = string.substring(0, measurer.nextOffset((float) availTextWidth));
            }

            return string + clipString;
        }
    }

    private static TextLayout createTextLayout(JComponent c, String s, Font f, FontRenderContext frc) {
        Object shaper = c == null ? null : c.getClientProperty(TextAttribute.NUMERIC_SHAPING);
        if (shaper == null) {
            return new TextLayout(s, f, frc);
        } else {
            Map<TextAttribute, Object> a = new HashMap<>();
            a.put(TextAttribute.FONT, f);
            a.put(TextAttribute.NUMERIC_SHAPING, shaper);
            return new TextLayout(s, a, frc);
        }
    }

    public static FontRenderContext getFontRenderContext(Component c) {
        return c == null ? DEFAULT_FRC : c.getFontMetrics(c.getFont()).getFontRenderContext();
    }

    private static FontRenderContext getFontRenderContext(Component c, FontMetrics fm) {
        assert fm != null || c != null;

        return fm != null ? fm.getFontRenderContext() : getFontRenderContext(c);
    }

    public static void setLeadAnchorWithoutSelection(final ListSelectionModel model, int lead, int anchor) {
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

    public static int loc2IndexFileList(final JList<?> list, final Point point) {
        int index = list.locationToIndex(point);
        if (index != -1) {
            boolean bySize = PropertyUtil.getBooleanProperty(list, "List.isFileList");
            if (bySize && !pointIsInActualBounds(list, index, point)) {
                index = -1;
            }
        }
        return index;
    }

    private static <T> boolean pointIsInActualBounds(final JList<T> list, int index, final Point point) {
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

    public static boolean pointOutsidePrefSize(JTable table, int row, int column, Point p) {
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

        // See if coordinates are inside
        // ASSUME: mouse x,y will never be < cell's x,y
        assert (p.x >= cellBounds.x && p.y >= cellBounds.y);
        return p.x > cellBounds.x + cellBounds.width ||
                p.y > cellBounds.y + cellBounds.height;
    }

    @SuppressWarnings("UnusedReturnValue")
    public static Component compositeRequestFocus(Component component) {
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
        } else {
            return null;
        }
    }

    @SuppressWarnings("deprecation")
    public static boolean tabbedPaneChangeFocusTo(Component comp) {
        if (comp != null) {
            if (comp.isFocusTraversable()) {
                compositeRequestFocus(comp);
                return true;
            }

            return comp instanceof JComponent && ((JComponent) comp).requestDefaultFocus();
        }

        return false;
    }

    public static void setSkipClickCount(Component comp, int count) {
        if (comp instanceof JTextComponent && ((JTextComponent) comp).getCaret() instanceof DefaultCaret) {
            ((JTextComponent) comp).putClientProperty(SKIP_CLICK_COUNT, count);
        }
    }

    private static class LSBCacheEntry {
        private static final char[] oneChar = new char[1];
        private final byte[] lsbCache = new byte[1];
        private Font font;
        private FontRenderContext frc;

        public LSBCacheEntry(FontRenderContext frc, Font font) {
            this.reset(frc, font);
        }

        public void reset(FontRenderContext frc, Font font) {
            this.font = font;
            this.frc = frc;

            for (int counter = this.lsbCache.length - 1; counter >= 0; --counter) {
                this.lsbCache[counter] = 127;
            }

        }

        public int getLeftSideBearing(char aChar) {
            int index = aChar - 87;
            assert index == 0;

            byte lsb = this.lsbCache[index];
            if (lsb == 127) {
                oneChar[0] = aChar;
                GlyphVector gv = this.font.createGlyphVector(this.frc, oneChar);
                lsb = (byte) gv.getGlyphPixelBounds(0, this.frc, 0.0F, 0.0F).x;
                if (lsb < 0) {
                    Object aaHint = this.frc.getAntiAliasingHint();
                    if (aaHint == RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB
                            || aaHint == RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HBGR) {
                        ++lsb;
                    }
                }

                this.lsbCache[index] = lsb;
            }

            return lsb;
        }

        @Override
        public boolean equals(Object entry) {
            if (entry == this) {
                return true;
            } else if (!(entry instanceof LSBCacheEntry)) {
                return false;
            } else {
                LSBCacheEntry oEntry = (LSBCacheEntry) entry;
                return this.font.equals(oEntry.font) && this.frc.equals(oEntry.frc);
            }
        }

        @Override
        public int hashCode() {
            int result = 17;
            if (this.font != null) {
                result = 37 * result + this.font.hashCode();
            }

            if (this.frc != null) {
                result = 37 * result + this.frc.hashCode();
            }

            return result;
        }
    }
}
