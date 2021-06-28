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

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.PrintGraphics;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.print.PrinterGraphics;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.text.BreakIterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIDefaults.LazyValue;
import javax.swing.UIManager;
import javax.swing.event.TreeModelEvent;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.Highlighter.Highlight;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.text.JTextComponent;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;


public class UIUtilities {
    public static final Object LAF_STATE_KEY = new StringBuffer("LookAndFeel State");
    public static final Object MENU_SELECTION_MANAGER_LISTENER_KEY =
            new StringBuffer("MenuSelectionManager listener key");
    private static final int MIN_LAYOUT_CHARCODE = 0x0300;
    private static final int MAX_LAYOUT_CHARCODE = 0x206F;
    private static final int HI_SURROGATE_START = 0xD800;
    private static final int LO_SURROGATE_END = 0xDFFF;
    private static final UIUtilities.LSBCacheEntry[] fontCache = new UIUtilities.LSBCacheEntry[6];
    private static final int CACHE_SIZE = 6;
    private static int nextIndex;
    private static UIUtilities.LSBCacheEntry searchKey;
    private static final int MIN_CHAR_INDEX = 87;
    private static final int MAX_CHAR_INDEX = 88;
    public static final FontRenderContext DEFAULT_FRC = new FontRenderContext(null, false, false);
    public static final Object AA_TEXT_PROPERTY_KEY = new StringBuffer("AATextInfoPropertyKey");
    public static final String IMPLIED_CR = "CR";
    private static final StringBuilder SKIP_CLICK_COUNT = new StringBuilder("skipClickCount");
    public static final Object COMPONENT_UI_PROPERTY_KEY = new StringBuffer("ComponentUIPropertyKey");
    public static final StringUIClientPropertyKey BASICMENUITEMUI_MAX_TEXT_OFFSET =
            new StringUIClientPropertyKey("maxTextOffset");
    private static Field inputEvent_CanAccessSystemClipboard_Field = null;
    private static final String UntrustedClipboardAccess = "UNTRUSTED_CLIPBOARD_ACCESS_KEY";
    private static final int CHAR_BUFFER_SIZE = 100;
    private static final Object charsBufferLock = new Object();
    private static char[] charsBuffer = new char[100];
    private static final boolean isMac = System.getProperty("os.name").toLowerCase(Locale.ENGLISH).startsWith("mac");

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

    public static final boolean isComplexLayout(char[] text, int start, int limit) {
        for (int i = start; i < limit; i++) {
            if (text[i] < MIN_LAYOUT_CHARCODE) {
                continue;
            } else if (isNonSimpleChar(text[i])) {
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
        } else return code >= 0x206a && code <= 0x206f;
    }

    public static UIUtilities.AATextInfo drawTextAntialiased(JComponent c) {
        return c != null ? (UIUtilities.AATextInfo) c.getClientProperty(AA_TEXT_PROPERTY_KEY) : null;
    }

    public static int getLeftSideBearing(JComponent c, FontMetrics fm, String string) {
        return string != null && string.length() != 0 ? getLeftSideBearing(c, fm, string.charAt(0)) : 0;
    }

    public static int getLeftSideBearing(JComponent c, FontMetrics fm, char firstChar) {
        if (firstChar < 'X' && firstChar >= 'W') {
            byte[] lsbs = null;
            FontRenderContext frc = getFontRenderContext(c, fm);
            Font font = fm.getFont();
            Class var7 = UIUtilities.class;
            synchronized (UIUtilities.class) {
                UIUtilities.LSBCacheEntry entry = null;
                if (searchKey == null) {
                    searchKey = new UIUtilities.LSBCacheEntry(frc, font);
                } else {
                    searchKey.reset(frc, font);
                }

                UIUtilities.LSBCacheEntry[] var9 = fontCache;
                int var10 = var9.length;

                for (int var11 = 0; var11 < var10; ++var11) {
                    UIUtilities.LSBCacheEntry cacheEntry = var9[var11];
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

    public static FontMetrics getFontMetrics(JComponent c, Graphics g) {
        return getFontMetrics(c, g, g.getFont());
    }

    public static FontMetrics getFontMetrics(JComponent c, Graphics g, Font font) {
        return c != null ? c.getFontMetrics(font) : Toolkit.getDefaultToolkit().getFontMetrics(font);
    }

    public static int stringWidth(JComponent c, FontMetrics fm, String string) {
        if (string != null && !string.equals("")) {
            boolean needsTextLayout = c != null && c.getClientProperty(TextAttribute.NUMERIC_SHAPING) != null;
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

    public static void drawString(JComponent c, Graphics g, String text, int x, int y) {
        if (text != null && text.length() > 0) {
            TextLayout layout;
            if (isPrinting(g)) {
                Graphics2D g2d = getGraphics2D(g);
                if (g2d != null) {
                    String trimmedText = trimTrailingSpaces(text);
                    if (!trimmedText.isEmpty()) {
                        float screenWidth = (float) g2d.getFont().getStringBounds(trimmedText, DEFAULT_FRC).getWidth();
                        layout = createTextLayout(c, text, g2d.getFont(), g2d.getFontRenderContext());
                        layout = layout.getJustifiedLayout(screenWidth);
                        layout.draw(g2d, (float) x, (float) y);
                    }
                    return;
                }
            }

            if (g instanceof Graphics2D) {
                UIUtilities.AATextInfo info = drawTextAntialiased(c);
                Graphics2D g2 = (Graphics2D) g;
                boolean needsTextLayout = c != null && c.getClientProperty(TextAttribute.NUMERIC_SHAPING) != null;
                if (needsTextLayout) {
                    synchronized (charsBufferLock) {
                        int length = syncCharsBuffer(text);
                        needsTextLayout = isComplexLayout(charsBuffer, 0, length);
                    }
                }

                if (info != null) {
                    Object oldContrast = null;
                    Object oldAAValue = g2.getRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING);
                    if (info.aaHint != oldAAValue) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, info.aaHint);
                    } else {
                        oldAAValue = null;
                    }

                    if (info.lcdContrastHint != null) {
                        oldContrast = g2.getRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST);
                        if (info.lcdContrastHint.equals(oldContrast)) {
                            oldContrast = null;
                        } else {
                            g2.setRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST, info.lcdContrastHint);
                        }
                    }

                    if (needsTextLayout) {
                        TextLayout layout2 = createTextLayout(c, text, g2.getFont(), g2.getFontRenderContext());
                        layout2.draw(g2, (float) x, (float) y);
                    } else {
                        g.drawString(text, x, y);
                    }

                    if (oldAAValue != null) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, oldAAValue);
                    }

                    if (oldContrast != null) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST, oldContrast);
                    }

                    return;
                }

                if (needsTextLayout) {
                    layout = createTextLayout(c, text, g2.getFont(), g2.getFontRenderContext());
                    layout.draw(g2, (float) x, (float) y);
                    return;
                }
            }

            g.drawString(text, x, y);
        }
    }

    public static void drawStringUnderlineCharAt(JComponent c, Graphics g, String text, int underlinedIndex, int x,
            int y) {
        if (text != null && text.length() > 0) {
            drawString(c, g, text, x, y);
            int textLength = text.length();
            if (underlinedIndex >= 0 && underlinedIndex < textLength) {
                int underlineRectHeight = 1;
                int underlineRectX = 0;
                int underlineRectWidth = 0;
                boolean isPrinting = isPrinting(g);
                boolean needsTextLayout = isPrinting;
                if (!isPrinting) {
                    synchronized (charsBufferLock) {
                        syncCharsBuffer(text);
                        needsTextLayout = isComplexLayout(charsBuffer, 0, textLength);
                    }
                }

                if (!needsTextLayout) {
                    FontMetrics fm = g.getFontMetrics();
                    underlineRectX = x + stringWidth(c, fm, text.substring(0, underlinedIndex));
                    underlineRectWidth = fm.charWidth(text.charAt(underlinedIndex));
                } else {
                    Graphics2D g2d = getGraphics2D(g);
                    if (g2d != null) {
                        TextLayout layout = createTextLayout(c, text, g2d.getFont(), g2d.getFontRenderContext());
                        if (isPrinting) {
                            float screenWidth = (float) g2d.getFont().getStringBounds(text, DEFAULT_FRC).getWidth();
                            layout = layout.getJustifiedLayout(screenWidth);
                        }

                        TextHitInfo leading = TextHitInfo.leading(underlinedIndex);
                        TextHitInfo trailing = TextHitInfo.trailing(underlinedIndex);
                        Shape shape = layout.getVisualHighlightShape(leading, trailing);
                        Rectangle rect = shape.getBounds();
                        underlineRectX = x + rect.x;
                        underlineRectWidth = rect.width;
                    }
                }

                g.fillRect(underlineRectX, y + 1, underlineRectWidth, underlineRectHeight);
            }

        }
    }

    public static int loc2IndexFileList(JList list, Point point) {
        int index = list.locationToIndex(point);
        if (index != -1) {
            Object bySize = list.getClientProperty("List.isFileList");
            if (bySize instanceof Boolean && (Boolean) bySize && !pointIsInActualBounds(list, index, point)) {
                index = -1;
            }
        }

        return index;
    }

    private static boolean pointIsInActualBounds(JList list, int index, Point point) {
        ListCellRenderer renderer = list.getCellRenderer();
        ListModel dataModel = list.getModel();
        Object value = dataModel.getElementAt(index);
        Component item = renderer.getListCellRendererComponent(list, value, index, false, false);
        Dimension itemSize = item.getPreferredSize();
        Rectangle cellBounds = list.getCellBounds(index, index);
        if (!item.getComponentOrientation().isLeftToRight()) {
            cellBounds.x += cellBounds.width - itemSize.width;
        }

        cellBounds.width = itemSize.width;
        return cellBounds.contains(point);
    }

    public static boolean pointOutsidePrefSize(JTable table, int row, int column, Point p) {
        if (table.convertColumnIndexToModel(column) == 0 && row != -1) {
            TableCellRenderer tcr = table.getCellRenderer(row, column);
            Object value = table.getValueAt(row, column);
            Component cell = tcr.getTableCellRendererComponent(table, value, false, false, row, column);
            Dimension itemSize = cell.getPreferredSize();
            Rectangle cellBounds = table.getCellRect(row, column, false);
            cellBounds.width = itemSize.width;
            cellBounds.height = itemSize.height;

            assert p.x >= cellBounds.x && p.y >= cellBounds.y;

            return p.x > cellBounds.x + cellBounds.width || p.y > cellBounds.y + cellBounds.height;
        } else {
            return true;
        }
    }

    public static void setLeadAnchorWithoutSelection(ListSelectionModel model, int lead, int anchor) {
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

    public static boolean shouldIgnore(MouseEvent me, JComponent c) {
        return c == null || !c.isEnabled() || !SwingUtilities.isLeftMouseButton(me) || me.isConsumed();
    }

    public static void adjustFocus(JComponent c) {
        if (!c.hasFocus() && c.isRequestFocusEnabled()) {
            c.requestFocus();
        }

    }

    public static int drawChars(JComponent c, Graphics g, char[] data, int offset, int length, int x, int y) {
        return (int) drawChars(c, g, data, offset, length, (float) x, (float) y, false);
    }

    public static float drawChars(JComponent c, Graphics g, char[] data, int offset, int length, float x, float y) {
        return drawChars(c, g, data, offset, length, x, y, true);
    }

    public static float drawChars(JComponent c, Graphics g, char[] data, int offset, int length, float x, float y,
            boolean useFPAPI) {
        if (length <= 0) {
            return x;
        } else {
            float nextX = x + getFontCharsWidth(data, offset, length, getFontMetrics(c, g), useFPAPI);
            Graphics2D g2;
            if (isPrinting(g)) {
                g2 = getGraphics2D(g);
                if (g2 != null) {
                    FontRenderContext deviceFontRenderContext = g2.getFontRenderContext();
                    FontRenderContext frc = getFontRenderContext(c);
                    if (frc != null && !isFontRenderContextPrintCompatible(deviceFontRenderContext, frc)) {
                        String text = new String(data, offset, length);
                        TextLayout layout = new TextLayout(text, g2.getFont(), deviceFontRenderContext);
                        String trimmedText = trimTrailingSpaces(text);
                        if (!trimmedText.isEmpty()) {
                            float screenWidth = (float) g2.getFont().getStringBounds(trimmedText, frc).getWidth();
                            layout = layout.getJustifiedLayout(screenWidth);
                            layout.draw(g2, x, y);
                        }

                        return nextX;
                    }
                }
            }

            if (!(g instanceof Graphics2D)) {
                g.drawChars(data, offset, length, (int) x, (int) y);
                return nextX;
            } else {
                g2 = (Graphics2D) g;
                UIUtilities.AATextInfo info = drawTextAntialiased(c);
                if (info != null) {
                    Object oldContrast = null;
                    Object oldAAValue = g2.getRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING);
                    if (info.aaHint != null && info.aaHint != oldAAValue) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, info.aaHint);
                    } else {
                        oldAAValue = null;
                    }

                    if (info.lcdContrastHint != null) {
                        oldContrast = g2.getRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST);
                        if (info.lcdContrastHint.equals(oldContrast)) {
                            oldContrast = null;
                        } else {
                            g2.setRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST, info.lcdContrastHint);
                        }
                    }

                    g2.drawString(new String(data, offset, length), x, y);
                    if (oldAAValue != null) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, oldAAValue);
                    }

                    if (oldContrast != null) {
                        g2.setRenderingHint(RenderingHints.KEY_TEXT_LCD_CONTRAST, oldContrast);
                    }
                } else {
                    g2.drawString(new String(data, offset, length), x, y);
                }

                return nextX;
            }
        }
    }

    public static float getFontCharWidth(char c, FontMetrics fm, boolean useFPAPI) {
        return getFontCharsWidth(new char[] {c}, 0, 1, fm, useFPAPI);
    }

    public static float getFontCharsWidth(char[] data, int offset, int len, FontMetrics fm, boolean useFPAPI) {
        if (len == 0) {
            return 0.0F;
        } else if (useFPAPI) {
            Rectangle2D bounds = fm.getFont().getStringBounds(data, offset, offset + len, fm.getFontRenderContext());
            return (float) bounds.getWidth();
        } else {
            return (float) fm.charsWidth(data, offset, len);
        }
    }

    public static float getFontStringWidth(String data, FontMetrics fm, boolean useFPAPI) {
        if (useFPAPI) {
            Rectangle2D bounds = fm.getFont().getStringBounds(data, fm.getFontRenderContext());
            return (float) bounds.getWidth();
        } else {
            return (float) fm.stringWidth(data);
        }
    }

    public static float drawString(JComponent c, Graphics g, AttributedCharacterIterator iterator, int x, int y) {
        return drawStringImpl(c, g, iterator, (float) x, (float) y);
    }

    public static float drawString(JComponent c, Graphics g, AttributedCharacterIterator iterator, float x, float y) {
        return drawStringImpl(c, g, iterator, x, y);
    }

    private static float drawStringImpl(JComponent c, Graphics g, AttributedCharacterIterator iterator, float x,
            float y) {
        boolean isPrinting = isPrinting(g);
        Graphics2D g2d = getGraphics2D(g);
        float retVal;
        if (g2d == null) {
            g.drawString(iterator, (int) x, (int) y);
            retVal = x;
        } else {
            FontRenderContext frc;
            if (isPrinting) {
                frc = getFontRenderContext(c);
                if (frc.isAntiAliased() || frc.usesFractionalMetrics()) {
                    frc = new FontRenderContext(frc.getTransform(), false, false);
                }
            } else if ((frc = getFRCProperty(c)) == null) {
                frc = g2d.getFontRenderContext();
            }

            TextLayout layout;
            if (isPrinting) {
                FontRenderContext deviceFRC = g2d.getFontRenderContext();
                if (!isFontRenderContextPrintCompatible(frc, deviceFRC)) {
                    layout = new TextLayout(iterator, deviceFRC);
                    AttributedCharacterIterator trimmedIt = getTrimmedTrailingSpacesIterator(iterator);
                    if (trimmedIt != null) {
                        float screenWidth = (new TextLayout(trimmedIt, frc)).getAdvance();
                        layout = layout.getJustifiedLayout(screenWidth);
                    }
                } else {
                    layout = new TextLayout(iterator, frc);
                }
            } else {
                layout = new TextLayout(iterator, frc);
            }

            layout.draw(g2d, x, y);
            retVal = layout.getAdvance();
        }
        return retVal;
    }

    public static void drawVLine(Graphics g, int x, int y1, int y2) {
        if (y2 < y1) {
            int temp = y2;
            y2 = y1;
            y1 = temp;
        }

        g.fillRect(x, y1, 1, y2 - y1 + 1);
    }

    public static void drawHLine(Graphics g, int x1, int x2, int y) {
        if (x2 < x1) {
            int temp = x2;
            x2 = x1;
            x1 = temp;
        }

        g.fillRect(x1, y, x2 - x1 + 1, 1);
    }

    public static void drawRect(Graphics g, int x, int y, int w, int h) {
        if (w >= 0 && h >= 0) {
            if (h != 0 && w != 0) {
                g.fillRect(x, y, w, 1);
                g.fillRect(x + w, y, 1, h);
                g.fillRect(x + 1, y + h, w, 1);
                g.fillRect(x, y + 1, 1, h);
            } else {
                g.fillRect(x, y, w + 1, h + 1);
            }

        }
    }

    private static TextLayout createTextLayout(JComponent c, String s, Font f, FontRenderContext frc) {
        Object shaper = c == null ? null : c.getClientProperty(TextAttribute.NUMERIC_SHAPING);
        if (shaper == null) {
            return new TextLayout(s, f, frc);
        } else {
            Map<TextAttribute, Object> a = new HashMap();
            a.put(TextAttribute.FONT, f);
            a.put(TextAttribute.NUMERIC_SHAPING, shaper);
            return new TextLayout(s, a, frc);
        }
    }

    private static boolean isFontRenderContextPrintCompatible(FontRenderContext frc1, FontRenderContext frc2) {
        if (frc1 == frc2) {
            return true;
        } else if (frc1 != null && frc2 != null) {
            if (frc1.getFractionalMetricsHint() != frc2.getFractionalMetricsHint()) {
                return false;
            } else if (!frc1.isTransformed() && !frc2.isTransformed()) {
                return true;
            } else {
                double[] mat1 = new double[4];
                double[] mat2 = new double[4];
                frc1.getTransform().getMatrix(mat1);
                frc2.getTransform().getMatrix(mat2);
                return mat1[0] == mat2[0] && mat1[1] == mat2[1] && mat1[2] == mat2[2] && mat1[3] == mat2[3];
            }
        } else {
            return false;
        }
    }

    public static Graphics2D getGraphics2D(Graphics g) {
        if (g instanceof Graphics2D) {
            return (Graphics2D) g;
        } else {
            return null;
        }
    }

    public static FontRenderContext getFontRenderContext(Component c) {
        assert c != null;

        return c == null ? DEFAULT_FRC : c.getFontMetrics(c.getFont()).getFontRenderContext();
    }

    private static FontRenderContext getFontRenderContext(Component c, FontMetrics fm) {
        assert fm != null || c != null;

        return fm != null ? fm.getFontRenderContext() : getFontRenderContext(c);
    }

    private static FontRenderContext getFRCProperty(JComponent c) {
        if (c != null) {
            GraphicsConfiguration gc = c.getGraphicsConfiguration();
            AffineTransform tx = gc == null ? null : gc.getDefaultTransform();
            if (!isMac && tx == null && !GraphicsEnvironment.isHeadless()) {
                tx = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice()
                        .getDefaultConfiguration().getDefaultTransform();
            }

            UIUtilities.AATextInfo info = (UIUtilities.AATextInfo) c.getClientProperty(AA_TEXT_PROPERTY_KEY);
            if (info != null) {
                return info.getFRC(tx);
            }
        }

        return null;
    }

    static boolean isPrinting(Graphics g) {
        return g instanceof PrinterGraphics || g instanceof PrintGraphics;
    }

    private static String trimTrailingSpaces(String s) {
        int i;
        for (i = s.length() - 1; i >= 0 && Character.isWhitespace(s.charAt(i)); --i) {
        }

        return s.substring(0, i + 1);
    }

    private static AttributedCharacterIterator getTrimmedTrailingSpacesIterator(AttributedCharacterIterator iterator) {
        int curIdx = iterator.getIndex();

        char c;
        for (c = iterator.last(); c != '\uffff' && Character.isWhitespace(c); c = iterator.previous()) {
        }

        if (c != '\uffff') {
            int endIdx = iterator.getIndex();
            if (endIdx == iterator.getEndIndex() - 1) {
                iterator.setIndex(curIdx);
                return iterator;
            } else {
                AttributedString trimmedText = new AttributedString(iterator, iterator.getBeginIndex(), endIdx + 1);
                return trimmedText.getIterator();
            }
        } else {
            return null;
        }
    }

    public static boolean useSelectedTextColor(Highlight h, JTextComponent c) {
        HighlightPainter painter = h.getPainter();
        String painterClass = painter.getClass().getName();
        if (painterClass.indexOf("javax.swing.text.DefaultHighlighter") != 0
                && painterClass.indexOf("com.sun.java.swing.plaf.windows.WindowsTextUI") != 0) {
            return false;
        } else {
            try {
                DefaultHighlightPainter defPainter = (DefaultHighlightPainter) painter;
                return defPainter.getColor() == null || defPainter.getColor().equals(c.getSelectionColor());
            } catch (ClassCastException var5) {
                return false;
            }
        }
    }

    private static synchronized boolean inputEvent_canAccessSystemClipboard(InputEvent ie) {
        if (inputEvent_CanAccessSystemClipboard_Field == null) {
            inputEvent_CanAccessSystemClipboard_Field =
                AccessController.doPrivileged(new PrivilegedAction<Field>() {
                    public Field run() {
                        try {
                            Field field = InputEvent.class.getDeclaredField("canAccessSystemClipboard");
                            field.setAccessible(true);
                            return field;
                        } catch (SecurityException var2) {
                        } catch (NoSuchFieldException var3) {
                        }

                        return null;
                    }
                });
        }

        if (inputEvent_CanAccessSystemClipboard_Field == null) {
            return false;
        } else {
            boolean ret = false;

            try {
                ret = inputEvent_CanAccessSystemClipboard_Field.getBoolean(ie);
            } catch (IllegalAccessException var3) {
            }

            return ret;
        }
    }

    private static boolean isAccessClipboardGesture(InputEvent ie) {
        boolean allowedGesture = false;
        if (ie instanceof KeyEvent) {
            KeyEvent ke = (KeyEvent) ie;
            int keyCode = ke.getKeyCode();
            int keyModifiers = ke.getModifiers();
            switch (keyCode) {
                case 67:
                case 86:
                case 88:
                    allowedGesture = keyModifiers == 2;
                    break;
                case 127:
                    allowedGesture = keyModifiers == 1;
                    break;
                case 155:
                    allowedGesture = keyModifiers == 2 || keyModifiers == 1;
                    break;
                case 65485:
                case 65487:
                case 65489:
                    allowedGesture = true;
            }
        }

        return allowedGesture;
    }

    private static boolean canEventAccessSystemClipboard(AWTEvent e, boolean checkGesture) {
        if (!EventQueue.isDispatchThread()) {
            return true;
        } else {
            return e instanceof InputEvent && (!checkGesture || isAccessClipboardGesture((InputEvent) e)) && inputEvent_canAccessSystemClipboard((InputEvent) e);
        }
    }

    public static void checkAccess(int modifiers) {
        if (System.getSecurityManager() != null && !Modifier.isPublic(modifiers)) {
            throw new SecurityException("Resource is not accessible");
        }
    }

    private static boolean canCurrentEventAccessSystemClipboard(boolean checkGesture) {
        AWTEvent event = EventQueue.getCurrentEvent();
        return canEventAccessSystemClipboard(event, checkGesture);
    }

    public static String displayPropertiesToCSS(Font font, Color fg) {
        StringBuffer rule = new StringBuffer("body {");
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

    public static Object makeIcon(final Class<?> baseClass, final Class<?> rootClass, final String imageFile) {
        return new LazyValue() {
            public Object createValue(UIDefaults table) {
                byte[] buffer = AccessController.doPrivileged(new PrivilegedAction<byte[]>() {
                    public byte[] run() {
                        try {
                            InputStream resource = null;

                            for (Class srchClass = baseClass; srchClass != null; srchClass =
                                    srchClass.getSuperclass()) {
                                resource = srchClass.getResourceAsStream(imageFile);
                                if (resource != null || srchClass == rootClass) {
                                    break;
                                }
                            }

                            if (resource == null) {
                                return null;
                            } else {
                                BufferedInputStream in = new BufferedInputStream(resource);
                                ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
                                byte[] buffer = new byte[1024];

                                int n;
                                while ((n = in.read(buffer)) > 0) {
                                    out.write(buffer, 0, n);
                                }

                                in.close();
                                out.flush();
                                return out.toByteArray();
                            }
                        } catch (IOException var7) {
                            System.err.println(var7);
                            return null;
                        }
                    }
                });
                if (buffer == null) {
                    return null;
                } else if (buffer.length == 0) {
                    System.err.println("warning: " + imageFile + " is zero-length");
                    return null;
                } else {
                    return new ImageIconUIResource(buffer);
                }
            }
        };
    }

    public static int getUIDefaultsInt(Object key) {
        return getUIDefaultsInt(key, 0);
    }

    public static int getUIDefaultsInt(Object key, Locale l) {
        return getUIDefaultsInt(key, l, 0);
    }

    public static int getUIDefaultsInt(Object key, int defaultValue) {
        return getUIDefaultsInt(key, null, defaultValue);
    }

    public static int getUIDefaultsInt(Object key, Locale l, int defaultValue) {
        Object value = UIManager.get(key, l);
        if (value instanceof Integer) {
            return (Integer) value;
        } else {
            if (value instanceof String) {
                try {
                    return Integer.parseInt((String) value);
                } catch (NumberFormatException var5) {
                }
            }

            return defaultValue;
        }
    }

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

    public static <V> Future<V> submit(Callable<V> task) {
        if (task == null) {
            throw new NullPointerException();
        } else {
            FutureTask<V> future = new FutureTask(task);
            execute(future);
            return future;
        }
    }

    public static <V> Future<V> submit(Runnable task, V result) {
        if (task == null) {
            throw new NullPointerException();
        } else {
            FutureTask<V> future = new FutureTask(task, result);
            execute(future);
            return future;
        }
    }

    private static void execute(Runnable command) {
        SwingUtilities.invokeLater(command);
    }

    public static void setSkipClickCount(Component comp, int count) {
        if (comp instanceof JTextComponent && ((JTextComponent) comp).getCaret() instanceof DefaultCaret) {
            ((JTextComponent) comp).putClientProperty(SKIP_CLICK_COUNT, count);
        }

    }

    public static int getAdjustedClickCount(JTextComponent comp, MouseEvent e) {
        int cc = e.getClickCount();
        if (cc == 1) {
            comp.putClientProperty(SKIP_CLICK_COUNT, null);
        } else {
            Integer sub = (Integer) comp.getClientProperty(SKIP_CLICK_COUNT);
            if (sub != null) {
                return cc - sub;
            }
        }

        return cc;
    }

    private static UIUtilities.Section liesIn(Rectangle rect, Point p, boolean horizontal, boolean ltr, boolean three) {
        int p0;
        int pComp;
        int length;
        boolean forward;
        if (horizontal) {
            p0 = rect.x;
            pComp = p.x;
            length = rect.width;
            forward = ltr;
        } else {
            p0 = rect.y;
            pComp = p.y;
            length = rect.height;
            forward = true;
        }

        int boundary;
        if (three) {
            boundary = length >= 30 ? 10 : length / 3;
            if (pComp < p0 + boundary) {
                return forward ? UIUtilities.Section.LEADING : UIUtilities.Section.TRAILING;
            } else if (pComp >= p0 + length - boundary) {
                return forward ? UIUtilities.Section.TRAILING : UIUtilities.Section.LEADING;
            } else {
                return UIUtilities.Section.MIDDLE;
            }
        } else {
            boundary = p0 + length / 2;
            if (forward) {
                return pComp >= boundary ? UIUtilities.Section.TRAILING : UIUtilities.Section.LEADING;
            } else {
                return pComp < boundary ? UIUtilities.Section.TRAILING : UIUtilities.Section.LEADING;
            }
        }
    }

    public static UIUtilities.Section liesInHorizontal(Rectangle rect, Point p, boolean ltr, boolean three) {
        return liesIn(rect, p, true, ltr, three);
    }

    public static UIUtilities.Section liesInVertical(Rectangle rect, Point p, boolean three) {
        return liesIn(rect, p, false, false, three);
    }

    public static int convertColumnIndexToModel(TableColumnModel cm, int viewColumnIndex) {
        return viewColumnIndex < 0 ? viewColumnIndex : cm.getColumn(viewColumnIndex).getModelIndex();
    }

    public static int convertColumnIndexToView(TableColumnModel cm, int modelColumnIndex) {
        if (modelColumnIndex < 0) {
            return modelColumnIndex;
        } else {
            for (int column = 0; column < cm.getColumnCount(); ++column) {
                if (cm.getColumn(column).getModelIndex() == modelColumnIndex) {
                    return column;
                }
            }

            return -1;
        }
    }

    public static TreePath getTreePath(TreeModelEvent event, TreeModel model) {
        TreePath path = event.getTreePath();
        if (path == null && model != null) {
            Object root = model.getRoot();
            if (root != null) {
                path = new TreePath(root);
            }
        }

        return path;
    }

    public static boolean isScaledGraphics(Graphics g) {
        if (g instanceof Graphics2D) {
            AffineTransform tx = ((Graphics2D) g).getTransform();
            return (tx.getType() & -66) != 0;
        } else {
            return false;
        }
    }

    public static boolean isFloatingPointScale(AffineTransform tx) {
        int type = tx.getType() & -66;
        if (type == 0) {
            return false;
        } else if ((type & -7) != 0) {
            return false;
        } else {
            double scaleX = tx.getScaleX();
            double scaleY = tx.getScaleY();
            return scaleX != (double) ((int) scaleX) || scaleY != (double) ((int) scaleY);
        }
    }

    public interface RepaintListener {
        void repaintPerformed(JComponent var1, int var2, int var3, int var4, int var5);
    }

    public enum Section {
        LEADING,
        MIDDLE,
        TRAILING;

        Section() {}
    }

    private static class LSBCacheEntry {
        private static final byte UNSET = 127;
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

            assert index >= 0 && index < 1;

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

        public boolean equals(Object entry) {
            if (entry == this) {
                return true;
            } else if (!(entry instanceof UIUtilities.LSBCacheEntry)) {
                return false;
            } else {
                UIUtilities.LSBCacheEntry oEntry = (UIUtilities.LSBCacheEntry) entry;
                return this.font.equals(oEntry.font) && this.frc.equals(oEntry.frc);
            }
        }

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

    public static class AATextInfo {
        Object aaHint;
        Integer lcdContrastHint;
        Map<Object, FontRenderContext> cache = new HashMap();


        public AATextInfo(Object aaHint, Integer lcdContrastHint) {
            if (aaHint == null) {
                throw new InternalError("null not allowed here");
            } else {
                this.aaHint = aaHint;
                this.lcdContrastHint = lcdContrastHint;
            }
        }

        FontRenderContext getFRC(AffineTransform tx) {
            if (tx == null && this.aaHint == null) {
                return null;
            } else {
                Object key = tx == null ? this.aaHint
                        : (this.aaHint == null ? tx : new UIUtilities.AATextInfo.KeyPair(tx, this.aaHint));
                FontRenderContext frc = this.cache.get(key);
                if (frc == null) {
                    this.aaHint = this.aaHint == null ? RenderingHints.VALUE_ANTIALIAS_OFF : this.aaHint;
                    frc = new FontRenderContext(tx, this.aaHint, RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT);
                    this.cache.put(key, frc);
                }

                return frc;
            }
        }

        private static class KeyPair {
            private final Object key1;
            private final Object key2;

            public KeyPair(Object key1, Object key2) {
                this.key1 = key1;
                this.key2 = key2;
            }

            public boolean equals(Object obj) {
                if (!(obj instanceof UIUtilities.AATextInfo.KeyPair)) {
                    return false;
                } else {
                    UIUtilities.AATextInfo.KeyPair that = (UIUtilities.AATextInfo.KeyPair) obj;
                    return this.key1.equals(that.key1) && this.key2.equals(that.key2);
                }
            }

            public int hashCode() {
                return this.key1.hashCode() + 37 * this.key2.hashCode();
            }
        }
    }
}
