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
package com.github.weisj.darklaf.bridge;

import com.github.weisj.darklaf.util.DarkSwingUtil;

import javax.swing.*;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

/**
 * Calculates preferred size and layouts menu items.
 * <p>
 * Mirror of MenuItemLayoutHelper.
 */
public class DarkMenuItemLayoutHelperBridge {

    /**
     * Client Property key for the text maximal offsets for BasicMenuItemUI
     */
    public static final UIClientPropertyKey BASICMENUITEMUI_MAX_TEXT_OFFSET =
        new DarkStringUIClientPropertyKey("maxTextOffset");

    /* Client Property keys for calculation of maximal widths */
    public static final UIClientPropertyKey MAX_ARROW_WIDTH = new DarkStringUIClientPropertyKey("maxArrowWidth");
    public static final UIClientPropertyKey MAX_CHECK_WIDTH = new DarkStringUIClientPropertyKey("maxCheckWidth");
    public static final UIClientPropertyKey MAX_ICON_WIDTH = new DarkStringUIClientPropertyKey("maxIconWidth");
    public static final UIClientPropertyKey MAX_TEXT_WIDTH = new DarkStringUIClientPropertyKey("maxTextWidth");
    public static final UIClientPropertyKey MAX_ACC_WIDTH = new DarkStringUIClientPropertyKey("maxAccWidth");
    public static final UIClientPropertyKey MAX_LABEL_WIDTH = new DarkStringUIClientPropertyKey("maxLabelWidth");

    private JMenuItem mi;
    private JComponent miParent;

    private Font font;
    private Font accFont;
    private FontMetrics fm;
    private FontMetrics accFm;

    private Icon icon;
    private Icon checkIcon;
    private Icon arrowIcon;
    private String text;
    private String accText;

    private boolean isColumnLayout;
    private boolean useCheckAndArrow;
    private boolean isLeftToRight;
    private boolean isTopLevelMenu;
    private View htmlView;

    private int verticalAlignment;
    private int horizontalAlignment;
    private int verticalTextPosition;
    private int horizontalTextPosition;
    private int gap;
    private int leadingGap;
    private int afterCheckIconGap;
    private int minTextOffset;

    private int leftTextExtraWidth;

    private Rectangle viewRect;

    private RectSize iconSize;
    private RectSize textSize;
    private RectSize accSize;
    private RectSize checkSize;
    private RectSize arrowSize;
    private RectSize labelSize;

    /**
     * The empty protected constructor is necessary for derived classes.
     */
    protected DarkMenuItemLayoutHelperBridge() {
    }

    public DarkMenuItemLayoutHelperBridge(final JMenuItem mi, final Icon checkIcon, final Icon arrowIcon,
                                          final Rectangle viewRect, final int gap, final String accDelimiter,
                                          final boolean isLeftToRight, final Font font, final Font accFont,
                                          final boolean useCheckAndArrow, final String propertyPrefix) {
        reset(mi, checkIcon, arrowIcon, viewRect, gap, accDelimiter,
              isLeftToRight, font, accFont, useCheckAndArrow, propertyPrefix);
    }

    public static boolean isColumnLayout(final boolean isLeftToRight,
                                         final JMenuItem mi) {
        assert (mi != null);
        return isColumnLayout(isLeftToRight, mi.getHorizontalAlignment(),
                              mi.getHorizontalTextPosition(), mi.getVerticalTextPosition());
    }

    /**
     * Answers should we do column layout for a menu item or not.
     * We do it when a user doesn't set any alignments
     * and text positions manually, except the vertical alignment.
     */
    public static boolean isColumnLayout(final boolean isLeftToRight,
                                         final int horizontalAlignment,
                                         final int horizontalTextPosition,
                                         final int verticalTextPosition) {
        if (verticalTextPosition != SwingConstants.CENTER) {
            return false;
        }
        if (isLeftToRight) {
            if (horizontalAlignment != SwingConstants.LEADING
                && horizontalAlignment != SwingConstants.LEFT) {
                return false;
            }
            return horizontalTextPosition == SwingConstants.TRAILING
                || horizontalTextPosition == SwingConstants.RIGHT;
        } else {
            if (horizontalAlignment != SwingConstants.LEADING
                && horizontalAlignment != SwingConstants.RIGHT) {
                return false;
            }
            return horizontalTextPosition == SwingConstants.TRAILING
                || horizontalTextPosition == SwingConstants.LEFT;
        }
    }

    /**
     * Returns parent of this component if it is not a top-level menu
     * Otherwise returns null.
     *
     * @param menuItem the menu item whose parent will be returned.
     * @return parent of this component if it is not a top-level menu
     * Otherwise returns null.
     */
    public static JComponent getMenuItemParent(final JMenuItem menuItem) {
        Container parent = menuItem.getParent();
        if ((parent instanceof JComponent) &&
            (!(menuItem instanceof JMenu) ||
                !((JMenu) menuItem).isTopLevelMenu())) {
            return (JComponent) parent;
        } else {
            return null;
        }
    }

    public static void clearUsedParentClientProperties(final JMenuItem menuItem) {
        clearUsedClientProperties(getMenuItemParent(menuItem));
    }

    public static void clearUsedClientProperties(final JComponent c) {
        if (c != null) {
            c.putClientProperty(MAX_ARROW_WIDTH, null);
            c.putClientProperty(MAX_CHECK_WIDTH, null);
            c.putClientProperty(MAX_ACC_WIDTH, null);
            c.putClientProperty(MAX_TEXT_WIDTH, null);
            c.putClientProperty(MAX_ICON_WIDTH, null);
            c.putClientProperty(MAX_LABEL_WIDTH, null);
            c.putClientProperty(BASICMENUITEMUI_MAX_TEXT_OFFSET, null);
        }
    }

    /**
     * Finds and returns maximal integer value in the given array.
     *
     * @param values array where the search will be performed.
     * @return maximal vaule.
     */
    public static int max(final int... values) {
        int maxValue = Integer.MIN_VALUE;
        for (int i : values) {
            if (i > maxValue) {
                maxValue = i;
            }
        }
        return maxValue;
    }

    public static Rectangle createMaxRect() {
        return new Rectangle(0, 0, Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    public static void addMaxWidth(final RectSize size, final int gap, final Dimension result) {
        if (size.maxWidth > 0) {
            result.width += size.maxWidth + gap;
        }
    }

    public static void addWidth(final int width, final int gap, final Dimension result) {
        if (width > 0) {
            result.width += width + gap;
        }
    }

    /**
     * Returns false if the component is a JMenu and it is a top
     * level menu (on the menubar).
     */
    public static boolean useCheckAndArrow(final JMenuItem menuItem) {
        boolean b = true;
        if ((menuItem instanceof JMenu) &&
            (((JMenu) menuItem).isTopLevelMenu())) {
            b = false;
        }
        return b;
    }

    protected void reset(final JMenuItem mi, final Icon checkIcon, final Icon arrowIcon,
                         final Rectangle viewRect, final int gap, final String accDelimiter,
                         final boolean isLeftToRight, final Font font, final Font accFont,
                         final boolean useCheckAndArrow, final String propertyPrefix) {
        this.mi = mi;
        this.miParent = getMenuItemParent(mi);
        this.accText = getAccText(accDelimiter);
        this.verticalAlignment = mi.getVerticalAlignment();
        this.horizontalAlignment = mi.getHorizontalAlignment();
        this.verticalTextPosition = mi.getVerticalTextPosition();
        this.horizontalTextPosition = mi.getHorizontalTextPosition();
        this.useCheckAndArrow = useCheckAndArrow;
        this.font = font;
        this.accFont = accFont;
        this.fm = mi.getFontMetrics(font);
        this.accFm = mi.getFontMetrics(accFont);
        this.isLeftToRight = isLeftToRight;
        this.isColumnLayout = isColumnLayout(isLeftToRight,
                                             horizontalAlignment, horizontalTextPosition,
                                             verticalTextPosition);
        this.isTopLevelMenu = this.miParent == null;
        this.checkIcon = checkIcon;
        this.icon = getIcon(propertyPrefix);
        this.arrowIcon = arrowIcon;
        this.text = mi.getText();
        this.gap = gap;
        this.afterCheckIconGap = getAfterCheckIconGap(propertyPrefix);
        this.minTextOffset = getMinTextOffset(propertyPrefix);
        this.htmlView = (View) mi.getClientProperty(BasicHTML.propertyKey);
        this.viewRect = viewRect;

        this.iconSize = new RectSize();
        this.textSize = new RectSize();
        this.accSize = new RectSize();
        this.checkSize = new RectSize();
        this.arrowSize = new RectSize();
        this.labelSize = new RectSize();
        calcExtraWidths();
        calcWidthsAndHeights();
        setOriginalWidths();
        calcMaxWidths();

        this.leadingGap = getLeadingGap(propertyPrefix);
        calcMaxTextOffset(viewRect);
    }

    private void calcExtraWidths() {
        leftTextExtraWidth = getLeftExtraWidth(text);
    }

    private int getLeftExtraWidth(final String str) {
        int lsb = DarkSwingUtil.getLeftSideBearing(mi, fm, str);
        if (lsb < 0) {
            return -lsb;
        } else {
            return 0;
        }
    }

    private void setOriginalWidths() {
        iconSize.origWidth = iconSize.width;
        textSize.origWidth = textSize.width;
        accSize.origWidth = accSize.width;
        checkSize.origWidth = checkSize.width;
        arrowSize.origWidth = arrowSize.width;
    }

    @SuppressWarnings("deprecation")
    private String getAccText(final String acceleratorDelimiter) {
        String accText = "";
        KeyStroke accelerator = mi.getAccelerator();
        if (accelerator != null) {
            int modifiers = accelerator.getModifiers();
            if (modifiers > 0) {
                accText = KeyEvent.getKeyModifiersText(modifiers);
                accText += acceleratorDelimiter;
            }
            int keyCode = accelerator.getKeyCode();
            if (keyCode != 0) {
                accText += KeyEvent.getKeyText(keyCode);
            } else {
                accText += accelerator.getKeyChar();
            }
        }
        return accText;
    }

    private Icon getIcon(final String propertyPrefix) {
        // In case of column layout, .checkIconFactory is defined for this UI,
        // the icon is compatible with it and useCheckAndArrow() is true,
        // then the icon is handled by the checkIcon.
//        Icon icon = null;
//        MenuItemCheckIconFactory iconFactory =
//            (MenuItemCheckIconFactory) UIManager.get(propertyPrefix
//                                                         + ".checkIconFactory");
//        if (!isColumnLayout || !useCheckAndArrow || iconFactory == null
//            || !iconFactory.isCompatible(checkIcon, propertyPrefix)) {
//            icon = mi.getIcon();
//        }
//        return icon;
        return null;
    }

    private int getMinTextOffset(final String propertyPrefix) {
        int minimumTextOffset = 0;
        Object minimumTextOffsetObject =
            UIManager.get(propertyPrefix + ".minimumTextOffset");
        if (minimumTextOffsetObject instanceof Integer) {
            minimumTextOffset = (Integer) minimumTextOffsetObject;
        }
        return minimumTextOffset;
    }

    private int getAfterCheckIconGap(final String propertyPrefix) {
        int afterCheckIconGap = gap;
        Object afterCheckIconGapObject =
            UIManager.get(propertyPrefix + ".afterCheckIconGap");
        if (afterCheckIconGapObject instanceof Integer) {
            afterCheckIconGap = (Integer) afterCheckIconGapObject;
        }
        return afterCheckIconGap;
    }

    private int getLeadingGap(final String propertyPrefix) {
        if (checkSize.getMaxWidth() > 0) {
            return getCheckOffset(propertyPrefix);
        } else {
            return gap; // There is no any check icon
        }
    }

    private int getCheckOffset(final String propertyPrefix) {
        int checkIconOffset = gap;
        Object checkIconOffsetObject =
            UIManager.get(propertyPrefix + ".checkIconOffset");
        if (checkIconOffsetObject instanceof Integer) {
            checkIconOffset = (Integer) checkIconOffsetObject;
        }
        return checkIconOffset;
    }

    protected void calcWidthsAndHeights() {
        // iconRect
        if (icon != null) {
            iconSize.width = icon.getIconWidth();
            iconSize.height = icon.getIconHeight();
        }

        // accRect
        if (!accText.equals("")) {
            accSize.width = DarkSwingUtil.stringWidth(mi, accFm, accText);
            accSize.height = accFm.getHeight();
        }

        // textRect
        if (text == null) {
            text = "";
        } else if (!text.equals("")) {
            if (htmlView != null) {
                // Text is HTML
                textSize.width =
                    (int) htmlView.getPreferredSpan(View.X_AXIS);
                textSize.height =
                    (int) htmlView.getPreferredSpan(View.Y_AXIS);
            } else {
                // Text isn't HTML
                textSize.width = DarkSwingUtil.stringWidth(mi, fm, text);
                textSize.height = fm.getHeight();
            }
        }

        if (useCheckAndArrow) {
            // checkIcon
            if (checkIcon != null) {
                checkSize.width = checkIcon.getIconWidth();
                checkSize.height = checkIcon.getIconHeight();
            }
            // arrowRect
            if (arrowIcon != null) {
                arrowSize.width = arrowIcon.getIconWidth();
                arrowSize.height = arrowIcon.getIconHeight();
            }
        }

        // labelRect
        if (isColumnLayout) {
            labelSize.width = iconSize.width + textSize.width + gap;
            labelSize.height = max(checkSize.height, iconSize.height,
                                   textSize.height, accSize.height, arrowSize.height);
        } else {
            Rectangle textRect = new Rectangle();
            Rectangle iconRect = new Rectangle();
            SwingUtilities.layoutCompoundLabel(mi, fm, text, icon,
                                               verticalAlignment, horizontalAlignment,
                                               verticalTextPosition, horizontalTextPosition,
                                               viewRect, iconRect, textRect, gap);
            textRect.width += leftTextExtraWidth;
            Rectangle labelRect = iconRect.union(textRect);
            labelSize.height = labelRect.height;
            labelSize.width = labelRect.width;
        }
    }

    protected void calcMaxWidths() {
        calcMaxWidth(checkSize, MAX_CHECK_WIDTH);
        calcMaxWidth(arrowSize, MAX_ARROW_WIDTH);
        calcMaxWidth(accSize, MAX_ACC_WIDTH);

        if (isColumnLayout) {
            calcMaxWidth(iconSize, MAX_ICON_WIDTH);
            calcMaxWidth(textSize, MAX_TEXT_WIDTH);
            int curGap = gap;
            if ((iconSize.getMaxWidth() == 0)
                || (textSize.getMaxWidth() == 0)) {
                curGap = 0;
            }
            labelSize.maxWidth =
                calcMaxValue(MAX_LABEL_WIDTH, iconSize.maxWidth
                    + textSize.maxWidth + curGap);
        } else {
            // We shouldn't use current icon and text widths
            // in maximal widths calculation for complex layout.
            iconSize.maxWidth = getParentIntProperty(MAX_ICON_WIDTH);
            calcMaxWidth(labelSize, MAX_LABEL_WIDTH);
            // If maxLabelWidth is wider
            // than the widest icon + the widest text + gap,
            // we should update the maximal text witdh
            int candidateTextWidth = labelSize.maxWidth - iconSize.maxWidth;
            if (iconSize.maxWidth > 0) {
                candidateTextWidth -= gap;
            }
            textSize.maxWidth = calcMaxValue(MAX_TEXT_WIDTH, candidateTextWidth);
        }
    }

    protected void calcMaxWidth(final RectSize rs, final Object key) {
        rs.maxWidth = calcMaxValue(key, rs.width);
    }

    /**
     * Calculates and returns maximal value through specified parent component
     * client property.
     *
     * @param propertyName name of the property, which stores the maximal value.
     * @param value        a value which pretends to be maximal
     * @return maximal value among the parent property and the value.
     */
    protected int calcMaxValue(final Object propertyName, final int value) {
        // Get maximal value from parent client property
        int maxValue = getParentIntProperty(propertyName);
        // Store new maximal width in parent client property
        if (value > maxValue) {
            if (miParent != null) {
                miParent.putClientProperty(propertyName, value);
            }
            return value;
        } else {
            return maxValue;
        }
    }

    /**
     * Returns parent client property as int.
     *
     * @param propertyName name of the parent property.
     * @return value of the property as int.
     */
    protected int getParentIntProperty(final Object propertyName) {
        Object value = null;
        if (miParent != null) {
            value = miParent.getClientProperty(propertyName);
        }
        if ((value == null) || !(value instanceof Integer)) {
            value = 0;
        }
        return (Integer) value;
    }

    /**
     * Calculates maximal text offset.
     * It is required for some L&Fs (ex: Vista L&F).
     * The offset is meaningful only for L2R column layout.
     *
     * @param viewRect the rectangle, the maximal text offset
     *                 will be calculated for.
     */
    private void calcMaxTextOffset(final Rectangle viewRect) {
        if (!isColumnLayout || !isLeftToRight) {
            return;
        }

        // Calculate the current text offset
        int offset = viewRect.x + leadingGap + checkSize.maxWidth
            + afterCheckIconGap + iconSize.maxWidth + gap;
        if (checkSize.maxWidth == 0) {
            offset -= afterCheckIconGap;
        }
        if (iconSize.maxWidth == 0) {
            offset -= gap;
        }

        // maximal text offset shouldn't be less than minimal text offset;
        if (offset < minTextOffset) {
            offset = minTextOffset;
        }

        // Calculate and store the maximal text offset
        calcMaxValue(BASICMENUITEMUI_MAX_TEXT_OFFSET, offset);
    }

    /**
     * Layout icon, text, check icon, accelerator text and arrow icon
     * in the viewRect and return their positions.
     * <p>
     * If horizontalAlignment, verticalTextPosition and horizontalTextPosition
     * are default (user doesn't set any manually) the layouting algorithm is:
     * Elements are layouted in the five columns:
     * check icon + icon + text + accelerator text + arrow icon
     * <p>
     * In the other case elements are layouted in the four columns:
     * check icon + label + accelerator text + arrow icon
     * Label is union of icon and text.
     * <p>
     * The order of columns can be reversed.
     * It depends on the menu item orientation.
     */
    public LayoutResult layoutMenuItem() {
        LayoutResult lr = createLayoutResult();
        prepareForLayout(lr);

        if (isColumnLayout()) {
            if (isLeftToRight()) {
                doLTRColumnLayout(lr, getLTRColumnAlignment());
            } else {
                doRTLColumnLayout(lr, getRTLColumnAlignment());
            }
        } else {
            if (isLeftToRight()) {
                doLTRComplexLayout(lr, getLTRColumnAlignment());
            } else {
                doRTLComplexLayout(lr, getRTLColumnAlignment());
            }
        }

        alignAccCheckAndArrowVertically(lr);
        return lr;
    }

    private LayoutResult createLayoutResult() {
        return new LayoutResult(
            new Rectangle(iconSize.width, iconSize.height),
            new Rectangle(textSize.width, textSize.height),
            new Rectangle(accSize.width, accSize.height),
            new Rectangle(checkSize.width, checkSize.height),
            new Rectangle(arrowSize.width, arrowSize.height),
            new Rectangle(labelSize.width, labelSize.height)
        );
    }

    public ColumnAlignment getLTRColumnAlignment() {
        return ColumnAlignment.LEFT_ALIGNMENT;
    }

    public ColumnAlignment getRTLColumnAlignment() {
        return ColumnAlignment.RIGHT_ALIGNMENT;
    }

    protected void prepareForLayout(final LayoutResult lr) {
        lr.checkRect.width = checkSize.maxWidth;
        lr.accRect.width = accSize.maxWidth;
        lr.arrowRect.width = arrowSize.maxWidth;
    }

    /**
     * Aligns the accelertor text and the check and arrow icons vertically
     * with the center of the label rect.
     */
    private void alignAccCheckAndArrowVertically(final LayoutResult lr) {
        lr.accRect.y = (int) (lr.labelRect.y
            + (float) lr.labelRect.height / 2
            - (float) lr.accRect.height / 2);
        fixVerticalAlignment(lr, lr.accRect);
        if (useCheckAndArrow) {
            lr.arrowRect.y = (int) (lr.labelRect.y
                + (float) lr.labelRect.height / 2
                - (float) lr.arrowRect.height / 2);
            lr.checkRect.y = (int) (lr.labelRect.y
                + (float) lr.labelRect.height / 2
                - (float) lr.checkRect.height / 2);
            fixVerticalAlignment(lr, lr.arrowRect);
            fixVerticalAlignment(lr, lr.checkRect);
        }
    }

    /**
     * Fixes vertical alignment of all menu item elements if rect.y
     * or (rect.y + rect.height) is out of viewRect bounds
     */
    private void fixVerticalAlignment(final LayoutResult lr, final Rectangle r) {
        int delta = 0;
        if (r.y < viewRect.y) {
            delta = viewRect.y - r.y;
        } else if (r.y + r.height > viewRect.y + viewRect.height) {
            delta = viewRect.y + viewRect.height - r.y - r.height;
        }
        if (delta != 0) {
            lr.checkRect.y += delta;
            lr.iconRect.y += delta;
            lr.textRect.y += delta;
            lr.accRect.y += delta;
            lr.arrowRect.y += delta;
            lr.labelRect.y += delta;
        }
    }

    private void doLTRColumnLayout(final LayoutResult lr, final ColumnAlignment alignment) {
        // Set maximal width for all the five basic rects
        // (three other ones are already maximal)
        lr.iconRect.width = iconSize.maxWidth;
        lr.textRect.width = textSize.maxWidth;

        // Set X coordinates
        // All rects will be aligned at the left side
        calcXPositionsLTR(viewRect.x, leadingGap, gap, lr.checkRect,
                          lr.iconRect, lr.textRect);

        // Tune afterCheckIconGap
        if (lr.checkRect.width > 0) { // there is the afterCheckIconGap
            lr.iconRect.x += afterCheckIconGap - gap;
            lr.textRect.x += afterCheckIconGap - gap;
        }

        calcXPositionsRTL(viewRect.x + viewRect.width, leadingGap, gap,
                          lr.arrowRect, lr.accRect);

        // Take into account minimal text offset
        int textOffset = lr.textRect.x - viewRect.x;
        if (!isTopLevelMenu && (textOffset < minTextOffset)) {
            lr.textRect.x += minTextOffset - textOffset;
        }

        alignRects(lr, alignment);

        // Set Y coordinate for text and icon.
        // Y coordinates for other rects
        // will be calculated later in layoutMenuItem.
        calcTextAndIconYPositions(lr);

        // Calculate valid X and Y coordinates for labelRect
        lr.setLabelRect(lr.textRect.union(lr.iconRect));
    }

    private void doLTRComplexLayout(final LayoutResult lr, final ColumnAlignment alignment) {
        lr.labelRect.width = labelSize.maxWidth;

        // Set X coordinates
        calcXPositionsLTR(viewRect.x, leadingGap, gap, lr.checkRect,
                          lr.labelRect);

        // Tune afterCheckIconGap
        if (lr.checkRect.width > 0) { // there is the afterCheckIconGap
            lr.labelRect.x += afterCheckIconGap - gap;
        }

        calcXPositionsRTL(viewRect.x + viewRect.width,
                          leadingGap, gap, lr.arrowRect, lr.accRect);

        // Take into account minimal text offset
        int labelOffset = lr.labelRect.x - viewRect.x;
        if (!isTopLevelMenu && (labelOffset < minTextOffset)) {
            lr.labelRect.x += minTextOffset - labelOffset;
        }

        alignRects(lr, alignment);

        // Center labelRect vertically
        calcLabelYPosition(lr);

        layoutIconAndTextInLabelRect(lr);
    }

    private void doRTLColumnLayout(final LayoutResult lr, final ColumnAlignment alignment) {
        // Set maximal width for all the five basic rects
        // (three other ones are already maximal)
        lr.iconRect.width = iconSize.maxWidth;
        lr.textRect.width = textSize.maxWidth;

        // Set X coordinates
        calcXPositionsRTL(viewRect.x + viewRect.width, leadingGap, gap,
                          lr.checkRect, lr.iconRect, lr.textRect);

        // Tune the gap after check icon
        if (lr.checkRect.width > 0) { // there is the gap after check icon
            lr.iconRect.x -= afterCheckIconGap - gap;
            lr.textRect.x -= afterCheckIconGap - gap;
        }

        calcXPositionsLTR(viewRect.x, leadingGap, gap, lr.arrowRect,
                          lr.accRect);

        // Take into account minimal text offset
        int textOffset = (viewRect.x + viewRect.width)
            - (lr.textRect.x + lr.textRect.width);
        if (!isTopLevelMenu && (textOffset < minTextOffset)) {
            lr.textRect.x -= minTextOffset - textOffset;
        }

        alignRects(lr, alignment);

        // Set Y coordinates for text and icon.
        // Y coordinates for other rects
        // will be calculated later in layoutMenuItem.
        calcTextAndIconYPositions(lr);

        // Calculate valid X and Y coordinate for labelRect
        lr.setLabelRect(lr.textRect.union(lr.iconRect));
    }

    private void doRTLComplexLayout(final LayoutResult lr, final ColumnAlignment alignment) {
        lr.labelRect.width = labelSize.maxWidth;

        // Set X coordinates
        calcXPositionsRTL(viewRect.x + viewRect.width, leadingGap, gap,
                          lr.checkRect, lr.labelRect);

        // Tune the gap after check icon
        if (lr.checkRect.width > 0) { // there is the gap after check icon
            lr.labelRect.x -= afterCheckIconGap - gap;
        }

        calcXPositionsLTR(viewRect.x, leadingGap, gap, lr.arrowRect, lr.accRect);

        // Take into account minimal text offset
        int labelOffset = (viewRect.x + viewRect.width)
            - (lr.labelRect.x + lr.labelRect.width);
        if (!isTopLevelMenu && (labelOffset < minTextOffset)) {
            lr.labelRect.x -= minTextOffset - labelOffset;
        }

        alignRects(lr, alignment);

        // Center labelRect vertically
        calcLabelYPosition(lr);

        layoutIconAndTextInLabelRect(lr);
    }

    private void alignRects(final LayoutResult lr, final ColumnAlignment alignment) {
        alignRect(lr.checkRect, alignment.getCheckAlignment(),
                  checkSize.getOrigWidth());
        alignRect(lr.iconRect, alignment.getIconAlignment(),
                  iconSize.getOrigWidth());
        alignRect(lr.textRect, alignment.getTextAlignment(),
                  textSize.getOrigWidth());
        alignRect(lr.accRect, alignment.getAccAlignment(),
                  accSize.getOrigWidth());
        alignRect(lr.arrowRect, alignment.getArrowAlignment(),
                  arrowSize.getOrigWidth());
    }

    private void alignRect(final Rectangle rect, final int alignment, final int origWidth) {
        if (alignment == SwingConstants.RIGHT) {
            rect.x = rect.x + rect.width - origWidth;
        }
        rect.width = origWidth;
    }

    protected void layoutIconAndTextInLabelRect(final LayoutResult lr) {
        lr.setTextRect(new Rectangle());
        lr.setIconRect(new Rectangle());
        SwingUtilities.layoutCompoundLabel(
            mi, fm, text, icon, verticalAlignment, horizontalAlignment,
            verticalTextPosition, horizontalTextPosition, lr.labelRect,
            lr.iconRect, lr.textRect, gap);
    }

    private void calcXPositionsLTR(final int startXPos, final int leadingGap,
                                   final int gap, final Rectangle... rects) {
        int curXPos = startXPos + leadingGap;
        for (Rectangle rect : rects) {
            rect.x = curXPos;
            if (rect.width > 0) {
                curXPos += rect.width + gap;
            }
        }
    }

    private void calcXPositionsRTL(final int startXPos, final int leadingGap,
                                   final int gap, final Rectangle... rects) {
        int curXPos = startXPos - leadingGap;
        for (Rectangle rect : rects) {
            rect.x = curXPos - rect.width;
            if (rect.width > 0) {
                curXPos -= rect.width + gap;
            }
        }
    }

    /**
     * Sets Y coordinates of text and icon
     * taking into account the vertical alignment
     */
    private void calcTextAndIconYPositions(final LayoutResult lr) {
        if (verticalAlignment == SwingUtilities.TOP) {
            lr.textRect.y = (int) (viewRect.y
                + (float) lr.labelRect.height / 2
                - (float) lr.textRect.height / 2);
            lr.iconRect.y = (int) (viewRect.y
                + (float) lr.labelRect.height / 2
                - (float) lr.iconRect.height / 2);
        } else if (verticalAlignment == SwingUtilities.CENTER) {
            lr.textRect.y = (int) (viewRect.y
                + (float) viewRect.height / 2
                - (float) lr.textRect.height / 2);
            lr.iconRect.y = (int) (viewRect.y
                + (float) viewRect.height / 2
                - (float) lr.iconRect.height / 2);
        } else if (verticalAlignment == SwingUtilities.BOTTOM) {
            lr.textRect.y = (int) (viewRect.y
                + viewRect.height
                - (float) lr.labelRect.height / 2
                - (float) lr.textRect.height / 2);
            lr.iconRect.y = (int) (viewRect.y
                + viewRect.height
                - (float) lr.labelRect.height / 2
                - (float) lr.iconRect.height / 2);
        }
    }

    /**
     * Sets labelRect Y coordinate
     * taking into account the vertical alignment
     */
    private void calcLabelYPosition(final LayoutResult lr) {
        if (verticalAlignment == SwingUtilities.TOP) {
            lr.labelRect.y = viewRect.y;
        } else if (verticalAlignment == SwingUtilities.CENTER) {
            lr.labelRect.y = (int) (viewRect.y
                + (float) viewRect.height / 2
                - (float) lr.labelRect.height / 2);
        } else if (verticalAlignment == SwingUtilities.BOTTOM) {
            lr.labelRect.y = viewRect.y + viewRect.height
                - lr.labelRect.height;
        }
    }

    public JMenuItem getMenuItem() {
        return mi;
    }

    protected void setMenuItem(final JMenuItem mi) {
        this.mi = mi;
    }

    public JComponent getMenuItemParent() {
        return miParent;
    }

    protected void setMenuItemParent(final JComponent miParent) {
        this.miParent = miParent;
    }

    public Font getFont() {
        return font;
    }

    protected void setFont(final Font font) {
        this.font = font;
    }

    public Font getAccFont() {
        return accFont;
    }

    protected void setAccFont(final Font accFont) {
        this.accFont = accFont;
    }

    public FontMetrics getFontMetrics() {
        return fm;
    }

    protected void setFontMetrics(final FontMetrics fm) {
        this.fm = fm;
    }

    public FontMetrics getAccFontMetrics() {
        return accFm;
    }

    protected void setAccFontMetrics(final FontMetrics accFm) {
        this.accFm = accFm;
    }

    public Icon getIcon() {
        return icon;
    }

    protected void setIcon(final Icon icon) {
        this.icon = icon;
    }

    public Icon getCheckIcon() {
        return checkIcon;
    }

    protected void setCheckIcon(final Icon checkIcon) {
        this.checkIcon = checkIcon;
    }

    public Icon getArrowIcon() {
        return arrowIcon;
    }

    protected void setArrowIcon(final Icon arrowIcon) {
        this.arrowIcon = arrowIcon;
    }

    public String getText() {
        return text;
    }

    protected void setText(final String text) {
        this.text = text;
    }

    public String getAccText() {
        return accText;
    }

    protected void setAccText(final String accText) {
        this.accText = accText;
    }

    public boolean isColumnLayout() {
        return isColumnLayout;
    }

    protected void setColumnLayout(final boolean columnLayout) {
        isColumnLayout = columnLayout;
    }

    public boolean useCheckAndArrow() {
        return useCheckAndArrow;
    }

    public boolean isLeftToRight() {
        return isLeftToRight;
    }

    protected void setLeftToRight(final boolean leftToRight) {
        isLeftToRight = leftToRight;
    }

    public boolean isTopLevelMenu() {
        return isTopLevelMenu;
    }

    protected void setTopLevelMenu(final boolean topLevelMenu) {
        isTopLevelMenu = topLevelMenu;
    }

    public View getHtmlView() {
        return htmlView;
    }

    protected void setHtmlView(final View htmlView) {
        this.htmlView = htmlView;
    }

    public int getVerticalAlignment() {
        return verticalAlignment;
    }

    protected void setVerticalAlignment(final int verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    public int getHorizontalAlignment() {
        return horizontalAlignment;
    }

    protected void setHorizontalAlignment(final int horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    public int getVerticalTextPosition() {
        return verticalTextPosition;
    }

    protected void setVerticalTextPosition(final int verticalTextPosition) {
        this.verticalTextPosition = verticalTextPosition;
    }

    public int getHorizontalTextPosition() {
        return horizontalTextPosition;
    }

    protected void setHorizontalTextPosition(final int horizontalTextPosition) {
        this.horizontalTextPosition = horizontalTextPosition;
    }

    public int getGap() {
        return gap;
    }

    protected void setGap(final int gap) {
        this.gap = gap;
    }

    public int getLeadingGap() {
        return leadingGap;
    }

    protected void setLeadingGap(final int leadingGap) {
        this.leadingGap = leadingGap;
    }

    public int getAfterCheckIconGap() {
        return afterCheckIconGap;
    }

    protected void setAfterCheckIconGap(final int afterCheckIconGap) {
        this.afterCheckIconGap = afterCheckIconGap;
    }

    public int getMinTextOffset() {
        return minTextOffset;
    }

    protected void setMinTextOffset(final int minTextOffset) {
        this.minTextOffset = minTextOffset;
    }

    public Rectangle getViewRect() {
        return viewRect;
    }

    protected void setViewRect(final Rectangle viewRect) {
        this.viewRect = viewRect;
    }

    public RectSize getIconSize() {
        return iconSize;
    }

    protected void setIconSize(final RectSize iconSize) {
        this.iconSize = iconSize;
    }

    public RectSize getTextSize() {
        return textSize;
    }

    protected void setTextSize(final RectSize textSize) {
        this.textSize = textSize;
    }

    public RectSize getAccSize() {
        return accSize;
    }

    protected void setAccSize(final RectSize accSize) {
        this.accSize = accSize;
    }

    public RectSize getCheckSize() {
        return checkSize;
    }

    protected void setCheckSize(final RectSize checkSize) {
        this.checkSize = checkSize;
    }

    public RectSize getArrowSize() {
        return arrowSize;
    }

    protected void setArrowSize(final RectSize arrowSize) {
        this.arrowSize = arrowSize;
    }

    public RectSize getLabelSize() {
        return labelSize;
    }

    protected void setLabelSize(final RectSize labelSize) {
        this.labelSize = labelSize;
    }

    protected void setUseCheckAndArrow(final boolean useCheckAndArrow) {
        this.useCheckAndArrow = useCheckAndArrow;
    }

    public int getLeftTextExtraWidth() {
        return leftTextExtraWidth;
    }

    public static class LayoutResult {
        private Rectangle iconRect;
        private Rectangle textRect;
        private Rectangle accRect;
        private Rectangle checkRect;
        private Rectangle arrowRect;
        private Rectangle labelRect;

        public LayoutResult() {
            iconRect = new Rectangle();
            textRect = new Rectangle();
            accRect = new Rectangle();
            checkRect = new Rectangle();
            arrowRect = new Rectangle();
            labelRect = new Rectangle();
        }

        public LayoutResult(final Rectangle iconRect, final Rectangle textRect,
                            final Rectangle accRect, final Rectangle checkRect,
                            final Rectangle arrowRect, final Rectangle labelRect) {
            this.iconRect = iconRect;
            this.textRect = textRect;
            this.accRect = accRect;
            this.checkRect = checkRect;
            this.arrowRect = arrowRect;
            this.labelRect = labelRect;
        }

        public Rectangle getIconRect() {
            return iconRect;
        }

        public void setIconRect(final Rectangle iconRect) {
            this.iconRect = iconRect;
        }

        public Rectangle getTextRect() {
            return textRect;
        }

        public void setTextRect(final Rectangle textRect) {
            this.textRect = textRect;
        }

        public Rectangle getAccRect() {
            return accRect;
        }

        public void setAccRect(final Rectangle accRect) {
            this.accRect = accRect;
        }

        public Rectangle getCheckRect() {
            return checkRect;
        }

        public void setCheckRect(final Rectangle checkRect) {
            this.checkRect = checkRect;
        }

        public Rectangle getArrowRect() {
            return arrowRect;
        }

        public void setArrowRect(final Rectangle arrowRect) {
            this.arrowRect = arrowRect;
        }

        public Rectangle getLabelRect() {
            return labelRect;
        }

        public void setLabelRect(final Rectangle labelRect) {
            this.labelRect = labelRect;
        }

        public Map<String, Rectangle> getAllRects() {
            Map<String, Rectangle> result = new HashMap<String, Rectangle>();
            result.put("checkRect", checkRect);
            result.put("iconRect", iconRect);
            result.put("textRect", textRect);
            result.put("accRect", accRect);
            result.put("arrowRect", arrowRect);
            result.put("labelRect", labelRect);
            return result;
        }
    }

    public static class ColumnAlignment {
        public static final ColumnAlignment LEFT_ALIGNMENT =
            new ColumnAlignment(
                SwingConstants.LEFT,
                SwingConstants.LEFT,
                SwingConstants.LEFT,
                SwingConstants.LEFT,
                SwingConstants.LEFT
            );
        public static final ColumnAlignment RIGHT_ALIGNMENT =
            new ColumnAlignment(
                SwingConstants.RIGHT,
                SwingConstants.RIGHT,
                SwingConstants.RIGHT,
                SwingConstants.RIGHT,
                SwingConstants.RIGHT
            );
        private int checkAlignment;
        private int iconAlignment;
        private int textAlignment;
        private int accAlignment;
        private int arrowAlignment;

        public ColumnAlignment(final int checkAlignment, final int iconAlignment,
                               final int textAlignment, final int accAlignment,
                               final int arrowAlignment) {
            this.checkAlignment = checkAlignment;
            this.iconAlignment = iconAlignment;
            this.textAlignment = textAlignment;
            this.accAlignment = accAlignment;
            this.arrowAlignment = arrowAlignment;
        }

        public int getCheckAlignment() {
            return checkAlignment;
        }

        public int getIconAlignment() {
            return iconAlignment;
        }

        public int getTextAlignment() {
            return textAlignment;
        }

        public int getAccAlignment() {
            return accAlignment;
        }

        public int getArrowAlignment() {
            return arrowAlignment;
        }
    }

    public static class RectSize {
        private int width;
        private int height;
        private int origWidth;
        private int maxWidth;

        public RectSize() {
        }

        public RectSize(final int width, final int height, final int origWidth, final int maxWidth) {
            this.width = width;
            this.height = height;
            this.origWidth = origWidth;
            this.maxWidth = maxWidth;
        }

        public int getWidth() {
            return width;
        }

        public void setWidth(final int width) {
            this.width = width;
        }

        public int getHeight() {
            return height;
        }

        public void setHeight(final int height) {
            this.height = height;
        }

        public int getOrigWidth() {
            return origWidth;
        }

        public void setOrigWidth(final int origWidth) {
            this.origWidth = origWidth;
        }

        public int getMaxWidth() {
            return maxWidth;
        }

        public void setMaxWidth(final int maxWidth) {
            this.maxWidth = maxWidth;
        }

        public String toString() {
            return "[w=" + width + ",h=" + height + ",ow="
                + origWidth + ",mw=" + maxWidth + "]";
        }
    }
}
