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

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package com.intellij.util.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.View;

import com.github.weisj.darklaf.util.Types;

@SuppressWarnings("unused")
public class MenuItemLayoutHelper {
    public static final StringUIClientPropertyKey MAX_ARROW_WIDTH = new StringUIClientPropertyKey("maxArrowWidth");
    public static final StringUIClientPropertyKey MAX_CHECK_WIDTH = new StringUIClientPropertyKey("maxCheckWidth");
    public static final StringUIClientPropertyKey MAX_ICON_WIDTH = new StringUIClientPropertyKey("maxIconWidth");
    public static final StringUIClientPropertyKey MAX_TEXT_WIDTH = new StringUIClientPropertyKey("maxTextWidth");
    public static final StringUIClientPropertyKey MAX_ACC_WIDTH = new StringUIClientPropertyKey("maxAccWidth");
    public static final StringUIClientPropertyKey MAX_LABEL_WIDTH = new StringUIClientPropertyKey("maxLabelWidth");
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
    private MenuItemLayoutHelper.RectSize iconSize;
    private MenuItemLayoutHelper.RectSize textSize;
    private MenuItemLayoutHelper.RectSize accSize;
    private MenuItemLayoutHelper.RectSize checkSize;
    private MenuItemLayoutHelper.RectSize arrowSize;
    private MenuItemLayoutHelper.RectSize labelSize;

    protected MenuItemLayoutHelper() {}

    public MenuItemLayoutHelper(JMenuItem mi, Icon checkIcon, Icon arrowIcon, Rectangle viewRect, int gap,
            String accDelimiter, boolean isLeftToRight, Font font, Font accFont, boolean useCheckAndArrow,
            String propertyPrefix) {
        this.reset(mi, checkIcon, arrowIcon, viewRect, gap, accDelimiter, isLeftToRight, font, accFont,
                useCheckAndArrow, propertyPrefix);
    }

    protected void reset(JMenuItem mi, Icon checkIcon, Icon arrowIcon, Rectangle viewRect, int gap, String accDelimiter,
            boolean isLeftToRight, Font font, Font accFont, boolean useCheckAndArrow, String propertyPrefix) {
        this.mi = mi;
        this.miParent = getMenuItemParent(mi);
        this.accText = this.getAccText(accDelimiter);
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
        this.isColumnLayout = isColumnLayout(isLeftToRight, this.horizontalAlignment, this.horizontalTextPosition,
                this.verticalTextPosition);
        this.isTopLevelMenu = this.miParent == null;
        this.checkIcon = checkIcon;
        this.icon = this.getIcon(propertyPrefix);
        this.arrowIcon = arrowIcon;
        this.text = mi.getText();
        this.gap = gap;
        this.afterCheckIconGap = this.getAfterCheckIconGap(propertyPrefix);
        this.minTextOffset = this.getMinTextOffset(propertyPrefix);
        this.htmlView = (View) mi.getClientProperty("html");
        this.viewRect = viewRect;
        this.iconSize = new MenuItemLayoutHelper.RectSize();
        this.textSize = new MenuItemLayoutHelper.RectSize();
        this.accSize = new MenuItemLayoutHelper.RectSize();
        this.checkSize = new MenuItemLayoutHelper.RectSize();
        this.arrowSize = new MenuItemLayoutHelper.RectSize();
        this.labelSize = new MenuItemLayoutHelper.RectSize();
        this.calcExtraWidths();
        this.calcWidthsAndHeights();
        this.setOriginalWidths();
        this.calcMaxWidths();
        this.leadingGap = this.getLeadingGap(propertyPrefix);
        this.calcMaxTextOffset(viewRect);
    }

    private void calcExtraWidths() {
        this.leftTextExtraWidth = this.getLeftExtraWidth(this.text);
    }

    private int getLeftExtraWidth(String str) {
        int lsb = UIUtilities.getLeftSideBearing(this.mi, this.fm, str);
        return lsb < 0 ? -lsb : 0;
    }

    private void setOriginalWidths() {
        this.iconSize.origWidth = this.iconSize.width;
        this.textSize.origWidth = this.textSize.width;
        this.accSize.origWidth = this.accSize.width;
        this.checkSize.origWidth = this.checkSize.width;
        this.arrowSize.origWidth = this.arrowSize.width;
    }

    @SuppressWarnings("deprecation")
    private String getAccText(String acceleratorDelimiter) {
        String accText = "";
        KeyStroke accelerator = this.mi.getAccelerator();
        if (accelerator != null) {
            int modifiers = accelerator.getModifiers();
            if (modifiers > 0) {
                accText = KeyEvent.getKeyModifiersText(modifiers);
                accText = accText + acceleratorDelimiter;
            }

            int keyCode = accelerator.getKeyCode();
            if (keyCode != 0) {
                accText = accText + KeyEvent.getKeyText(keyCode);
            } else {
                accText = accText + accelerator.getKeyChar();
            }
        }

        return accText;
    }

    private Icon getIcon(String propertyPrefix) {
        Icon icon = null;
        MenuItemCheckIconFactory iconFactory = Types.safeCast(
                UIManager.get(propertyPrefix + ".checkIconFactory"), MenuItemCheckIconFactory.class);
        if (!this.isColumnLayout || !this.useCheckAndArrow || iconFactory == null
                || !iconFactory.isCompatible(this.checkIcon, propertyPrefix)) {
            icon = this.mi.getIcon();
        }
        return icon;
    }

    private int getMinTextOffset(String propertyPrefix) {
        int minimumTextOffset = 0;
        Object minimumTextOffsetObject = UIManager.get(propertyPrefix + ".minimumTextOffset");
        if (minimumTextOffsetObject instanceof Integer) {
            minimumTextOffset = (Integer) minimumTextOffsetObject;
        }

        return minimumTextOffset;
    }

    private int getAfterCheckIconGap(String propertyPrefix) {
        int afterCheckIconGap = this.gap;
        Object afterCheckIconGapObject = UIManager.get(propertyPrefix + ".afterCheckIconGap");
        if (afterCheckIconGapObject instanceof Integer) {
            afterCheckIconGap = (Integer) afterCheckIconGapObject;
        }

        return afterCheckIconGap;
    }

    private int getLeadingGap(String propertyPrefix) {
        return this.checkSize.getMaxWidth() > 0 ? this.getCheckOffset(propertyPrefix) : this.gap;
    }

    private int getCheckOffset(String propertyPrefix) {
        int checkIconOffset = this.gap;
        Object checkIconOffsetObject = UIManager.get(propertyPrefix + ".checkIconOffset");
        if (checkIconOffsetObject instanceof Integer) {
            checkIconOffset = (Integer) checkIconOffsetObject;
        }

        return checkIconOffset;
    }

    protected void calcWidthsAndHeights() {
        if (this.icon != null) {
            this.iconSize.width = this.icon.getIconWidth();
            this.iconSize.height = this.icon.getIconHeight();
        }

        if (!this.accText.equals("")) {
            this.accSize.width = UIUtilities.stringWidth(this.mi, this.accFm, this.accText);
            this.accSize.height = this.accFm.getHeight();
        }

        if (this.text == null) {
            this.text = "";
        } else if (!this.text.equals("")) {
            if (this.htmlView != null) {
                this.textSize.width = (int) this.htmlView.getPreferredSpan(0);
                this.textSize.height = (int) this.htmlView.getPreferredSpan(1);
            } else {
                this.textSize.width = UIUtilities.stringWidth(this.mi, this.fm, this.text);
                this.textSize.height = this.fm.getHeight();
            }
        }

        if (this.useCheckAndArrow) {
            if (this.checkIcon != null) {
                this.checkSize.width = this.checkIcon.getIconWidth();
                this.checkSize.height = this.checkIcon.getIconHeight();
            }

            if (this.arrowIcon != null) {
                this.arrowSize.width = this.arrowIcon.getIconWidth();
                this.arrowSize.height = this.arrowIcon.getIconHeight();
            }
        }

        if (this.isColumnLayout) {
            this.labelSize.width = this.iconSize.width + this.textSize.width + this.gap;
            this.labelSize.height = max(this.checkSize.height, this.iconSize.height, this.textSize.height,
                    this.accSize.height, this.arrowSize.height);
        } else {
            Rectangle textRect = new Rectangle();
            Rectangle iconRect = new Rectangle();
            SwingUtilities.layoutCompoundLabel(this.mi, this.fm, this.text, this.icon, this.verticalAlignment,
                    this.horizontalAlignment, this.verticalTextPosition, this.horizontalTextPosition, this.viewRect,
                    iconRect, textRect, this.gap);
            textRect.width += this.leftTextExtraWidth;
            Rectangle labelRect = iconRect.union(textRect);
            this.labelSize.height = labelRect.height;
            this.labelSize.width = labelRect.width;
        }

    }

    protected void calcMaxWidths() {
        this.calcMaxWidth(this.checkSize, MAX_CHECK_WIDTH);
        this.calcMaxWidth(this.arrowSize, MAX_ARROW_WIDTH);
        this.calcMaxWidth(this.accSize, MAX_ACC_WIDTH);
        int curGap;
        if (this.isColumnLayout) {
            this.calcMaxWidth(this.iconSize, MAX_ICON_WIDTH);
            this.calcMaxWidth(this.textSize, MAX_TEXT_WIDTH);
            curGap = this.gap;
            if (this.iconSize.getMaxWidth() == 0 || this.textSize.getMaxWidth() == 0) {
                curGap = 0;
            }

            this.labelSize.maxWidth =
                    this.calcMaxValue(MAX_LABEL_WIDTH, this.iconSize.maxWidth + this.textSize.maxWidth + curGap);
        } else {
            this.iconSize.maxWidth = this.getParentIntProperty(MAX_ICON_WIDTH);
            this.calcMaxWidth(this.labelSize, MAX_LABEL_WIDTH);
            curGap = this.labelSize.maxWidth - this.iconSize.maxWidth;
            if (this.iconSize.maxWidth > 0) {
                curGap -= this.gap;
            }

            this.textSize.maxWidth = this.calcMaxValue(MAX_TEXT_WIDTH, curGap);
        }

    }

    protected void calcMaxWidth(MenuItemLayoutHelper.RectSize rs, Object key) {
        rs.maxWidth = this.calcMaxValue(key, rs.width);
    }

    protected int calcMaxValue(Object propertyName, int value) {
        int maxValue = this.getParentIntProperty(propertyName);
        if (value > maxValue) {
            if (this.miParent != null) {
                this.miParent.putClientProperty(propertyName, value);
            }

            return value;
        } else {
            return maxValue;
        }
    }

    protected int getParentIntProperty(Object propertyName) {
        Object value = null;
        if (this.miParent != null) {
            value = this.miParent.getClientProperty(propertyName);
        }

        if (!(value instanceof Integer)) {
            value = 0;
        }

        return (Integer) value;
    }

    public static boolean isColumnLayout(boolean isLeftToRight, int horizontalAlignment, int horizontalTextPosition,
            int verticalTextPosition) {
        if (verticalTextPosition != 0) {
            return false;
        } else {
            if (isLeftToRight) {
                if (horizontalAlignment != 10 && horizontalAlignment != 2) {
                    return false;
                }

                return horizontalTextPosition == 11 || horizontalTextPosition == 4;
            } else {
                if (horizontalAlignment != 10 && horizontalAlignment != 4) {
                    return false;
                }

                return horizontalTextPosition == 11 || horizontalTextPosition == 2;
            }
        }
    }

    private void calcMaxTextOffset(Rectangle viewRect) {
        if (this.isColumnLayout && this.isLeftToRight) {
            int offset = viewRect.x + this.leadingGap + this.checkSize.maxWidth + this.afterCheckIconGap
                    + this.iconSize.maxWidth + this.gap;
            if (this.checkSize.maxWidth == 0) {
                offset -= this.afterCheckIconGap;
            }

            if (this.iconSize.maxWidth == 0) {
                offset -= this.gap;
            }

            if (offset < this.minTextOffset) {
                offset = this.minTextOffset;
            }

            this.calcMaxValue(UIUtilities.BASICMENUITEMUI_MAX_TEXT_OFFSET, offset);
        }
    }

    public MenuItemLayoutHelper.LayoutResult layoutMenuItem() {
        MenuItemLayoutHelper.LayoutResult lr = this.createLayoutResult();
        this.prepareForLayout(lr);
        if (this.isColumnLayout()) {
            if (this.isLeftToRight()) {
                this.doLTRColumnLayout(lr, this.getLTRColumnAlignment());
            } else {
                this.doRTLColumnLayout(lr, this.getRTLColumnAlignment());
            }
        } else if (this.isLeftToRight()) {
            this.doLTRComplexLayout(lr, this.getLTRColumnAlignment());
        } else {
            this.doRTLComplexLayout(lr, this.getRTLColumnAlignment());
        }

        this.alignAccCheckAndArrowVertically(lr);
        return lr;
    }

    private MenuItemLayoutHelper.LayoutResult createLayoutResult() {
        return new MenuItemLayoutHelper.LayoutResult(new Rectangle(this.iconSize.width, this.iconSize.height),
                new Rectangle(this.textSize.width, this.textSize.height),
                new Rectangle(this.accSize.width, this.accSize.height),
                new Rectangle(this.checkSize.width, this.checkSize.height),
                new Rectangle(this.arrowSize.width, this.arrowSize.height),
                new Rectangle(this.labelSize.width, this.labelSize.height));
    }

    public MenuItemLayoutHelper.ColumnAlignment getLTRColumnAlignment() {
        return MenuItemLayoutHelper.ColumnAlignment.LEFT_ALIGNMENT;
    }

    public MenuItemLayoutHelper.ColumnAlignment getRTLColumnAlignment() {
        return MenuItemLayoutHelper.ColumnAlignment.RIGHT_ALIGNMENT;
    }

    protected void prepareForLayout(MenuItemLayoutHelper.LayoutResult lr) {
        lr.checkRect.width = this.checkSize.maxWidth;
        lr.accRect.width = this.accSize.maxWidth;
        lr.arrowRect.width = this.arrowSize.maxWidth;
    }

    private void alignAccCheckAndArrowVertically(MenuItemLayoutHelper.LayoutResult lr) {
        lr.accRect.y =
                (int) ((float) lr.labelRect.y + (float) lr.labelRect.height / 2.0F - (float) lr.accRect.height / 2.0F);
        this.fixVerticalAlignment(lr, lr.accRect);
        if (this.useCheckAndArrow) {
            lr.arrowRect.y = (int) ((float) lr.labelRect.y + (float) lr.labelRect.height / 2.0F
                    - (float) lr.arrowRect.height / 2.0F);
            lr.checkRect.y = (int) ((float) lr.labelRect.y + (float) lr.labelRect.height / 2.0F
                    - (float) lr.checkRect.height / 2.0F);
            this.fixVerticalAlignment(lr, lr.arrowRect);
            this.fixVerticalAlignment(lr, lr.checkRect);
        }

    }

    private void fixVerticalAlignment(MenuItemLayoutHelper.LayoutResult lr, Rectangle r) {
        int delta = 0;
        if (r.y < this.viewRect.y) {
            delta = this.viewRect.y - r.y;
        } else if (r.y + r.height > this.viewRect.y + this.viewRect.height) {
            delta = this.viewRect.y + this.viewRect.height - r.y - r.height;
        }

        if (delta != 0) {
            Rectangle var10000 = lr.checkRect;
            var10000.y += delta;
            var10000 = lr.iconRect;
            var10000.y += delta;
            var10000 = lr.textRect;
            var10000.y += delta;
            var10000 = lr.accRect;
            var10000.y += delta;
            var10000 = lr.arrowRect;
            var10000.y += delta;
            var10000 = lr.labelRect;
            var10000.y += delta;
        }

    }

    private void doLTRColumnLayout(MenuItemLayoutHelper.LayoutResult lr,
            MenuItemLayoutHelper.ColumnAlignment alignment) {
        lr.iconRect.width = this.iconSize.maxWidth;
        lr.textRect.width = this.textSize.maxWidth;
        this.calcXPositionsLTR(this.viewRect.x, this.leadingGap, this.gap, lr.checkRect, lr.iconRect, lr.textRect);
        Rectangle var10000;
        if (lr.checkRect.width > 0) {
            var10000 = lr.iconRect;
            var10000.x += this.afterCheckIconGap - this.gap;
            var10000 = lr.textRect;
            var10000.x += this.afterCheckIconGap - this.gap;
        }

        this.calcXPositionsRTL(this.viewRect.x + this.viewRect.width, this.leadingGap, this.gap, lr.arrowRect,
                lr.accRect);
        int textOffset = lr.textRect.x - this.viewRect.x;
        if (!this.isTopLevelMenu && textOffset < this.minTextOffset) {
            var10000 = lr.textRect;
            var10000.x += this.minTextOffset - textOffset;
        }

        this.alignRects(lr, alignment);
        this.calcTextAndIconYPositions(lr);
        lr.setLabelRect(lr.textRect.union(lr.iconRect));
    }

    private void doLTRComplexLayout(MenuItemLayoutHelper.LayoutResult lr,
            MenuItemLayoutHelper.ColumnAlignment alignment) {
        lr.labelRect.width = this.labelSize.maxWidth;
        this.calcXPositionsLTR(this.viewRect.x, this.leadingGap, this.gap, lr.checkRect, lr.labelRect);
        Rectangle var10000;
        if (lr.checkRect.width > 0) {
            var10000 = lr.labelRect;
            var10000.x += this.afterCheckIconGap - this.gap;
        }

        this.calcXPositionsRTL(this.viewRect.x + this.viewRect.width, this.leadingGap, this.gap, lr.arrowRect,
                lr.accRect);
        int labelOffset = lr.labelRect.x - this.viewRect.x;
        if (!this.isTopLevelMenu && labelOffset < this.minTextOffset) {
            var10000 = lr.labelRect;
            var10000.x += this.minTextOffset - labelOffset;
        }

        this.alignRects(lr, alignment);
        this.calcLabelYPosition(lr);
        this.layoutIconAndTextInLabelRect(lr);
    }

    private void doRTLColumnLayout(MenuItemLayoutHelper.LayoutResult lr,
            MenuItemLayoutHelper.ColumnAlignment alignment) {
        lr.iconRect.width = this.iconSize.maxWidth;
        lr.textRect.width = this.textSize.maxWidth;
        this.calcXPositionsRTL(this.viewRect.x + this.viewRect.width, this.leadingGap, this.gap, lr.checkRect,
                lr.iconRect, lr.textRect);
        Rectangle var10000;
        if (lr.checkRect.width > 0) {
            var10000 = lr.iconRect;
            var10000.x -= this.afterCheckIconGap - this.gap;
            var10000 = lr.textRect;
            var10000.x -= this.afterCheckIconGap - this.gap;
        }

        this.calcXPositionsLTR(this.viewRect.x, this.leadingGap, this.gap, lr.arrowRect, lr.accRect);
        int textOffset = this.viewRect.x + this.viewRect.width - (lr.textRect.x + lr.textRect.width);
        if (!this.isTopLevelMenu && textOffset < this.minTextOffset) {
            var10000 = lr.textRect;
            var10000.x -= this.minTextOffset - textOffset;
        }

        this.alignRects(lr, alignment);
        this.calcTextAndIconYPositions(lr);
        lr.setLabelRect(lr.textRect.union(lr.iconRect));
    }

    private void doRTLComplexLayout(MenuItemLayoutHelper.LayoutResult lr,
            MenuItemLayoutHelper.ColumnAlignment alignment) {
        lr.labelRect.width = this.labelSize.maxWidth;
        this.calcXPositionsRTL(this.viewRect.x + this.viewRect.width, this.leadingGap, this.gap, lr.checkRect,
                lr.labelRect);
        Rectangle var10000;
        if (lr.checkRect.width > 0) {
            var10000 = lr.labelRect;
            var10000.x -= this.afterCheckIconGap - this.gap;
        }

        this.calcXPositionsLTR(this.viewRect.x, this.leadingGap, this.gap, lr.arrowRect, lr.accRect);
        int labelOffset = this.viewRect.x + this.viewRect.width - (lr.labelRect.x + lr.labelRect.width);
        if (!this.isTopLevelMenu && labelOffset < this.minTextOffset) {
            var10000 = lr.labelRect;
            var10000.x -= this.minTextOffset - labelOffset;
        }

        this.alignRects(lr, alignment);
        this.calcLabelYPosition(lr);
        this.layoutIconAndTextInLabelRect(lr);
    }

    private void alignRects(MenuItemLayoutHelper.LayoutResult lr, MenuItemLayoutHelper.ColumnAlignment alignment) {
        this.alignRect(lr.checkRect, alignment.getCheckAlignment(), this.checkSize.getOrigWidth());
        this.alignRect(lr.iconRect, alignment.getIconAlignment(), this.iconSize.getOrigWidth());
        this.alignRect(lr.textRect, alignment.getTextAlignment(), this.textSize.getOrigWidth());
        this.alignRect(lr.accRect, alignment.getAccAlignment(), this.accSize.getOrigWidth());
        this.alignRect(lr.arrowRect, alignment.getArrowAlignment(), this.arrowSize.getOrigWidth());
    }

    private void alignRect(Rectangle rect, int alignment, int origWidth) {
        if (alignment == 4) {
            rect.x = rect.x + rect.width - origWidth;
        }

        rect.width = origWidth;
    }

    protected void layoutIconAndTextInLabelRect(MenuItemLayoutHelper.LayoutResult lr) {
        lr.setTextRect(new Rectangle());
        lr.setIconRect(new Rectangle());
        SwingUtilities.layoutCompoundLabel(this.mi, this.fm, this.text, this.icon, this.verticalAlignment,
                this.horizontalAlignment, this.verticalTextPosition, this.horizontalTextPosition, lr.labelRect,
                lr.iconRect, lr.textRect, this.gap);
    }

    private void calcXPositionsLTR(int startXPos, int leadingGap, int gap, Rectangle... rects) {
        int curXPos = startXPos + leadingGap;

        for (Rectangle rect : rects) {
            rect.x = curXPos;
            if (rect.width > 0) {
                curXPos += rect.width + gap;
            }
        }

    }

    private void calcXPositionsRTL(int startXPos, int leadingGap, int gap, Rectangle... rects) {
        int curXPos = startXPos - leadingGap;

        for (Rectangle rect : rects) {
            rect.x = curXPos - rect.width;
            if (rect.width > 0) {
                curXPos -= rect.width + gap;
            }
        }

    }

    private void calcTextAndIconYPositions(MenuItemLayoutHelper.LayoutResult lr) {
        if (this.verticalAlignment == 1) {
            lr.textRect.y = (int) ((float) this.viewRect.y + (float) lr.labelRect.height / 2.0F
                    - (float) lr.textRect.height / 2.0F);
            lr.iconRect.y = (int) ((float) this.viewRect.y + (float) lr.labelRect.height / 2.0F
                    - (float) lr.iconRect.height / 2.0F);
        } else if (this.verticalAlignment == 0) {
            lr.textRect.y = (int) ((float) this.viewRect.y + (float) this.viewRect.height / 2.0F
                    - (float) lr.textRect.height / 2.0F);
            lr.iconRect.y = (int) ((float) this.viewRect.y + (float) this.viewRect.height / 2.0F
                    - (float) lr.iconRect.height / 2.0F);
        } else if (this.verticalAlignment == 3) {
            lr.textRect.y = (int) ((float) (this.viewRect.y + this.viewRect.height) - (float) lr.labelRect.height / 2.0F
                    - (float) lr.textRect.height / 2.0F);
            lr.iconRect.y = (int) ((float) (this.viewRect.y + this.viewRect.height) - (float) lr.labelRect.height / 2.0F
                    - (float) lr.iconRect.height / 2.0F);
        }

    }

    private void calcLabelYPosition(MenuItemLayoutHelper.LayoutResult lr) {
        if (this.verticalAlignment == 1) {
            lr.labelRect.y = this.viewRect.y;
        } else if (this.verticalAlignment == 0) {
            lr.labelRect.y = (int) ((float) this.viewRect.y + (float) this.viewRect.height / 2.0F
                    - (float) lr.labelRect.height / 2.0F);
        } else if (this.verticalAlignment == 3) {
            lr.labelRect.y = this.viewRect.y + this.viewRect.height - lr.labelRect.height;
        }

    }

    public static JComponent getMenuItemParent(JMenuItem menuItem) {
        Container parent = menuItem.getParent();
        return !(parent instanceof JComponent) || menuItem instanceof JMenu && ((JMenu) menuItem).isTopLevelMenu()
                ? null
                : (JComponent) parent;
    }

    public static void clearUsedParentClientProperties(JMenuItem menuItem) {
        clearUsedClientProperties(getMenuItemParent(menuItem));
    }

    public static void clearUsedClientProperties(JComponent c) {
        if (c != null) {
            c.putClientProperty(MAX_ARROW_WIDTH, null);
            c.putClientProperty(MAX_CHECK_WIDTH, null);
            c.putClientProperty(MAX_ACC_WIDTH, null);
            c.putClientProperty(MAX_TEXT_WIDTH, null);
            c.putClientProperty(MAX_ICON_WIDTH, null);
            c.putClientProperty(MAX_LABEL_WIDTH, null);
            c.putClientProperty(UIUtilities.BASICMENUITEMUI_MAX_TEXT_OFFSET, null);
        }

    }

    public static int max(int... values) {
        int maxValue = -2147483648;

        for (int i : values) {
            if (i > maxValue) {
                maxValue = i;
            }
        }

        return maxValue;
    }

    public static Rectangle createMaxRect() {
        return new Rectangle(0, 0, 2147483647, 2147483647);
    }

    public static void addMaxWidth(MenuItemLayoutHelper.RectSize size, int gap, Dimension result) {
        if (size.maxWidth > 0) {
            result.width += size.maxWidth + gap;
        }

    }

    public static void addWidth(int width, int gap, Dimension result) {
        if (width > 0) {
            result.width += width + gap;
        }
    }

    public JMenuItem getMenuItem() {
        return this.mi;
    }

    public JComponent getMenuItemParent() {
        return this.miParent;
    }

    public Font getFont() {
        return this.font;
    }

    public Font getAccFont() {
        return this.accFont;
    }

    public FontMetrics getFontMetrics() {
        return this.fm;
    }

    public FontMetrics getAccFontMetrics() {
        return this.accFm;
    }

    public Icon getIcon() {
        return this.icon;
    }

    public Icon getCheckIcon() {
        return this.checkIcon;
    }

    public Icon getArrowIcon() {
        return this.arrowIcon;
    }

    public String getText() {
        return this.text;
    }

    public String getAccText() {
        return this.accText;
    }

    public boolean isColumnLayout() {
        return this.isColumnLayout;
    }

    public boolean useCheckAndArrow() {
        return this.useCheckAndArrow;
    }

    public boolean isLeftToRight() {
        return this.isLeftToRight;
    }

    public boolean isTopLevelMenu() {
        return this.isTopLevelMenu;
    }

    public View getHtmlView() {
        return this.htmlView;
    }

    public int getVerticalAlignment() {
        return this.verticalAlignment;
    }

    public int getHorizontalAlignment() {
        return this.horizontalAlignment;
    }

    public int getVerticalTextPosition() {
        return this.verticalTextPosition;
    }

    public int getHorizontalTextPosition() {
        return this.horizontalTextPosition;
    }

    public int getGap() {
        return this.gap;
    }

    public int getLeadingGap() {
        return this.leadingGap;
    }

    public int getAfterCheckIconGap() {
        return this.afterCheckIconGap;
    }

    public int getMinTextOffset() {
        return this.minTextOffset;
    }

    public Rectangle getViewRect() {
        return this.viewRect;
    }

    public MenuItemLayoutHelper.RectSize getIconSize() {
        return this.iconSize;
    }

    public MenuItemLayoutHelper.RectSize getTextSize() {
        return this.textSize;
    }

    public MenuItemLayoutHelper.RectSize getAccSize() {
        return this.accSize;
    }

    public MenuItemLayoutHelper.RectSize getCheckSize() {
        return this.checkSize;
    }

    public MenuItemLayoutHelper.RectSize getArrowSize() {
        return this.arrowSize;
    }

    public MenuItemLayoutHelper.RectSize getLabelSize() {
        return this.labelSize;
    }

    protected void setMenuItem(JMenuItem mi) {
        this.mi = mi;
    }

    protected void setMenuItemParent(JComponent miParent) {
        this.miParent = miParent;
    }

    protected void setFont(Font font) {
        this.font = font;
    }

    protected void setAccFont(Font accFont) {
        this.accFont = accFont;
    }

    protected void setFontMetrics(FontMetrics fm) {
        this.fm = fm;
    }

    protected void setAccFontMetrics(FontMetrics accFm) {
        this.accFm = accFm;
    }

    protected void setIcon(Icon icon) {
        this.icon = icon;
    }

    protected void setCheckIcon(Icon checkIcon) {
        this.checkIcon = checkIcon;
    }

    protected void setArrowIcon(Icon arrowIcon) {
        this.arrowIcon = arrowIcon;
    }

    protected void setText(String text) {
        this.text = text;
    }

    protected void setAccText(String accText) {
        this.accText = accText;
    }

    protected void setColumnLayout(boolean columnLayout) {
        this.isColumnLayout = columnLayout;
    }

    protected void setUseCheckAndArrow(boolean useCheckAndArrow) {
        this.useCheckAndArrow = useCheckAndArrow;
    }

    protected void setLeftToRight(boolean leftToRight) {
        this.isLeftToRight = leftToRight;
    }

    protected void setTopLevelMenu(boolean topLevelMenu) {
        this.isTopLevelMenu = topLevelMenu;
    }

    protected void setHtmlView(View htmlView) {
        this.htmlView = htmlView;
    }

    protected void setVerticalAlignment(int verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    protected void setHorizontalAlignment(int horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    protected void setVerticalTextPosition(int verticalTextPosition) {
        this.verticalTextPosition = verticalTextPosition;
    }

    protected void setHorizontalTextPosition(int horizontalTextPosition) {
        this.horizontalTextPosition = horizontalTextPosition;
    }

    protected void setGap(int gap) {
        this.gap = gap;
    }

    protected void setLeadingGap(int leadingGap) {
        this.leadingGap = leadingGap;
    }

    protected void setAfterCheckIconGap(int afterCheckIconGap) {
        this.afterCheckIconGap = afterCheckIconGap;
    }

    protected void setMinTextOffset(int minTextOffset) {
        this.minTextOffset = minTextOffset;
    }

    protected void setViewRect(Rectangle viewRect) {
        this.viewRect = viewRect;
    }

    protected void setIconSize(MenuItemLayoutHelper.RectSize iconSize) {
        this.iconSize = iconSize;
    }

    protected void setTextSize(MenuItemLayoutHelper.RectSize textSize) {
        this.textSize = textSize;
    }

    protected void setAccSize(MenuItemLayoutHelper.RectSize accSize) {
        this.accSize = accSize;
    }

    protected void setCheckSize(MenuItemLayoutHelper.RectSize checkSize) {
        this.checkSize = checkSize;
    }

    protected void setArrowSize(MenuItemLayoutHelper.RectSize arrowSize) {
        this.arrowSize = arrowSize;
    }

    protected void setLabelSize(MenuItemLayoutHelper.RectSize labelSize) {
        this.labelSize = labelSize;
    }

    public int getLeftTextExtraWidth() {
        return this.leftTextExtraWidth;
    }

    public static boolean useCheckAndArrow(JMenuItem menuItem) {
        return !(menuItem instanceof JMenu) || !((JMenu) menuItem).isTopLevelMenu();
    }

    public static class RectSize {
        private int width;
        private int height;
        private int origWidth;
        private int maxWidth;

        public RectSize() {}

        public RectSize(int width, int height, int origWidth, int maxWidth) {
            this.width = width;
            this.height = height;
            this.origWidth = origWidth;
            this.maxWidth = maxWidth;
        }

        public int getWidth() {
            return this.width;
        }

        public int getHeight() {
            return this.height;
        }

        public int getOrigWidth() {
            return this.origWidth;
        }

        public int getMaxWidth() {
            return this.maxWidth;
        }

        public void setWidth(int width) {
            this.width = width;
        }

        public void setHeight(int height) {
            this.height = height;
        }

        public void setOrigWidth(int origWidth) {
            this.origWidth = origWidth;
        }

        public void setMaxWidth(int maxWidth) {
            this.maxWidth = maxWidth;
        }

        public String toString() {
            return "[w=" + this.width + ",h=" + this.height + ",ow=" + this.origWidth + ",mw=" + this.maxWidth + "]";
        }
    }

    public static class ColumnAlignment {
        private final int checkAlignment;
        private final int iconAlignment;
        private final int textAlignment;
        private final int accAlignment;
        private final int arrowAlignment;
        public static final MenuItemLayoutHelper.ColumnAlignment LEFT_ALIGNMENT =
                new MenuItemLayoutHelper.ColumnAlignment(2, 2, 2, 2, 2);
        public static final MenuItemLayoutHelper.ColumnAlignment RIGHT_ALIGNMENT =
                new MenuItemLayoutHelper.ColumnAlignment(4, 4, 4, 4, 4);

        public ColumnAlignment(int checkAlignment, int iconAlignment, int textAlignment, int accAlignment,
                int arrowAlignment) {
            this.checkAlignment = checkAlignment;
            this.iconAlignment = iconAlignment;
            this.textAlignment = textAlignment;
            this.accAlignment = accAlignment;
            this.arrowAlignment = arrowAlignment;
        }

        public int getCheckAlignment() {
            return this.checkAlignment;
        }

        public int getIconAlignment() {
            return this.iconAlignment;
        }

        public int getTextAlignment() {
            return this.textAlignment;
        }

        public int getAccAlignment() {
            return this.accAlignment;
        }

        public int getArrowAlignment() {
            return this.arrowAlignment;
        }
    }

    public static class LayoutResult {
        private Rectangle iconRect;
        private Rectangle textRect;
        private Rectangle accRect;
        private Rectangle checkRect;
        private Rectangle arrowRect;
        private Rectangle labelRect;

        public LayoutResult() {
            this.iconRect = new Rectangle();
            this.textRect = new Rectangle();
            this.accRect = new Rectangle();
            this.checkRect = new Rectangle();
            this.arrowRect = new Rectangle();
            this.labelRect = new Rectangle();
        }

        public LayoutResult(Rectangle iconRect, Rectangle textRect, Rectangle accRect, Rectangle checkRect,
                Rectangle arrowRect, Rectangle labelRect) {
            this.iconRect = iconRect;
            this.textRect = textRect;
            this.accRect = accRect;
            this.checkRect = checkRect;
            this.arrowRect = arrowRect;
            this.labelRect = labelRect;
        }

        public Rectangle getIconRect() {
            return this.iconRect;
        }

        public void setIconRect(Rectangle iconRect) {
            this.iconRect = iconRect;
        }

        public Rectangle getTextRect() {
            return this.textRect;
        }

        public void setTextRect(Rectangle textRect) {
            this.textRect = textRect;
        }

        public Rectangle getAccRect() {
            return this.accRect;
        }

        public void setAccRect(Rectangle accRect) {
            this.accRect = accRect;
        }

        public Rectangle getCheckRect() {
            return this.checkRect;
        }

        public void setCheckRect(Rectangle checkRect) {
            this.checkRect = checkRect;
        }

        public Rectangle getArrowRect() {
            return this.arrowRect;
        }

        public void setArrowRect(Rectangle arrowRect) {
            this.arrowRect = arrowRect;
        }

        public Rectangle getLabelRect() {
            return this.labelRect;
        }

        public void setLabelRect(Rectangle labelRect) {
            this.labelRect = labelRect;
        }

        public Map<String, Rectangle> getAllRects() {
            Map<String, Rectangle> result = new HashMap<>();
            result.put("checkRect", this.checkRect);
            result.put("iconRect", this.iconRect);
            result.put("textRect", this.textRect);
            result.put("accRect", this.accRect);
            result.put("arrowRect", this.arrowRect);
            result.put("labelRect", this.labelRect);
            return result;
        }
    }
}
