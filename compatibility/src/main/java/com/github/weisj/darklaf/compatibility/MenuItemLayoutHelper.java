/*
 * MIT License
 *
 * Copyright (c) 2021-2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.compatibility;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.text.View;

import com.intellij.util.ui.StringUIClientPropertyKey;


public interface MenuItemLayoutHelper {

    StringUIClientPropertyKey MAX_ARROW_WIDTH = new StringUIClientPropertyKey("maxArrowWidth");
    StringUIClientPropertyKey MAX_CHECK_WIDTH = new StringUIClientPropertyKey("maxCheckWidth");
    StringUIClientPropertyKey MAX_ICON_WIDTH = new StringUIClientPropertyKey("maxIconWidth");
    StringUIClientPropertyKey MAX_NON_CHECK_ICON_WIDTH = new StringUIClientPropertyKey("maxNonCheckIconWidth");
    StringUIClientPropertyKey MAX_TEXT_WIDTH = new StringUIClientPropertyKey("maxTextWidth");
    StringUIClientPropertyKey MAX_ACC_WIDTH = new StringUIClientPropertyKey("maxAccWidth");
    StringUIClientPropertyKey MAX_LABEL_WIDTH = new StringUIClientPropertyKey("maxLabelWidth");

    static MenuItemLayoutHelper create(final JMenuItem mi, final Icon checkIcon, final Icon arrowIcon,
            final Rectangle viewRect, final int defaultTextIconGap,
            final String acceleratorDelimiter, final boolean leftToRight,
            final Font font, final Font acceleratorFont, final String propertyPrefix) {
        boolean useCheckAndArrow = com.intellij.util.ui.MenuItemLayoutHelper.useCheckAndArrow(mi);
        return new com.intellij.util.ui.MenuItemLayoutHelper(mi, checkIcon, arrowIcon, viewRect, defaultTextIconGap,
                acceleratorDelimiter, leftToRight, font,
                acceleratorFont, useCheckAndArrow, propertyPrefix);
    }

    static void uninstall(final JMenuItem mi) {
        com.intellij.util.ui.MenuItemLayoutHelper.clearUsedParentClientProperties(mi);
    }

    MILayoutResult layoutMenuItem();

    JMenuItem getMenuItem();

    FontMetrics getFontMetrics();

    FontMetrics getAccFontMetrics();

    Icon getIcon();

    Icon getCheckIcon();

    Icon getArrowIcon();

    String getText();

    String getAccText();

    boolean useCheckAndArrow();

    boolean isTopLevelMenu();

    View getHtmlView();

    int getGap();

    int getLeadingGap();

    int getAfterCheckIconGap();

    int getMinTextOffset();

    Rectangle getViewRect();

    MIRectSize getIconSize();

    MIRectSize getTextSize();

    MIRectSize getAccSize();

    MIRectSize getCheckSize();

    MIRectSize getArrowSize();

    MIRectSize getLabelSize();

    static Rectangle createMaxRect() {
        return new Rectangle(0, 0, 2147483647, 2147483647);
    }

    static void addMaxWidth(final MIRectSize size, final int gap, final Dimension result) {
        if (size.getMaxWidth() > 0) {
            result.width += size.getMaxWidth() + gap;
        }
    }

    interface MILayoutResult {

        Rectangle getCheckRect();

        Rectangle getIconRect();

        Rectangle getTextRect();

        Rectangle getAccRect();

        Rectangle getArrowRect();
    }

    interface MIRectSize {

        int getWidth();

        int getHeight();

        int getOrigWidth();

        int getMaxWidth();

        void setWidth(final int width);

        void setHeight(final int height);
    }

    interface CheckBoxMarker {
    }
}
