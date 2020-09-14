/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.util.PropertyUtil;

public interface ButtonConstants {
    String KEY_VARIANT = "JButton.variant";
    String KEY_HOVER_COLOR = "JButton.borderless.hover";
    String KEY_CLICK_COLOR = "JButton.borderless.click";
    String KEY_ALT_ARC = "JButton.alternativeArc";
    String KEY_NO_ARC = "JButton.noArc";
    String KEY_SQUARE = "JButton.square";
    String KEY_THIN = "JButton.thin";
    String KEY_NO_BORDERLESS_OVERWRITE = "JButton.noBorderlessOverwrite";
    String KEY_CORNER = "JButton.cornerFlag";
    String KEY_ROUND = "JButton.round";

    String KEY_LEFT_NEIGHBOUR = "JButton.leftNeighbour";
    String KEY_RIGHT_NEIGHBOUR = "JButton.rightNeighbour";
    String KEY_TOP_NEIGHBOUR = "JButton.topNeighbour";
    String KEY_TOP_LEFT_NEIGHBOUR = "JButton.topLeftNeighbour";
    String KEY_TOP_RIGHT_NEIGHBOUR = "JButton.topRightNeighbour";
    String KEY_BOTTOM_NEIGHBOUR = "JButton.bottomNeighbour";
    String KEY_BOTTOM_LEFT_NEIGHBOUR = "JButton.bottomLeftNeighbour";
    String KEY_BOTTOM_RIGHT_NEIGHBOUR = "JButton.bottomRightNeighbour";

    String VARIANT_BORDERLESS_RECTANGULAR = "borderlessRectangular";
    String VARIANT_BORDERLESS = "borderless";
    String VARIANT_NONE = "none";

    static boolean chooseAlternativeArc(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_ALT_ARC);
    }

    static int chooseArcWithBorder(final Component c, final int arc, final int minimum, final int altArc,
            final int borderSize) {
        return chooseArc(c, arc, minimum, altArc, c.getHeight() - 2 * borderSize);
    }

    static int chooseArc(final Component c, final int arc, final int minimum, final int altArc, final int roundedArc) {
        if (ButtonConstants.isNoArc(c)) return minimum;
        if (ButtonConstants.isRound(c)) return roundedArc;
        boolean alt = ButtonConstants.chooseAlternativeArc(c);
        return alt ? altArc : arc;
    }

    static boolean isNoArc(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_NO_ARC);
    }

    static boolean isSquare(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_SQUARE);
    }

    static boolean isRound(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_ROUND);
    }

    static boolean isThin(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_THIN)
                || (c instanceof AbstractButton && ButtonConstants.doConvertToBorderless((AbstractButton) c));
    }

    static boolean isBorderlessVariant(final Component c) {
        if (isBorderlessRectangular(c)) return true;
        if (c instanceof JButton || c instanceof JToggleButton) {
            return isBorderless(c) || doConvertToBorderless((AbstractButton) c);
        }
        return false;
    }

    static boolean isBorderless(final Component c) {
        return PropertyUtil.isPropertyEqual(c, KEY_VARIANT, VARIANT_BORDERLESS);
    }

    static boolean isBorderlessRectangular(final Component c) {
        return PropertyUtil.isPropertyEqual(c, KEY_VARIANT, VARIANT_BORDERLESS_RECTANGULAR);
    }

    static boolean doConvertToBorderless(final AbstractButton b) {
        return isIconOnly(b) && !b.isFocusable() && convertIconButtonToBorderless(b)
                && (b instanceof JButton || b instanceof JToggleButton);
    }

    static boolean convertIconButtonToBorderless(final AbstractButton b) {
        return !(b instanceof UIResource) && UIManager.getBoolean("Button.convertIconOnlyToBorderless")
                && !PropertyUtil.getBooleanProperty(b, KEY_NO_BORDERLESS_OVERWRITE);
    }

    static boolean isIconOnly(final AbstractButton b) {
        return b.getIcon() != null && (b.getText() == null || b.getText().isEmpty());
    }

    static boolean isDefaultButton(final JComponent c) {
        return c instanceof JButton && ((JButton) c).isDefaultButton();
    }

    static JComponent getNeighbour(final String key, final Component comp) {
        return PropertyUtil.getObject(comp, key, JComponent.class);
    }
}
