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
package com.github.weisj.darklaf.components;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class CloseButton extends JButton {

    public CloseButton() {
        putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
        putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS_RECTANGULAR);
        putClientProperty(DarkButtonUI.KEY_THIN, true);
        putClientProperty(DarkButtonUI.KEY_SQUARE, true);
        setBorder(BorderFactory.createEmptyBorder());
        setMargin(new Insets(0, 0, 0, 0));
        setOpaque(false);
        setRolloverEnabled(true);
        setBorderPainted(false);
        setContentAreaFilled(false);

        Icon icon = UIManager.getIcon("CloseButton.closeIcon");
        if (icon == null) icon = DarkUIUtil.ICON_LOADER.getIcon("navigation/close.svg", true);
        setIcon(icon);

        icon = UIManager.getIcon("CloseButton.closeDisabledIcon");
        if (icon == null) icon = DarkUIUtil.ICON_LOADER.getIcon("navigation/closeDisabled.svg", true);
        setDisabledIcon(icon);

        icon = UIManager.getIcon("CloseButton.closeHoverIcon");
        if (icon == null) icon = DarkUIUtil.ICON_LOADER.getIcon("navigation/closeHovered.svg", true);
        setRolloverIcon(icon);
    }
}
