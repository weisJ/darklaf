/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.ui.checkbox;

import com.github.weisj.darklaf.decorators.MouseClickListener;
import com.github.weisj.darklaf.ui.menu.DarkMenuItemUIBase;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.MenuItemLayoutHelper;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkCheckBoxMenuItemUI extends DarkMenuItemUIBase {

    private final MouseClickListener clickListener = e -> SwingUtilities.invokeLater(() -> menuItem.setArmed(true));

    private Icon checkBoxIcon;
    private Icon checkBoxDisabledIcon;
    private Icon checkBoxFocusedIcon;
    private Icon checkBoxSelectedIcon;
    private Icon checkBoxSelectedDisabledIcon;
    private Icon checkBoxSelectedFocusedIcon;


    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkCheckBoxMenuItemUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        c.putClientProperty("CheckBoxMenuItem.doNotCloseOnMouseClick", Boolean.TRUE);
    }

    protected void paintCheckIcon(final Graphics g2, @NotNull final MenuItemLayoutHelper lh,
                                  @NotNull final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color holdc, final Color foreground) {
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        var rect = lr.getCheckRect();
        getCheckBoxIcon(lh.getMenuItem()).paintIcon(lh.getMenuItem(), g2, rect.x - 1, rect.y);
        config.restore();
        g.setColor(foreground);
    }

    protected Icon getCheckBoxIcon(@NotNull final AbstractButton b) {
        boolean selected = b.isSelected();
        boolean enabled = b.isEnabled();
        boolean hasFocus = b.hasFocus();
        return selected ? enabled ? hasFocus ? checkBoxSelectedFocusedIcon
                                             : checkBoxSelectedIcon
                                  : checkBoxSelectedDisabledIcon
                        : enabled ? hasFocus ? checkBoxFocusedIcon
                                             : checkBoxIcon
                                  : checkBoxDisabledIcon;
    }

    public void installDefaults() {
        super.installDefaults();
        checkBoxIcon = UIManager.getIcon("CheckBox.unchecked.icon");
        checkBoxDisabledIcon = UIManager.getIcon("CheckBox.uncheckedDisabled.icon");
        checkBoxFocusedIcon = UIManager.getIcon("CheckBox.uncheckedFocused.icon");
        checkBoxSelectedIcon = UIManager.getIcon("CheckBox.selected.icon");
        checkBoxSelectedDisabledIcon = UIManager.getIcon("CheckBox.selectedDisabled.icon");
        checkBoxSelectedFocusedIcon = UIManager.getIcon("CheckBox.selectedFocused.icon");
    }

    protected String getPropertyPrefix() {
        return "CheckBoxMenuItem";
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        menuItem.addMouseListener(clickListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        menuItem.removeMouseListener(clickListener);
    }
}
