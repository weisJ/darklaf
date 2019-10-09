package com.weis.darklaf.ui.checkbox;

import com.weis.darklaf.decorators.MouseClickListener;
import com.weis.darklaf.ui.menu.DarkMenuItemUIBase;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
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

    public void installDefaults() {
        super.installDefaults();
        checkBoxIcon = UIManager.getIcon("CheckBox.unchecked.icon");
        checkBoxDisabledIcon = UIManager.getIcon("CheckBox.uncheckedDisabled.icon");
        checkBoxFocusedIcon = UIManager.getIcon("CheckBox.uncheckedFocused.icon");
        checkBoxSelectedIcon = UIManager.getIcon("CheckBox.selected.icon");
        checkBoxSelectedDisabledIcon = UIManager.getIcon("CheckBox.selectedDisabled.icon");
        checkBoxSelectedFocusedIcon = UIManager.getIcon("CheckBox.selectedFocused.icon");
    }

    protected void paintCheckIcon(final Graphics g2, @NotNull final MenuItemLayoutHelper lh,
                                  @NotNull final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color holdc, final Color foreground) {
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        var rect = lr.getCheckRect();
        getCheckBoxIcon(lh.getMenuItem()).paintIcon(lh.getMenuItem(), g2, rect.x, rect.y);
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
