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
 * @author Jannis Weis
 * @since 2019
 */
public class DarkCheckBoxMenuItemUI extends DarkMenuItemUIBase {

    private final MouseClickListener clickListener = e -> SwingUtilities.invokeLater(() -> menuItem.setArmed(true));

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

        g.translate(lr.getCheckRect().x, lr.getCheckRect().y + 2);

        DarkCheckBoxUI.paintCheckBorder(g, lh.getMenuItem().isEnabled(), lh.getMenuItem().hasFocus());
        if (lh.getMenuItem().isSelected()) {
            DarkCheckBoxUI.paintCheckArrow(g, lh.getMenuItem().isEnabled());
        }

        g.translate(-lr.getCheckRect().x, -lr.getCheckRect().y - 2);
        config.restore();
        g.setColor(foreground);
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
