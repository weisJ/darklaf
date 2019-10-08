package com.weis.darklaf.ui.radiobutton;

import com.weis.darklaf.decorators.MouseClickListener;
import com.weis.darklaf.ui.menu.DarkMenuItemUIBase;
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
public class DarkRadioButtonMenuItemUI extends DarkMenuItemUIBase {

    private final MouseClickListener clickListener = e -> SwingUtilities.invokeLater(() -> menuItem.setArmed(true));


    private Icon radioIcon;
    private Icon radioDisabledIcon;
    private Icon radioFocusedIcon;
    private Icon radioSelectedIcon;
    private Icon radioSelectedDisabledIcon;
    private Icon radioSelectedFocusedIcon;

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkRadioButtonMenuItemUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        acceleratorFont = UIManager.getFont("MenuItem.font");
        acceleratorForeground = UIManager.getColor("MenuItem.foreground");
        acceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
        c.putClientProperty("RadioButtonMenuItem.doNotCloseOnMouseClick", Boolean.TRUE);
    }

    public void installDefaults() {
        super.installDefaults();
        radioIcon = UIManager.getIcon("RadioButton.unchecked.icon");
        radioDisabledIcon = UIManager.getIcon("RadioButton.uncheckedDisabled.icon");
        radioFocusedIcon = UIManager.getIcon("RadioButton.uncheckedFocused.icon");
        radioSelectedIcon = UIManager.getIcon("RadioButton.selected.icon");
        radioSelectedDisabledIcon = UIManager.getIcon("RadioButton.selectedDisabled.icon");
        radioSelectedFocusedIcon = UIManager.getIcon("RadioButton.selectedFocused.icon");
    }

    @Override
    protected void paintCheckIcon(final Graphics g2, @NotNull final MenuItemLayoutHelper lh,
                                  @NotNull final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color holdc, final Color foreground) {
        var rect = lr.getCheckRect();
        getRadioIcon(lh.getMenuItem()).paintIcon(lh.getMenuItem(), g2, rect.x, rect.y);
    }

    protected Icon getRadioIcon(@NotNull final AbstractButton b) {
        boolean selected = b.isSelected();
        boolean enabled = b.isEnabled();
        boolean hasFocus = b.hasFocus();
        return selected ? enabled ? hasFocus ? radioSelectedFocusedIcon
                                             : radioSelectedIcon
                                  : radioSelectedDisabledIcon
                        : enabled ? hasFocus ? radioFocusedIcon
                                             : radioIcon
                                  : radioDisabledIcon;
    }

    protected String getPropertyPrefix() {
        return "RadioButtonMenuItem";
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
