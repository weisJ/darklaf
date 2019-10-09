package com.weis.darklaf.ui.tristate;

import com.weis.darklaf.components.tristate.TristateCheckBox;
import com.weis.darklaf.components.tristate.TristateState;
import com.weis.darklaf.ui.checkbox.DarkCheckBoxUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

/**
 * @author Jannis Weis
 */
public class DarkTristateCheckBoxUI extends DarkCheckBoxUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTristateCheckBoxUI();
    }

    private Icon checkBoxIndeterminateIcon;
    private Icon checkBoxIndeterminateDisabledIcon;
    private Icon checkBoxIndeterminateFocusedIcon;

    @Override
    public void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        checkBoxIndeterminateIcon = UIManager.getIcon("CheckBox.indeterminate.icon");
        checkBoxIndeterminateDisabledIcon = UIManager.getIcon("CheckBox.indeterminateDisabled.icon");
        checkBoxIndeterminateFocusedIcon = UIManager.getIcon("CheckBox.indeterminateFocused.icon");
    }

    @Override
    protected Icon getCheckIcon(@NotNull final AbstractButton b) {
        if (b instanceof TristateCheckBox) {
            var state = ((TristateCheckBox) b).getState();
            if (state == TristateState.INDETERMINATE) {
                if (b.isEnabled()) {
                    return b.hasFocus() ? checkBoxIndeterminateFocusedIcon : checkBoxIndeterminateIcon;
                } else {
                    return checkBoxIndeterminateDisabledIcon;
                }
            }
        }
        return super.getCheckIcon(b);
    }
}
