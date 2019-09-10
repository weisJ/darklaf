package com.weis.darklaf.ui;

import com.bulenkov.darcula.ui.DarculaTreeUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

/**
 * Adaption of DarculaTreeUI to allow a Tree to not be skinny.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class DarkTreeUI extends DarculaTreeUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTreeUI();
    }

    @Override
    public int getRightChildIndent() {
        return isSkinny() ? 8 : rightChildIndent;
    }

    @Override
    protected int getRowX(final int row, final int depth) {
        return isSkinny() ? 8 * depth + 8 : totalChildIndent * (depth + depthOffset);
    }

    private boolean isSkinny() {
        return tree != null && !Boolean.FALSE.equals(tree.getClientProperty("skinny"));
    }
}
