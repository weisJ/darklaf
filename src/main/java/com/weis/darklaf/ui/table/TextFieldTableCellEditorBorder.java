package com.weis.darklaf.ui.table;

import com.weis.darklaf.ui.text.DarkTextFieldUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class TextFieldTableCellEditorBorder extends DarkTableCellBorder {

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g, final int x, final int y,
                            final int width, final int height) {
        g.setColor(DarkTextFieldUI.getBorderColor(c));
        var parent = c.getParent();
        if (parent instanceof JTable) {
            var table = ((JTable) parent);
            if (!table.getShowHorizontalLines()) {
                g.fillRect(0, 0, width, 1);
                g.fillRect(0, height - 1, width, 1);
            }
            if (!table.getShowVerticalLines()) {
                g.fillRect(0, 0, 1, height);
                g.fillRect(width - 1, 0, 1, height);
            }
        }
    }
}
